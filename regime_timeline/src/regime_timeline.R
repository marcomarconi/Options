# Vol-regime classifier replicating the Sharpe Two "Regime Timeline" charts.
# R port of regime_model.py + plot_timeline.py (see README.md).
#
# Pipeline: ORATS features -> diagonal-covariance GMM (mclust, VVI, k=7)
# pooled across a 15-ETF basket -> clusters auto-labeled from centroids ->
# per-day Extreme Stress override -> timeline scatter plots.
#
# Usage:
#   Rscript regime_timeline.R                # fit 30dte + replication/sanity figures
#   Rscript regime_timeline.R TLT            # + single-ticker figure, last 12 months

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(slider)
  library(mclust)
  library(ggplot2)
})

ORATS_CORE <- "/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq"
HERE    <- normalizePath(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
RESULTS <- file.path(HERE, "..", "results")
FIGURES <- file.path(HERE, "..", "figures")

BASKET <- c("SPY", "QQQ", "IWM", "DIA", "GLD", "TLT", "USO",
            "XLE", "EEM", "FXI", "SLV", "HYG", "EFA", "XLF", "SMH")

FEATS <- c("iv_pct", "rv_pct", "vrp", "iv_off_peak", "iv_mom", "stress_recency")

TENORS <- list("30dte" = c(iv = "iv30d", rv = "orHv20d"),
               "9dte"  = c(iv = "iv10d", rv = "orHv5d"))

# ------------------------------------------------------------------ features
roll_pct <- function(x, win = 504, minp = 252) {
  # trailing percentile rank of the last value (pandas rank(pct=TRUE) analog)
  slide_dbl(x, ~ if (sum(!is.na(.x)) < minp) NA_real_
            else mean(.x <= .x[length(.x)], na.rm = TRUE),
            .before = win - 1)
}

roll_max <- function(x, win, minp) {
  slide_dbl(x, ~ if (sum(!is.na(.x)) < minp) NA_real_ else max(.x, na.rm = TRUE),
            .before = win - 1)
}

load_features <- function(tenor = "30dte", tickers = BASKET) {
  iv_col <- TENORS[[tenor]]["iv"]; rv_col <- TENORS[[tenor]]["rv"]
  df <- read_parquet(ORATS_CORE,
                     col_select = all_of(unique(c("ticker", "tradeDate", iv_col, rv_col)))) |>
    filter(ticker %in% tickers) |>
    arrange(ticker, tradeDate)

  df |>
    group_by(ticker) |>
    mutate(
      iv  = .data[[iv_col]],
      liv = log(ifelse(.data[[iv_col]] <= 0, NA, .data[[iv_col]])),
      lrv = log(ifelse(.data[[rv_col]] <= 0, NA, .data[[rv_col]])),
      vrp = liv - lrv,
      # trailing-2y percentile ranks: comparable across tickers
      iv_pct = roll_pct(liv),
      rv_pct = roll_pct(lrv),
      # distance from the trailing 1m IV peak (very negative = vol crush)
      iv_off_peak = liv - roll_max(liv, 21, 10),
      iv_mom = liv - lag(liv, 10),
      # was there a stress episode in the last 3 months?
      stress_recency = roll_max(iv_pct, 63, 21)
    ) |>
    ungroup() |>
    filter(if_all(all_of(FEATS), ~ !is.na(.x)))
}

# ------------------------------------------------------------------ labeling
label_centroids <- function(cent) {
  apply(cent, 1, function(r) {
    if      (r["iv_off_peak"] < -1.0)                                "Vol Crush"
    else if (r["iv_pct"] > 0.85 && r["rv_pct"] > 0.85)               "High Stress"
    else if (r["iv_pct"] > 0.5  && (r["vrp"] < 0.05 || r["iv_mom"] < -0.02)) "Post Stress"
    else if (r["iv_pct"] > 0.6)                                      "Stress"
    else if (r["iv_pct"] < 0.15 && r["rv_pct"] < 0.15)               "Low RV"
    else if (r["vrp"] > 0.15)                                        "Positive VRP"
    else                                                             "Calm"
  })
}

fit_regimes <- function(tenor = "30dte", k = 7, verbose = TRUE) {
  d <- load_features(tenor)
  X <- as.matrix(d[FEATS])
  Z <- scale(X)
  set.seed(42)
  # hierarchical init does not scale to ~40k rows -> init on a random subset
  gmm <- Mclust(Z, G = k, modelNames = "VVI", verbose = FALSE,
                initialization = list(subset = sample(nrow(Z), 5000)))
  d$cluster <- gmm$classification
  # centroids back in original feature units
  cent <- t(gmm$parameters$mean) * attr(Z, "scaled:scale")[col(t(gmm$parameters$mean))] +
          attr(Z, "scaled:center")[col(t(gmm$parameters$mean))]
  colnames(cent) <- FEATS
  labels <- label_centroids(cent)
  d$regime <- labels[d$cluster]
  # day-level refinement: reserve Extreme Stress for genuine tail days
  extreme <- d$regime %in% c("Stress", "High Stress", "Post Stress") &
    ((d$iv_pct > 0.99 & d$rv_pct > 0.80) | (d$iv_pct > 0.97 & d$rv_pct > 0.90))
  d$regime[extreme] <- "Extreme Stress"
  if (verbose) {
    print(round(cbind(as.data.frame(cent),
                      `share%` = round(100 * gmm$parameters$pro, 1)), 3))
    print(data.frame(label = labels))
    print(d |> count(regime) |> mutate(median_iv = tapply(d$iv, d$regime, median)[regime]))
  }
  list(d = d, gmm = gmm, labels = labels)
}

# ------------------------------------------------------------------ plotting
COLORS <- c("Low RV"       = "#008300",
            "Positive VRP" = "#4a3aa7",
            "Calm"         = "#2a78d6",
            "Post Stress"  = "#eda100",
            "Stress"       = "#eb6834",
            "High Stress"  = "#9c5f2e",
            "Extreme Stress" = "#e34948",
            "Vol Crush"    = "#e87ba4")

plot_timeline <- function(d, tkr, t0 = NULL, t1 = NULL) {
  g <- d |> filter(ticker == tkr)
  if (!is.null(t0)) g <- filter(g, tradeDate >= as.Date(t0))
  if (!is.null(t1)) g <- filter(g, tradeDate <= as.Date(t1))
  g$regime <- factor(g$regime, levels = names(COLORS))
  ggplot(g, aes(tradeDate, iv, color = regime)) +
    geom_point(size = 1.6) +
    scale_color_manual(values = COLORS, drop = TRUE, name = NULL) +
    scale_y_continuous(limits = c(0, NA), n.breaks = 4) +
    labs(title = tkr, x = NULL, y = "IMPLIED VOLATILITY") +
    theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11, color = "#0b0b0b"),
      plot.background = element_rect(fill = "#fcfcfb", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#e1e0d9", linetype = "dashed"),
      axis.text = element_text(color = "#898781"),
      axis.title.y = element_text(color = "#898781", size = 7),
      axis.line.x = element_line(color = "#c3c2b7"),
      legend.position = "bottom",
      legend.text = element_text(color = "#52514e", size = 8)
    ) +
    guides(color = guide_legend(nrow = 1, override.aes = list(size = 2.5)))
}

save_panel <- function(plots, fname, height = 8) {
  dir.create(FIGURES, showWarnings = FALSE, recursive = TRUE)
  # one legend for the whole panel: keep it only on the bottom plot
  for (i in seq_len(length(plots) - 1))
    plots[[i]] <- plots[[i]] + theme(legend.position = "none")
  p <- Reduce(`/`, plots)                       # needs patchwork
  ggsave(file.path(FIGURES, fname), p, width = 9, height = height,
         dpi = 120, bg = "#fcfcfb")
  cat("saved", file.path(FIGURES, fname), "\n")
}

# ------------------------------------------------------------------ main
if (sys.nframe() == 0L) {
  fit <- fit_regimes("30dte")
  d <- fit$d
  dir.create(RESULTS, showWarnings = FALSE, recursive = TRUE)
  write_parquet(d, file.path(RESULTS, "regimes_30dte_r.pq"))

  # scoring bundle for external consumers (e.g. the Shiny app):
  # stripped Mclust object (predict.Mclust only needs parameters/modelName)
  # + the feature scaler + cluster label map
  gmm_slim <- fit$gmm
  gmm_slim$data <- gmm_slim$data[1, , drop = FALSE]  # predict.Mclust wants ncol(data)
  gmm_slim$z <- gmm_slim$classification <- gmm_slim$uncertainty <- NULL
  Z <- scale(as.matrix(d[FEATS]))   # recompute attrs identical to fit
  saveRDS(list(gmm = gmm_slim,
               center = attr(Z, "scaled:center"),
               scale  = attr(Z, "scaled:scale"),
               labels = fit$labels, feats = FEATS,
               tenor = "30dte", iv_col = "iv30d", rv_col = "orHv20d",
               fitted = Sys.Date()),
          file.path(RESULTS, "regime_gmm_30dte.rds"))
  cat("saved", file.path(RESULTS, "regime_gmm_30dte.rds"), "\n")

  has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
  if (has_patchwork) {
    library(patchwork)
    save_panel(list(plot_timeline(d, "QQQ", "2025-06-19", "2026-06-26"),
                    plot_timeline(d, "USO", "2025-07-25", "2026-03-13")),
               "replication_qqq_uso_r.png")
    save_panel(list(plot_timeline(d, "SPY", "2017-01-01", "2019-01-31"),
                    plot_timeline(d, "SPY", "2019-06-01", "2021-06-30")),
               "sanity_spy_history_r.png")
  } else {
    for (nm in list(c("QQQ", "2025-06-19", "2026-06-26"),
                    c("USO", "2025-07-25", "2026-03-13"))) {
      p <- plot_timeline(d, nm[1], nm[2], nm[3])
      ggsave(file.path(FIGURES, paste0("timeline_", nm[1], "_r.png")), p,
             width = 9, height = 4.5, dpi = 120, bg = "#fcfcfb")
    }
  }

  args <- commandArgs(TRUE)
  if (length(args)) {
    tkr <- toupper(args[1])
    t1 <- max(d$tradeDate)
    p <- plot_timeline(d, tkr, t1 - 365, t1)
    ggsave(file.path(FIGURES, paste0("timeline_", tkr, "_r.png")), p,
           width = 9, height = 4.5, dpi = 120, bg = "#fcfcfb")
    cat("saved", file.path(FIGURES, paste0("timeline_", tkr, "_r.png")), "\n")
  }
}
