# Regime Timeline scoring module for the Options Shiny app.
#
# Scores ANY ticker with the GMM fitted by regime_timeline.R (bundle saved in
# results/regime_gmm_30dte.rds) against the full-history research parquet,
# and returns a Sharpe-Two-style timeline ggplot (pipe through ggplotly()).
#
# Self-contained on purpose: source() this file, then call
#   regime_timeline_plotly("NVDA")                  # plotly, for the app
#   regime_timeline_plot("NVDA", months = 24)       # ggplot, for PNGs
# (the app's ggplot2 4.x breaks ggplotly(), hence the native plot_ly builder)
#
# Feature definitions must stay in sync with regime_timeline.R (source of
# truth for the fit). Caveat: IVs here are NOT earnings-adjusted, so single
# names can print Stress into earnings weeks.

REGIME_RDS   <- "/home/marco/trading/Systems/Options/regime_timeline/results/regime_gmm_30dte.rds"
# Full-history warehouse (1.9GB, all 54 cols) — used ONLY to seed the slim
# store below the first time. Nothing keeps it fresh, so do not read it live.
REGIME_ORATS <- "/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq"
# Slim, self-refreshing regime store: just the 4 columns the plot needs, full
# history from 2015. regime_iv_update() seeds it once (from the warehouse if
# present, else the raw dailies) then appends only new daily files on each app
# start — seconds, not the minutes a full warehouse rebuild costs.
REGIME_IV       <- "/home/marco/trading/Systems/Options/Data/regime_iv.pq"
REGIME_CORE_DIR <- "/home/marco/trading/HistoricalData/ORATS/core/"
REGIME_IV_COLS  <- c("ticker", "tradeDate", "iv30d", "orHv20d")

REGIME_COLORS <- c("Low RV"       = "#008300",
                   "Positive VRP" = "#4a3aa7",
                   "Calm"         = "#2a78d6",
                   "Post Stress"  = "#eda100",
                   "Stress"       = "#eb6834",
                   "High Stress"  = "#9c5f2e",
                   "Extreme Stress" = "#e34948",
                   "Vol Crush"    = "#e87ba4")

.regime_model <- NULL
regime_model <- function() {
  if (is.null(.regime_model))
    .regime_model <<- readRDS(REGIME_RDS)
  .regime_model
}

# --- slim regime store: seed once, then incremental daily append ------------

# raw daily core files, sorted by their embedded date
.regime_iv_files <- function() {
  f <- list.files(REGIME_CORE_DIR, "orats_core_20[0-9]{2}[0-9]{4}\\.csv\\.gz$")
  f[order(as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1", f), "%Y%m%d"))]
}

# read only the 4 regime columns out of a set of daily files
.regime_iv_read_days <- function(files) {
  purrr::map_dfr(files, function(fn) {
    dt   <- data.table::fread(file.path(REGIME_CORE_DIR, fn), showProgress = FALSE)
    keep <- intersect(REGIME_IV_COLS, names(dt))
    out  <- as.data.frame(dt[, ..keep])
    out$tradeDate <- as.Date(out$tradeDate)
    out
  })
}

# Ensure REGIME_IV exists and is current. Safe to call on every app start:
# builds once if missing, otherwise appends only daily files newer than the
# last stored day. Wrapped by callers in tryCatch so a data hiccup never
# blocks startup. Returns invisibly; side effect is the refreshed parquet.
regime_iv_update <- function(verbose = TRUE) {
  say <- function(...) if (verbose) message("[regime_iv] ", ...)
  if (!file.exists(REGIME_IV)) {
    if (file.exists(REGIME_ORATS)) {
      say("seeding from warehouse ", REGIME_ORATS)
      g <- arrow::open_dataset(REGIME_ORATS) |>
        dplyr::select(dplyr::all_of(REGIME_IV_COLS)) |>
        dplyr::collect()
      g$tradeDate <- as.Date(g$tradeDate)
    } else {
      say("no warehouse; seeding from raw dailies (slow, one-time)")
      g <- .regime_iv_read_days(.regime_iv_files())
    }
    arrow::write_parquet(g, REGIME_IV)
  }
  g <- arrow::read_parquet(REGIME_IV)
  g$tradeDate <- as.Date(g$tradeDate)
  last_day <- suppressWarnings(max(g$tradeDate, na.rm = TRUE))
  files  <- .regime_iv_files()
  fdates <- as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1", files), "%Y%m%d")
  new    <- files[!is.na(fdates) & fdates > last_day]
  if (length(new)) {
    say("appending ", length(new), " new day(s): ",
        format(last_day), " -> ", format(max(fdates, na.rm = TRUE)))
    g <- dplyr::bind_rows(g, .regime_iv_read_days(new))
    arrow::write_parquet(g, REGIME_IV)
  } else {
    say("already current at ", format(last_day))
  }
  invisible(g)
}

.regime_roll_pct <- function(x, win = 504, minp = 252) {
  slider::slide_dbl(x, ~ if (sum(!is.na(.x)) < minp) NA_real_
                    else mean(.x <= .x[length(.x)], na.rm = TRUE),
                    .before = win - 1)
}

.regime_roll_max <- function(x, win, minp) {
  slider::slide_dbl(x, ~ if (sum(!is.na(.x)) < minp) NA_real_
                    else max(.x, na.rm = TRUE),
                    .before = win - 1)
}

# full-history features + GMM regime for one ticker; NULL if unknown ticker
regime_score_ticker <- function(tkr) {
  m <- regime_model()
  g <- arrow::open_dataset(REGIME_IV) |>
    dplyr::filter(ticker == tkr) |>
    dplyr::select(dplyr::all_of(c("ticker", "tradeDate", m$iv_col, m$rv_col))) |>
    dplyr::collect() |>
    dplyr::arrange(tradeDate)
  if (nrow(g) < 300) return(NULL)

  # drop single-day bad prints (e.g. NVDA 2025-04 iv30d=380): >4x / <0.25x
  # the trailing 3-week median is a data error, not a vol regime
  med <- slider::slide_dbl(g[[m$iv_col]], stats::median,
                           .before = 20, .complete = FALSE)
  ratio <- g[[m$iv_col]] / med
  g <- g[is.na(ratio) | (ratio < 4 & ratio > 0.25), ]

  liv <- log(ifelse(g[[m$iv_col]] <= 0, NA, g[[m$iv_col]]))
  lrv <- log(ifelse(g[[m$rv_col]] <= 0, NA, g[[m$rv_col]]))
  g$iv     <- g[[m$iv_col]]
  g$vrp    <- liv - lrv
  g$iv_pct <- .regime_roll_pct(liv)
  g$rv_pct <- .regime_roll_pct(lrv)
  g$iv_off_peak <- liv - .regime_roll_max(liv, 21, 10)
  g$iv_mom <- liv - dplyr::lag(liv, 10)
  g$stress_recency <- .regime_roll_max(g$iv_pct, 63, 21)
  g <- g[stats::complete.cases(g[m$feats]), ]
  if (!nrow(g)) return(NULL)

  Z <- sweep(sweep(as.matrix(g[m$feats]), 2, m$center), 2, m$scale, "/")
  g$regime <- m$labels[mclust::predict.Mclust(m$gmm, newdata = Z)$classification]
  extreme <- g$regime %in% c("Stress", "High Stress", "Post Stress") &
    ((g$iv_pct > 0.99 & g$rv_pct > 0.80) | (g$iv_pct > 0.97 & g$rv_pct > 0.90))
  g$regime[extreme] <- "Extreme Stress"
  g
}

regime_timeline_plotly <- function(tkr, months = 12) {
  g <- regime_score_ticker(tkr)
  if (is.null(g))
    return(plotly::plot_ly() |>
             plotly::layout(title = paste("No ORATS history for", tkr)))
  g <- dplyr::filter(g, tradeDate >= max(tradeDate) - months * 30.5)
  g$regime <- droplevels(factor(g$regime, levels = names(REGIME_COLORS)))
  g$hover <- paste0(format(g$tradeDate), "  IV ", round(g$iv, 1),
                    "<br>", g$regime,
                    "<br>IV pctile ", round(100 * g$iv_pct),
                    " | RV pctile ", round(100 * g$rv_pct),
                    " | VRP ", round(g$vrp, 2))
  plotly::plot_ly(g, x = ~tradeDate, y = ~iv,
                  color = ~regime, colors = REGIME_COLORS[levels(g$regime)],
                  type = "scatter", mode = "markers",
                  marker = list(size = 7),
                  text = ~hover, hoverinfo = "text") |>
    plotly::layout(
      title = list(text = paste0(tkr, " — 30DTE IV regime timeline"),
                   font = list(size = 14, color = "#0b0b0b")),
      xaxis = list(title = "", gridcolor = "#f0efec"),
      yaxis = list(title = "IMPLIED VOLATILITY", rangemode = "tozero",
                   gridcolor = "#e1e0d9",
                   titlefont = list(size = 10, color = "#898781")),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)"
    )
}

regime_timeline_plot <- function(tkr, months = 12) {
  g <- regime_score_ticker(tkr)
  if (is.null(g))
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", 0, 0, label = paste("No ORATS history for", tkr)) +
             ggplot2::theme_void())
  g <- dplyr::filter(g, tradeDate >= max(tradeDate) - months * 30.5)
  g$regime <- factor(g$regime, levels = names(REGIME_COLORS))
  ggplot2::ggplot(g, ggplot2::aes(tradeDate, iv, color = regime,
                                  text = paste0(format(tradeDate), "  IV ", round(iv, 1),
                                                "<br>", regime,
                                                "<br>IV pctile ", round(100 * iv_pct),
                                                " | RV pctile ", round(100 * rv_pct),
                                                " | VRP ", round(vrp, 2)))) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_color_manual(values = REGIME_COLORS, drop = TRUE, name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +  # (n.breaks breaks ggplotly)
    ggplot2::labs(title = paste0(tkr, " — 30DTE IV regime timeline"),
                  x = NULL, y = "IMPLIED VOLATILITY") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 11, color = "#0b0b0b"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e1e0d9", linetype = "dashed"),
      axis.text = ggplot2::element_text(color = "#898781"),
      axis.title.y = ggplot2::element_text(color = "#898781", size = 7),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(color = "#52514e", size = 8)
    )
}

# Refresh the slim store when this module is sourced (i.e. on every app start).
# Guarded: a data problem here must never stop the Shiny app from loading.
tryCatch(regime_iv_update(),
         error = function(e) message("[regime_iv] update skipped: ", conditionMessage(e)))
