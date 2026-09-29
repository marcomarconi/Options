# ---------------------------------------------------------------------------
# The CBOE volatility-index complex, for the VIX tab.
#
# CBOE publishes each index's whole history as one free CSV and refreshes it
# nightly, so there is no incremental logic here: the eleven files together are
# ~2 MB and are simply re-pulled once a day into a cached parquet. The cache is
# what the app reads; the network is touched only when it is stale.
#
# Two schemas come back from the same endpoint. The VIX-family files are OHLC
# (DATE, OPEN, HIGH, LOW, CLOSE); VVIX and SKEW are (DATE, <NAME>). Both are
# normalised to date/series/close here so the wide table downstream is uniform.
#
# NOT included, deliberately: the VIX futures basis. Sinclair watches the front
# future against spot VIX, and Barchart/Futures/VIX is the only local source -
# but it stops in 2024, and its Time column mixes m/d/Y with Y-m-d between
# contracts (the known Barchart quirk). A basis panel off that would be wrong
# more often than right. VIX1D/VIX9D against VIX carry the same "front of the
# curve is dislocated" information from data that is clean and current.
# ---------------------------------------------------------------------------

CBOE_URL <- "https://cdn.cboe.com/api/global/us_indices/daily_prices/%s_History.csv"

# term structure, then vol-of-vol, then the other index VIXes, then skew
VIX_TERM  <- c("VIX1D", "VIX9D", "VIX", "VIX3M", "VIX6M", "VIX1Y")
VIX_OTHER <- c("VVIX", "VXN", "RVX", "VXD", "SKEW",
               # implied correlation and dispersion. These are the missing term
               # in the cross-product panel: index vol can be cheap against
               # single-name vol either because index vol is cheap or because
               # correlation is low, and nothing else here separates the two.
               "COR1M", "COR3M", "DSPX")
VIX_ALL   <- c(VIX_TERM, VIX_OTHER)

# SPX itself rides in the same cache. The flow-spike screen needs a spot leg,
# and taking it from ORATS_core's SPY limited that panel to the app's loaded
# window - about 510 days, in which a big VIX move without a spot move happens
# perhaps seven times. Yahoo's ^GSPC goes back to 1990, which is exactly the
# span of the VIX file, so the screen gets 36 years instead of two.
VIX_SPOT <- "SPX"

# Cross-asset implied vol, for "is this an equity event or everyone's?". Kept
# out of VIX_ALL so the monitor table does not grow a row per asset; they ride
# in the same cache and get a percentile like the ratios do.
#
# All CBOE, all % vol on a price, so their levels share one axis. The bond leg
# is VXTLT (TLT options) rather than ICE's MOVE: MOVE is a basis-point vol on
# yields, so its level is not comparable, and it tracks VXTLT closely anyway.
# TYVIX, EVZ and the sector VXs are discontinued (the CBOE endpoint returns
# AccessDenied), hence not here.
VIX_XASSET <- tibble::tribble(
    ~key,    ~label,
    "VIX",   "VIX (SPX)",
    "VXTLT", "VXTLT (20y+ Treasury)",
    "GVZ",   "GVZ (gold)",
    "OVX",   "OVX (crude)",
    "VXEEM", "VXEEM (EM equity)"
)

# the ETF whose realised vol stands in for each index's underlying, so an
# implied ratio can be checked against the realised one it should reflect
VIX_PROXY <- c(VIX = "SPY", VXN = "QQQ", RVX = "IWM", VXD = "DIA")

fetch_cboe_index <- function(sym) {
    df <- try(readr::read_csv(sprintf(CBOE_URL, sym), show_col_types = FALSE,
                              progress = FALSE), silent = TRUE)
    if (inherits(df, "try-error") || nrow(df) == 0) {
        warning(paste("CBOE fetch failed for", sym)); return(NULL)
    }
    names(df)[1] <- "date"
    # OHLC files carry CLOSE; the single-column ones name the column after the
    # index itself. Take CLOSE when present, otherwise the second column.
    val <- if ("CLOSE" %in% names(df)) "CLOSE" else names(df)[2]
    tibble::tibble(date = as.Date(df$date, format = "%m/%d/%Y"),
                   series = sym,
                   close = suppressWarnings(as.numeric(df[[val]]))) %>%
        dplyr::filter(!is.na(date), is.finite(close), close > 0)
}

# quantmod rather than a raw CSV endpoint: Stooq now sits behind a JS challenge
# and returns an HTML page to curl, and getSymbols already handles Yahoo's
# quirks. Failure is non-fatal - the rest of the tab does not depend on it.
fetch_yahoo <- function(ticker, series) {
    x <- try(quantmod::getSymbols(ticker, src = "yahoo", from = "1990-01-01",
                                  auto.assign = FALSE), silent = TRUE)
    if (inherits(x, "try-error")) { warning(paste(series, "fetch failed")); return(NULL) }
    tibble::tibble(date = as.Date(zoo::index(x)),
                   series = series,
                   close = as.numeric(quantmod::Cl(x))) %>%
        dplyr::filter(is.finite(close), close > 0)
}
fetch_spx <- function() fetch_yahoo("^GSPC", VIX_SPOT)

update_vix_complex <- function(cache_file, force = FALSE) {
    have <- if (file.exists(cache_file)) arrow::read_parquet(cache_file) else NULL
    # CBOE posts after the close, so "yesterday or later" is fresh enough; this
    # keeps the app from re-downloading on every restart during a session.
    # a cache written before a series was added to the lists is stale too
    want <- unique(c(VIX_ALL, VIX_SPOT, VIX_XASSET$key))
    fresh <- !is.null(have) && max(have$date) >= (Sys.Date() - 1) &&
        all(want %in% unique(have$series))
    if (fresh && !force) return(have)
    print("Fetching the CBOE volatility index complex...")
    xa <- setdiff(VIX_XASSET$key, VIX_ALL)
    got <- purrr::map(c(VIX_ALL, xa), fetch_cboe_index) %>% purrr::compact()
    got <- c(got, list(fetch_spx())) %>% purrr::compact()
    if (length(got) == 0) {
        if (is.null(have)) stop("no CBOE data and no cache")
        warning("CBOE unreachable - using the cached VIX complex")
        return(have)
    }
    out <- dplyr::bind_rows(got) %>% dplyr::arrange(series, date)
    arrow::write_parquet(out, cache_file)
    out
}

# ---------------------------------------------------------------------------
# Derived series. Everything a ratio, because a level tells you nothing without
# the rest of the curve to compare it to - and everything then ranked, because
# a ratio tells you nothing without its own history. Both of Sinclair's tools:
# a z-score where the thing mean-reverts on a known timescale, a percentile
# where it drifts (he uses both, and says which is which).
# ---------------------------------------------------------------------------

# CBOE's VIX_History.csv carries a row on US market holidays that no other
# index in the complex has - 33 of them, all but one since 2022 (Memorial Day,
# Juneteenth, July 4, Labor Day, Thanksgiving, MLK, Presidents' Day, plus
# 2004-06-11 for Reagan's funeral). On those dates VIX has a value and VIX1D,
# VIX9D, VIX3M, VIX6M, VIX1Y, VVIX and SPX are all absent, which breaks every
# line in the term-structure plot once a month or so. The values are not even
# carry-forwards - only 1 of the 33 repeats the prior close - so they are some
# indicative calculation on a day nothing traded, and they inject two spurious
# one-day moves into dVIX around every holiday.
#
# SPX is the session calendar: ^GSPC has a bar on every day the market opened
# and none on the days it did not. Dropping rows without it removes all 33
# holidays and keeps the four days (1991-03-01, 1997-01-31, 1997-11-26,
# 1999-12-31) where the VIX file itself is missing and the gap is real.
vix_wide <- function(vix_long, trading_days_only = TRUE) {
    w <- tidyr::pivot_wider(vix_long, names_from = series, values_from = close) %>%
        dplyr::arrange(date)
    has_spx <- VIX_SPOT %in% names(w) &&
        sum(!is.na(w[[VIX_SPOT]])) > 0.5 * nrow(w)   # guard: the SPX pull can fail
    if (trading_days_only && has_spx)
        w <- w %>% dplyr::filter(!is.na(.data[[VIX_SPOT]]))
    w
}

# how many rows the session filter removes, for the startup line
vix_holiday_rows <- function(vix_long)
    nrow(vix_wide(vix_long, FALSE)) - nrow(vix_wide(vix_long, TRUE))

# TTR's rolling functions reject interior NAs, and the series start at
# different dates (VIX 1990, VVIX 2006, VIX1D 2022), so a ratio is NA on the
# left of whichever leg starts later. Rank only the contiguous tail.
rank_tail <- function(x, n) {
    out <- rep(NA_real_, length(x))
    ok <- which(is.finite(x))
    if (length(ok) < n + 1) return(out)
    i <- min(ok):length(x)
    v <- x[i]; v[!is.finite(v)] <- NA; v <- zoo::na.locf(v, na.rm = FALSE)
    out[i] <- TTR::runPercentRank(v, n, cumulative = FALSE) * 100
    out
}
z_tail <- function(x, n) {
    out <- rep(NA_real_, length(x))
    ok <- which(is.finite(x))
    if (length(ok) < n + 1) return(out)
    i <- min(ok):length(x)
    v <- x[i]; v[!is.finite(v)] <- NA; v <- zoo::na.locf(v, na.rm = FALSE)
    out[i] <- (v - TTR::runMean(v, n)) / TTR::runSD(v, n)
    out
}

# Every series the tab ranks against its own history, in one table so the
# plots, the monitor and the tooltips cannot drift apart. Mostly ratios,
# because a level tells you nothing without the rest of the curve to compare it
# to - but VIX and VVIX are carried as levels too, since "where is vol, and
# where is vol-of-vol" is a percentile question in its own right.
# `plot` gates the percentiles chart only - everything here is ranked and
# everything reaches the monitor table. The five term-structure ratios are
# tracked but not drawn: five lines in one pane is unreadable, and the curve
# plot at the top of the tab already shows that shape directly.
VIX_RATIOS <- tibble::tribble(
    ~key,        ~label,                  ~group,           ~plot,
    "VIX1D_VIX", "VIX1D / VIX",           "term structure", FALSE,
    "VIX9D_VIX", "VIX9D / VIX",           "term structure", FALSE,
    "VIX_VIX3M", "VIX / VIX3M",           "term structure", FALSE,
    "VIX3M_VIX6M","VIX3M / VIX6M",        "term structure", FALSE,
    "VIX6M_VIX1Y","VIX6M / VIX1Y",        "term structure", FALSE,
    "VIX",       "VIX  (level)",          "vol of vol",     TRUE,
    "VVIX",      "VVIX (level)",          "vol of vol",     TRUE,
    "VIX_VVIX",  "VIX / VVIX",            "vol of vol",     TRUE,
    "VXN_VIX",   "VXN / VIX  (NDX/SPX)",  "cross product",  TRUE,
    "RVX_VIX",   "RVX / VIX  (RUT/SPX)",  "cross product",  TRUE,
    "VXD_VIX",   "VXD / VIX  (DJI/SPX)",  "cross product",  TRUE,
    # ranked in the monitor, drawn on their own panel rather than as
    # percentiles - unlike a ratio, a correlation level means something
    "COR1M",     "COR1M (1m implied corr)","correlation",   FALSE,
    "COR3M",     "COR3M (3m implied corr)","correlation",   FALSE,
    "DSPX",      "DSPX (dispersion)",      "correlation",   FALSE
)

# short, similar-width pane labels for the faceted plot: a long y-axis title
# widens that plot's left margin and knocks it out of line with the others
VIX_PANE <- c("term structure" = "term", "vol of vol" = "vol of vol",
              "cross product" = "cross")

build_vix_derived <- function(vix_long, lookback = 252) {
    w <- vix_wide(vix_long)
    d <- w %>% dplyr::mutate(
        VIX1D_VIX    = VIX1D / VIX,
        VIX9D_VIX    = VIX9D / VIX,
        VIX_VIX3M    = VIX   / VIX3M,
        VIX3M_VIX6M  = VIX3M / VIX6M,
        VIX6M_VIX1Y  = VIX6M / VIX1Y,
        VIX_VVIX     = VIX   / VVIX,
        VXN_VIX      = VXN   / VIX,
        RVX_VIX      = RVX   / VIX,
        VXD_VIX      = VXD   / VIX,
        # one-day change in VIX, in points and as a z-score of its own history:
        # the input to the flow-spike screen below
        dVIX         = VIX - dplyr::lag(VIX),
        spx_ret      = if ("SPX" %in% names(.)) log(SPX / dplyr::lag(SPX)) else NA_real_,
        vix_chg      = log(VIX / dplyr::lag(VIX))
    )
    # 60-day rolling correlation of the two. It is the continuous version of
    # the flow-spike screen: a day when VIX moves and spot does not is one
    # observation, this is the same thing measured as a running relationship.
    d$spx_vix_cor <- roll_cor(d$spx_ret, d$vix_chg, 60)
    # cross-asset series may be missing if their fetch failed; skip, not stop
    xa <- intersect(VIX_XASSET$key, names(d))
    for (k in unique(c(VIX_RATIOS$key, xa, "dVIX"))) {
        d[[paste0(k, "_pct")]] <- rank_tail(d[[k]], lookback)
        d[[paste0(k, "_z")]]   <- z_tail(d[[k]], lookback)
    }
    d
}

roll_cor <- function(x, y, n) {
    out <- rep(NA_real_, length(x))
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < n + 1) return(out)
    i <- which(ok)
    v <- rep(NA_real_, length(i))
    for (k in seq(n, length(i))) {
        idx <- i[(k - n + 1):k]
        v[k] <- suppressWarnings(cor(x[idx], y[idx]))
    }
    out[i] <- v
    out
}

# ---------------------------------------------------------------------------
# Fixed-strike volatility (Jared Stocks' chart #14).
#
# ATM IV alone cannot tell you whether vol moved because SOMEONE BOUGHT IT or
# because spot slid along a fixed smile - the ATM strike itself moves with
# spot. Holding the strike fixed and comparing two consecutive sessions
# separates the two: the smile shifting up at unchanged strikes is a purchase,
# the smile staying put while spot moves is not. "Market up + vol up" is the
# reading that matters, and it means hedges are being bought into a catalyst.
#
# Source is the per-ticker API archive, which for SPY runs 2007-01-03 to
# 2026-03-24. It is only as current as the last downloader run, so the panel
# labels both dates rather than saying "yesterday" and "today".
# ---------------------------------------------------------------------------
SPY_CHAIN_DIR <- "/home/marco/trading/HistoricalData/ORATS/API/strikes/SPY"

spy_chain_sessions <- function(n = 2) {
    fs <- sort(list.files(SPY_CHAIN_DIR, "^orats_SPY_[0-9]{8}\\.csv\\.gz$"))
    utils::tail(fs, n)
}

read_spy_chain <- function(file) {
    p <- file.path(SPY_CHAIN_DIR, file)
    d <- try(data.table::fread(cmd = paste("zcat", shQuote(p)), showProgress = FALSE),
             silent = TRUE)
    if (inherits(d, "try-error")) return(NULL)
    tibble::as_tibble(d) %>%
        dplyr::transmute(tdate = as.Date(tradeDate), expir = as.Date(expirDate),
                         dte = as.integer(dte), strike = as.numeric(strike),
                         spot = as.numeric(stockPrice), iv = as.numeric(smvVol),
                         delta = as.numeric(delta)) %>%
        # the API archive's smvVol carries zeros-as-NA and occasional garbage,
        # the same defect documented for the UNG chains in _data/README.md
        dplyr::filter(is.finite(iv), iv > 0, iv < 3, is.finite(strike))
}

fixed_strike_pair <- function(target_dte = 30) {
    fs <- spy_chain_sessions(2)
    if (length(fs) < 2) return(NULL)
    a <- read_spy_chain(fs[1]); b <- read_spy_chain(fs[2])
    if (is.null(a) || is.null(b)) return(NULL)
    # the SAME expiry on both days, or the comparison is across two contracts.
    # intersect() drops the Date class, so put it back before any date maths.
    common <- as.Date(intersect(unique(a$expir), unique(b$expir)), origin = "1970-01-01")
    common <- common[common > max(b$tdate)]
    if (!length(common)) return(NULL)
    pick <- common[which.min(abs(as.numeric(common - max(b$tdate)) - target_dte))]
    j <- dplyr::inner_join(
        a %>% dplyr::filter(expir == pick) %>% dplyr::select(strike, iv_prev = iv),
        b %>% dplyr::filter(expir == pick) %>% dplyr::select(strike, iv_now = iv),
        by = "strike")
    list(data = j, expir = as.Date(pick),
         d_prev = max(a$tdate), d_now = max(b$tdate),
         s_prev = a$spot[1], s_now = b$spot[1])
}

# ---------------------------------------------------------------------------
# The flow-spike screen (Sinclair, 4:20): "occasionally you'll see a big spike
# in the VIX and the SPY does nothing ... that's a pretty good bet that's going
# to revert, because that's some sort of flow issue in the options that isn't
# driven by any relationship you'd expect to the stock."
#
# Mechanically: flag days where the VIX move is large in its own terms AND the
# underlying barely moved. The control is the opposite corner - a big VIX move
# WITH a big spot move, which should not revert, and which the scatter shows
# alongside so the flagged set is never read on its own.
#
# The spot leg comes from ORATS_core's SPY (pxAtmIv), so this panel sees only
# the app's loaded window (days_to_load), not the CBOE history. It widens by
# itself if days_to_load grows.
# ---------------------------------------------------------------------------
vix_spike_screen <- function(vix_derived, z_min = 1.5, move_max = 0.005,
                             horizons = c(1, 3, 5), spot_df = NULL) {
    d <- vix_derived %>% dplyr::select(date, VIX, dVIX, dVIX_z, spx_ret) %>%
        dplyr::arrange(date)
    # fall back to an ORATS spot column if the SPX pull failed, so the panel
    # degrades to the short window instead of disappearing
    if (all(is.na(d$spx_ret)) && !is.null(spot_df)) {
        sp <- spot_df %>%
            dplyr::transmute(date = as.Date(tradeDate), px = pxAtmIv) %>%
            dplyr::arrange(date) %>%
            dplyr::mutate(spx_ret2 = log(px / dplyr::lag(px)))
        d <- d %>% dplyr::select(-spx_ret) %>%
            dplyr::inner_join(sp %>% dplyr::select(date, spx_ret = spx_ret2), by = "date")
    }
    d <- d %>% dplyr::filter(is.finite(VIX))
    for (h in horizons) {
        d[[paste0("fwd", h)]] <- dplyr::lead(d$VIX, h) - d$VIX
    }
    d %>% dplyr::mutate(
        flag = dplyr::case_when(
            is.na(dVIX_z) | is.na(spx_ret)                 ~ NA_character_,
            dVIX_z >= z_min & abs(spx_ret) <= move_max     ~ "VIX up, spot flat",
            dVIX_z >= z_min                                ~ "VIX up, spot moved",
            TRUE                                           ~ "other"
        )
    )
}
