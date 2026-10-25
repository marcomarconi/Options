
# ui2.R = ui.R plus a "Chain" tab (live strikes chain: expiry strip, strike
# ladder, structure pricer). Run with: shiny::runApp(source("ui2.R", local = TRUE)$value)
{
library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggthemes)
library(arrow)
library(plotly)
library(DT)
library(ggcorrplot)
source("/home/marco/trading/Systems/Options/OptionsCommon.R")
source("/home/marco/trading/Systems/Common/Common.R")
source("/home/marco/trading/Systems/Options/regime_timeline/src/regime_shiny.R")
source("/home/marco/trading/Systems/Options/Shiny/vix_data.R")
}
{
    initialize <- FALSE   
    orats_dir <- "/home/marco/trading/HistoricalData/ORATS/"
    core_dir <- "/home/marco/trading/HistoricalData/ORATS/core/"
    delayed_dir <- "/home/marco/trading/HistoricalData/ORATS/delayed/"
    options_dir <- "/home/marco/trading/Systems/Options/Data/"
    # ui2 caches its own copy: it loads the skew grid ui.R does not, and the
    # two apps sharing one file and one signature would each see the other's
    # column set as a change and rebuild it from the raw core files on every
    # switch. Options.R and regime_timeline keep reading ORATS_core.pq.
    ORATS_core_file <- paste0(options_dir, "ORATS_core_ui2.pq")
    ORATS_code_delayed_file <- paste0(delayed_dir, "orats_core_delayed.csv")
    # a cached parquet holds whatever universe was current when it was
    # written; these record it so a widened screener can be detected
    ORATS_universe_file <- paste0(options_dir, "ORATS_universe_ui2.rds")
    ORATS_ohlc_universe_file <- paste0(options_dir, "ORATS_ohlc_universe_ui2.rds")
    days_to_load <- 500
    # A ticker had to appear in every one of the days_to_load core files to
    # survive init_ORATS_core, which made a single missing vendor row fatal:
    # ORATS dropped the plain KRE line on 2026-03-26 (publishing only KRE_C
    # that day), leaving 499 rows and disqualifying KRE from the whole app.
    # 82 symbols - KMI, CLSK, SW, AI among them - were lost to gaps of 10 days
    # or fewer. The filter exists to guarantee history for the rolling
    # windows, the widest of which is 252d, so a 5% tolerance keeps that
    # intent with room to spare while surviving the vendor's silent gaps.
    min_days <- ceiling(days_to_load * 0.95)

    etf_screener <- read_csv("/home/marco/trading/Systems/Options/etf-screener-weekly-options.csv", show_col_types = F)
    stock_screener <- read_csv("/home/marco/trading/Systems/Options/stocks-screener-08-23-2025.csv", show_col_types = F)

    # The app was ETF-only because init_ORATS_core joined the ETF screener
    # alone and stock_screener was loaded but never used. Single names come in
    # here. The ETF list carries an Asset Class and the stock list does not,
    # so label those "Stock"; distinct() keeps the ETF row for the 9 symbols
    # that appear on both lists and stops the join fanning out.
    combined_screener <- bind_rows(
        etf_screener   %>% dplyr::select(Symbol, `Asset Class`),
        stock_screener %>% dplyr::select(Symbol) %>% mutate(`Asset Class` = "Stock")
    ) %>% distinct(Symbol, .keep_all = TRUE)

today_date <- Sys.Date() 

# function for loading an ORATS core data file and returning a subset of columns
quiet_fread <- purrr::quietly((.f = fread))
load_orats_day <- function(filename, cols_to_extract) {
    print(filename)
    # select= parses only the wanted columns of the ~310 in the file; the
    # result is identical, about 30% faster (gunzip is the rest)
    quiet_fread(glue::glue(core_dir, "{filename}"), select = cols_to_extract) %>%
        purrr::pluck("result")  %>%  
        select(all_of(cols_to_extract)) %>%
        # everything except the key and the date columns is numeric; ernDate1
        # is an m/d/Y string and as.single() would silently turn it into NA
        mutate(across(-any_of(date_cols), ~ as.single(.))) %>%
        mutate(across(any_of(setdiff(date_cols, c("ticker", "tradeDate"))),
                      ~ as.Date(.x, format = "%m/%d/%Y"))) %>%
        mutate(across(
            where(~ inherits(., "IDate")),
            ~ as.Date(.)
        ))
}

# Read many daily files and stack them. A full rebuild reads ~500 core files
# (or ~800 dailies for the OHLC seed), so fan them out over forked workers -
# 2.3x on 4 cores, identical output - with fread single-threaded inside each.
# A worker that dies (out of memory, say) comes back as a try-error, in which
# case read them serially rather than fail the startup. rbindlist instead of
# map_df: the bind alone took 26s of a rebuild under bind_rows.
read_days <- function(files, loader, ...) {
    if (!length(files)) return(NULL)
    res <- parallel::mclapply(files, function(f) { setDTthreads(1); loader(f, ...) },
                              mc.cores = max(1, min(4, parallel::detectCores() - 1)))
    if (any(vapply(res, function(r) inherits(r, "try-error") || is.null(r), NA))) {
        print("Parallel read failed - reading serially")
        res <- lapply(files, loader, ...)
    }
    out <- rbindlist(res)
    # as.single() flags every column Csingle; bind_rows dropped the flag and
    # rbindlist keeps it, and arrow would write it into the cache. Strip it so
    # the frame is exactly what map_df produced.
    for (k in names(out)) setattr(out[[k]], "Csingle", NULL)
    out
}

cols_to_extract <- c('ticker', 'tradeDate', 'pxAtmIv', 'hiStrikeM1', 'hiStrikeM2',
                     "stkPxChng1wk", "stkPxChng1m", "stkPxChng6m",
                     "straPxM1", "straPxM2", "atmIvM1",	"atmIvM2", "atmIvM3",	"atmIvM4",
                     "avgOptVolu20d", "cVolu",  "cOi" , "pVolu",  "pOi", 
                     "dtExM1","dtExM2", 
                     "exErnIv10d", "exErnIv30d", "exErnIv60d", "exErnIv90d", "exErnIv6m", "exErnIv1yr", "volOfVol", "volOfIvol", 
                     "orHvXern10d", "orHvXern20d", "orHvXern60d", "orHvXern90d", "orHvXern120d", "orHvXern252d",
                     "clsHvXern10d", "clsHvXern20d", "clsHvXern60d", "clsHvXern90d", "clsHvXern120d", "clsHvXern252d",
                     "ivPctile1y", "ivHvXernRatio", "ivSpyRatio", "correlSpy1m",
                     "fexErn30_20", "fexErn60_30", "fexErn90_60", "fexErn180_90", "fexErn90_30",
                     "slope", "contango", "deriv",
                     # Earnings. ORATS never populates nextErn/daysToNextErn in
                     # this feed - both read 0000-00-00 and 0 for every ticker,
                     # this year and last - and the earnings/ directory is
                     # empty, so days-to-NEXT-earnings is simply not available.
                     # ernDate1 is the last REPORTED date, which given the
                     # quarterly cadence still tells you whether one is due.
                     "ernDate1", "absAvgErnMv", "impliedIee",
                     # Screener additions. mktWidthVol is the bid/ask width in
                     # vol points - what crossing costs, which volume does not
                     # tell you. fcstStraPxM1 and orIvFcst20d give ORATS's own
                     # forecast to price against, the forward-looking
                     # counterpart to VRP (which compares IV to REALIZED vol).
                     # confidence/error gate the fit quality. ivPctileSpy is a
                     # real cross-sectional rank, unlike ivSpyRatio.
                     "mktWidthVol", "fcstStraPxM1", "orIvFcst20d",
                     "confidence", "error", "ivPctileSpy",
                     # Skew and surface. ORATS labels the grid by CALL delta,
                     # so dlt5/dlt25 are the 5d/25d call wing and dlt75/dlt95
                     # the 25d/5d PUT wing (SPY 2026-10-23, 30d: dlt5 11.9,
                     # dlt95 31.1). Ex-earnings, to match the exErnIv ATM
                     # columns every other Ticker panel uses; the raw fields
                     # put a front-tenor earnings bump into the wings.
                     as.vector(outer(c("exErnDlt5Iv", "exErnDlt25Iv", "exErnDlt75Iv", "exErnDlt95Iv"),
                                     c("10d", "30d", "60d", "90d", "6m", "1y"), paste0)),
                     # 5-day realized, for RV5/RV20, on ORATS' intraday estimator:
                     # far less noisy than close-to-close (vrp_etf_v2 rv5_family.py:
                     # week-to-week persistence 0.65 vs 0.43), and the family the
                     # RV-surprise result was measured on
                     "orHvXern5d"
)

# columns in cols_to_extract that must not be coerced to numeric on load
date_cols <- c("ticker", "tradeDate", "ernDate1")


# TTR's rolling functions reject a series with interior NAs outright, and NaN
# counts as NA to them. Across ~4k single names there are many ways to produce
# one (a 0 or a negative through a log, a ratio of two 0s, a gap in coverage),
# so normalise instead of chasing each cause: anything non-finite becomes NA
# and is carried forward, leaving only leading NAs.
roll_safe <- function(x) {
    x[!is.finite(x)] <- NA
    na.locf(x, na.rm = FALSE)
}

# A cached parquet holds the universe AND the column set it was built for.
# Widening either cannot be repaired by appending days - the new tickers have
# no history in the cache, and the new columns no values - so record the
# signature next to the cache and rebuild when it changes.
.as_sig <- function(x) if (is.list(x)) x else sort(unique(x))
universe_changed <- function(sig_file, universe) {
    !file.exists(sig_file) || !identical(readRDS(sig_file), .as_sig(universe))
}
save_universe <- function(sig_file, universe) saveRDS(.as_sig(universe), sig_file)

create_ORATS_core <- function(core_dir){
    print("Force load ORATS core file")
    files <- list.files(core_dir, "orats_core_202[0-9].*gz")
    files_sorted <- files[order(as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1", basename(files)),format = "%Y%m%d"))]
    ORATS_core <- files_sorted %>% tail(days_to_load) %>% read_days(load_orats_day, cols_to_extract) %>% arrange(ticker, tradeDate)
    return(ORATS_core) 
}

init_ORATS_core <- function(ORATS_core, screener) {
    ORATS_core %>% select(-any_of("class")) %>% 
        right_join(screener %>% 
                       dplyr::select(Symbol, `Asset Class`), by=c("ticker" = "Symbol"),relationship="many-to-many") %>% 
        rename(class = `Asset Class`)%>% 
        group_by(ticker) %>% arrange(tradeDate) %>% dplyr::filter(n() >= min_days) %>% 
        mutate(
            class = if_else(is.na(class), "Stock", class),
            # Single names carry 0s in these columns where the ETF universe
            # did not, and 0 through a log() or a ratio becomes NaN mid-series
            # - which TTR's runSum/runPercentRank reject outright ("series
            # contains non-leading NAs"). Treat 0 as the missing value it is
            # and carry the last observation forward, the same way steepness
            # below has always been handled, so only leading NAs survive.
            iv30_clean = na.locf(na_if(exErnIv30d, 0), na.rm = FALSE),
            rv20_clean = na.locf(na_if(clsHvXern20d, 0), na.rm = FALSE),
            # VRP keeps its honest gaps; only what feeds a rolling window is
            # filled, so the plotted series is not silently carried forward
            VRP = log(lag(iv30_clean, 20) / rv20_clean), # Realized VRP, NOT future VRP
            VRPzscore = runZscore(roll_safe(VRP), 252),
            rvPctile1y = TTR::runPercentRank(roll_safe(rv20_clean), 252) * 100,
            # days since the last REPORTED earnings, not days until the next:
            # see the note on cols_to_extract. On a quarterly reporter this
            # still separates "just reported" from "about to report".
            daysSinceErn = as.numeric(as.Date(tradeDate) - as.Date(ernDate1)),
            daysSinceErn = if_else(is.na(daysSinceErn) | daysSinceErn < 0,
                                   NA_real_, daysSinceErn),
            steepness_30d90d = log(iv30_clean/na_if(exErnIv90d, 0)) %>% na.locf(na.rm=F),
            steepness_30d6m = log(iv30_clean/na_if(exErnIv6m, 0)) %>% na.locf(na.rm=F)

        ) %>% dplyr::select(-iv30_clean, -rv20_clean)
}



# Slim OHLC store for the realized-vol estimators. The core file carries
# closes only; the dailies carry open/hi/lo, from the same downloader and the
# same trade dates. Seeded once from the last days_to_load + 300 dailies (the
# extra days warm up a 252d estimator at the left edge of the widest window),
# then only new days are appended on each app start - the regime_iv.pq
# pattern. Restricted to the core universe (~1.2k tickers of the 6.3k in a
# daily file), so it stays a few tens of MB.
dailies_dir <- paste0(orats_dir, "dailies/")
# The adjusted open/hiPx/loPx/clsPx are split-adjusted as of the day each
# file was DOWNLOADED, so a store stitched from many files mixes vintages:
# XLE's 2025-07-09 and 2025-11-24 files were re-fetched after its 2025-12-05
# 2-for-1 and print half their neighbours' price - fake +-71% returns. The
# unadjusted fields do not depend on the vintage; ticker_ohlc() back-adjusts
# them with split_adjust(). Own file, because ui.R's store lacks these columns.
ORATS_ohlc_file <- paste0(options_dir, "ORATS_ohlc_ui2.pq")
ohlc_cols <- c("ticker", "tradeDate", "unadjOpen", "unadjHiPx", "unadjLoPx", "unadjClsPx")

# ORATS split history; the newest file is cumulative. Its dates are hints, not
# facts: AZN's 2026-02-02 jump lands a session later, AMCR's and AIFU's days
# later, ALIT lists the same 1-for-20 on two consecutive days, and ADPT's
# listed split never shows in the prices. split_adjust() therefore looks for
# the jump each entry implies near its date and adjusts only where it finds one.
ORATS_splits <- local({
    sf <- sort(list.files(paste0(orats_dir, "splits/"), "orats_splits_[0-9]{8}\\.csv\\.gz", full.names = TRUE))
    if (!length(sf)) return(tibble(ticker = character(), splitDate = as.Date(character()), divisor = numeric()))
    read_csv(tail(sf, 1), show_col_types = FALSE) %>%
        mutate(splitDate = as.Date(splitDate)) %>%
        dplyr::filter(is.finite(divisor), divisor > 0, divisor != 1) %>%
        distinct() %>% arrange(ticker, splitDate)
})

# Back-adjust a price series to today's share basis. For each listed split,
# take the session within [-5, +7] calendar days of its date whose return is
# closest to the jump the divisor implies (log(1/divisor)); accept it within
# 25%, and divide everything before it by the divisor. Applied one split at a
# time on the already-adjusted series, so a duplicate entry finds no jump left
# and is skipped. The sessions it adjusted on ride along as attr "jumps".
split_adjust <- function(dates, px, tkr) {
    f <- rep(1, length(px)); jumps <- as.Date(character())
    sp <- ORATS_splits[ORATS_splits$ticker == tkr, ]
    if (!nrow(sp) || length(px) < 2)
        return(structure(px, jumps = jumps))
    for (i in seq_len(nrow(sp))) {
        win <- which(dates >= sp$splitDate[i] - 5 & dates <= sp$splitDate[i] + 7)
        win <- win[win > 1]
        if (!length(win)) next
        a <- px / f
        err <- abs(log(a[win] / a[win - 1]) + log(sp$divisor[i]))
        if (all(!is.finite(err)) || min(err, na.rm = TRUE) > log(1.25)) next
        j <- win[which.min(err)]
        f[seq_len(j - 1)] <- f[seq_len(j - 1)] * sp$divisor[i]
        jumps <- c(jumps, dates[j])
    }
    structure(px / f, jumps = jumps)
}

load_orats_daily <- function(filename, universe) {
    quiet_fread(glue::glue(dailies_dir, "{filename}"), select = ohlc_cols) %>%
        purrr::pluck("result") %>%
        dplyr::select(all_of(ohlc_cols)) %>%
        dplyr::filter(ticker %in% universe) %>%
        mutate(tradeDate = as.Date(tradeDate))
}

update_ORATS_ohlc <- function(universe) {
    files <- list.files(dailies_dir, "orats_dailies_[0-9]{8}\\.csv\\.gz")
    dates <- as.Date(sub("orats_dailies_([0-9]{8})\\.csv\\.gz", "\\1", files),
                     format = "%Y%m%d")
    files <- files[order(dates)]; dates <- sort(dates)
    keep  <- tail(seq_along(files), days_to_load + 300)
    files <- files[keep]; dates <- dates[keep]

    have <- if (file.exists(ORATS_ohlc_file)) read_parquet(ORATS_ohlc_file) else NULL
    # a store written before a column was added cannot be appended to
    if (!is.null(have) && !all(ohlc_cols %in% names(have))) {
        print("OHLC columns changed - reseeding the store")
        have <- NULL
    }
    # A store built for a narrower universe holds no rows at all for the new
    # tickers, and the date-range check below cannot see that - every date it
    # wants is already there. Drop it and reseed.
    if (!is.null(have) && universe_changed(ORATS_ohlc_universe_file, universe)) {
        print("OHLC universe changed - reseeding the store")
        have <- NULL
    }
    # Load anything the store is missing at EITHER end: new days on the right,
    # and older days on the left when days_to_load grows and the requested
    # window reaches back past what was seeded. Appending only on the right
    # would leave the estimators stopping short of the core's history.
    todo <- if (is.null(have)) files
            else files[dates > max(have$tradeDate) | dates < min(have$tradeDate)]
    if (length(todo)) {
        print(paste("Loading", length(todo), "ORATS dailies for OHLC"))
        have <- rbind(have, read_days(todo, load_orats_daily, universe)) %>%
            arrange(ticker, tradeDate)
        write_parquet(have, ORATS_ohlc_file)
    }
    save_universe(ORATS_ohlc_universe_file, universe)
    have
}

load_ORATS_core <- function(ORATS_core_file) {
    print(paste("Load ORATS core file", ORATS_core_file))
    ORATS_core <- read_parquet(ORATS_core_file) %>% arrange(tradeDate)
    return(ORATS_core)
}

update_ORATS_core <- function(ORATS_core, core_dir) {
    core_last_day <- ORATS_core %>% arrange(tradeDate) %>% tail(1) %>% pull(tradeDate) %>% as.Date
    files <- list.files(core_dir, "orats_core_202[0-9].*gz")
    files_sorted <- files[order(as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1", basename(files)),format = "%Y%m%d"))]
    last_day <- as.Date(tail(files_sorted, 1) %>% sub("orats_core_(.*)\\.csv\\.gz", "\\1", .), format="%Y%m%d")
    # if the temp file day is different from last core files day, attach the remaining ones
    if(core_last_day < last_day) {
        print(paste("Current tmp day is", core_last_day, "let's load the others"))
        core_last_day_string <- gsub("\\-", "", core_last_day)
        index <- grep(core_last_day_string, files_sorted)
        if(index >= length(files_sorted))
            stop("something went wrong")
        tmp <- files_sorted[(index+1):length(files_sorted)] %>% read_days(load_orats_day, cols_to_extract)
        ORATS_core <- rbind(ORATS_core, tmp) %>% arrange(ticker, tradeDate)
    }
    return(ORATS_core)
}

append_ORATS_delayed <- function(ORATS_core, ORATS_code_delayed_file) {
    if(file.exists(ORATS_code_delayed_file)) {
        ORATS_code_delayed <- read_csv(ORATS_code_delayed_file, show_col_types = FALSE) %>%
            dplyr::select(any_of(cols_to_extract))
        missing_cols <- setdiff(cols_to_extract, names(ORATS_code_delayed))
        if (length(missing_cols))   # a newly added column the delayed feed lacks
            ORATS_code_delayed[missing_cols] <- NA
        core_last_day <- ORATS_core %>% arrange(tradeDate) %>% tail(1) %>% pull(tradeDate) %>% as.Date
        delayed_last_day <- ORATS_code_delayed %>% arrange(tradeDate) %>% tail(1) %>% pull(tradeDate) %>% as.Date
        if(delayed_last_day > core_last_day) {
            ORATS_core <- rbind(ORATS_core, ORATS_code_delayed) %>% arrange(ticker, tradeDate)
        }
    }else{
        print("Delayed file not existing")
    }
    return(ORATS_core)
}
}

# Load ORATS_core on startup when missing from the session (or forced via
# initialize=TRUE). Fast path: plain parquet load. Slow path (only when new
# daily files or fresh delayed data exist): strip derived columns, append the
# new days, recompute the derived columns once, and re-cache — but never
# persist the provisional delayed rows, so the next real EOD file always
# replaces them.
# A frame already in the session may be ui.R's, which lacks the skew grid.
if (initialize || !exists("ORATS_core") || !all(cols_to_extract %in% names(ORATS_core))) {
    # no cache yet (first run, or a renamed file): the missing signature file
    # below sends this down the full-rebuild path
    ORATS_core <- if (file.exists(ORATS_core_file)) load_ORATS_core(ORATS_core_file) else NULL
    files <- list.files(core_dir, "orats_core_202[0-9].*\\.csv\\.gz")
    last_real_day <- max(as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1",
                                     basename(files)), format = "%Y%m%d"))
    loaded_last_day <- if (is.null(ORATS_core)) as.Date("1900-01-01")
                       else max(as.Date(ORATS_core$tradeDate))
    delayed_is_new <- file.exists(ORATS_code_delayed_file) &&
        as.Date(file.mtime(ORATS_code_delayed_file)) > loaded_last_day
    # min_days belongs in the signature for the same reason the universe and
    # the column set do: loosening it admits tickers the cache has no rows
    # for, and appending days cannot backfill them.
    core_sig <- list(universe = sort(unique(combined_screener$Symbol)),
                     cols = sort(cols_to_extract),
                     min_days = min_days)
    universe_grew <- universe_changed(ORATS_universe_file, core_sig)
    if (initialize || universe_grew || loaded_last_day < last_real_day || delayed_is_new) {
        if (initialize || universe_grew) {
            # appending days would only extend the tickers already cached, so
            # re-derive the whole thing from the raw core files (~2-10 min)
            print("ORATS_core universe changed - rebuilding from the core files")
            ORATS_core <- create_ORATS_core(core_dir)
        } else {
            print(paste("Updating ORATS_core:", loaded_last_day, "->", last_real_day))
            ORATS_core <- ORATS_core %>% ungroup() %>%
                dplyr::select(any_of(cols_to_extract))
            ORATS_core <- update_ORATS_core(ORATS_core, core_dir)
        }
        ORATS_core <- append_ORATS_delayed(ORATS_core, ORATS_code_delayed_file)
        ORATS_core <- init_ORATS_core(ORATS_core, screener = combined_screener)
        write_parquet(ORATS_core %>% dplyr::filter(as.Date(tradeDate) <= last_real_day),
                      ORATS_core_file)
        save_universe(ORATS_universe_file, core_sig)
    }
    print(paste("ORATS_core ready, last day:", max(as.Date(ORATS_core$tradeDate))))
}

# OHLC for the realized-vol estimators. First run seeds the store (~30s);
# afterwards it is an append of whatever days are new.
if (initialize || !exists("ORATS_ohlc") || !all(ohlc_cols %in% names(ORATS_ohlc))) {
    ORATS_ohlc <- update_ORATS_ohlc(unique(ORATS_core$ticker))
    print(paste("ORATS_ohlc ready, last day:", max(ORATS_ohlc$tradeDate)))
}

# The CBOE volatility index complex. Unlike the ORATS stores this is a single
# ~2 MB pull that CBOE refreshes nightly, so there is no append logic - the
# cache is rebuilt whole once a day and reused for the rest of the session.
if (initialize || !exists("vix_complex")) {
    vix_complex <- update_vix_complex(paste0(options_dir, "vix_complex.pq"),
                                      force = initialize)
    vix_derived <- build_vix_derived(vix_complex)
    print(paste0("VIX complex ready, last day: ", max(vix_complex$date),
                 " (dropped ", vix_holiday_rows(vix_complex),
                 " CBOE holiday rows the rest of the complex does not have)"))
}

# The Dashboard filters on an exact tradeDate, so its date box has to default
# to the last day in the data rather than the calendar date: on any weekend,
# holiday, or morning before the file lands, Sys.Date() matches nothing and
# every plot on the tab renders empty with no explanation.
last_data_day <- max(as.Date(ORATS_core$tradeDate))

# what the free-form screen scatter can plot against what
screen_vars <- c("richScore", "vrpScore", "q_ivrv", "q_vov", "q_ffr", "ivrvNow", "fwdRatio", "move1mSig",
                 "ivrv_z", "ivrv_pct", "iv_ivvol_z", "iv_ivvol_pct", "ff_30_60", "ff_30_90", "ff_60_90", "ff_90_180", "back_pct_60", "back_pct_90", "back_pct_180",
                 "stkPxChng1wk", "stkPxChng1m", "stkPxChng6m",
                 "straRich", "ivFcstRatio",
                 "ivPctile1y", "rvPctile1y", "ivPctileSpy", "VRPzscore",
                 "steepness_30d6m", "ivHvXernRatio", "slope", "contango",
                 "mktWidthVol", "confidence", "correlSpy1m",
                 "volOfVol", "volOfIvol", "avgOptVolu20d",
                 "exErnIv30d", "clsHvXern20d",
                 "daysSinceErn", "absAvgErnMv", "impliedIee",
                 "rv5_20", "rv20_252", "iv_z", "skewPct", "termPct")

# ---- CHAIN BEGIN ----
# Chain tab: the live strikes chain from the ORATS delayed feed.
#
# Everything else in this app is built on the core file - one row per ticker
# per day - which cannot see WHERE on the curve a premium sits. The questions
# that actually decide a trade ("is the 96th-percentile IV an event two
# expiries out or a regime bid?", "is the half-spread bigger than the edge?",
# "what is this strangle worth at the realized vol?") need the per-expiry,
# per-strike chain. This block fetches it on demand and derives three views:
# the expiry strip (ATM IV, forward vol, straddle %, half-spread %), the
# strike ladder (skew and open interest), and a structure pricer with a
# fair-value grid across realized-vol assumptions.
#
# Same endpoint and token as orats_delayed.R. Cached per ticker for a few
# minutes so switching expiries or strikes does not re-hit the API.

ORATS_TOKEN <- local({
    src <- paste0(orats_dir, "orats_delayed.R")
    if (!file.exists(src)) return(NA_character_)
    m <- regmatches(readLines(src, warn = FALSE), regexpr("token=[0-9a-f-]+", readLines(src, warn = FALSE)))
    if (!length(m)) NA_character_ else sub("token=", "", m[1])
})
CHAIN_CACHE_MIN <- 10
chain_cache <- new.env(parent = emptyenv())

# A JSON field ORATS omits for a quote-less strike comes back as NULL through
# fromJSON; every numeric column is coerced so downstream arithmetic never
# sees a list column.
chain_num_cols <- c("dte", "strike", "stockPrice", "callVolume", "callOpenInterest",
                    "putVolume", "putOpenInterest", "callBidPrice", "callValue",
                    "callAskPrice", "putBidPrice", "putValue", "putAskPrice",
                    "callMidIv", "putMidIv", "delta", "gamma", "theta", "vega")

fetch_chain <- function(ticker, force = FALSE) {
    key <- toupper(trimws(ticker))
    hit <- chain_cache[[key]]
    if (!force && !is.null(hit) &&
        difftime(Sys.time(), hit$fetched, units = "mins") < CHAIN_CACHE_MIN)
        return(hit$df)
    if (is.na(ORATS_TOKEN)) stop("no ORATS token found in orats_delayed.R")
    url <- sprintf("https://api.orats.io/datav2/strikes?token=%s&ticker=%s", ORATS_TOKEN, key)
    txt <- RCurl::getURL(url, .opts = list(followlocation = TRUE, timeout = 90))
    js  <- jsonlite::fromJSON(txt)
    if (is.null(js$data) || !length(js$data)) stop(paste("ORATS returned no chain for", key))
    df <- as_tibble(js$data)
    for (cc in intersect(chain_num_cols, names(df))) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
    df <- df %>%
        mutate(expirDate = as.Date(expirDate),
               quoteTime = suppressWarnings(as.POSIXct(quoteDate, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")),
               callMid = (callBidPrice + callAskPrice) / 2,
               putMid  = (putBidPrice + putAskPrice) / 2,
               # ORATS' delta is the call delta; the put's is delta - 1
               putDelta = delta - 1) %>%
        arrange(expirDate, strike)
    chain_cache[[key]] <- list(df = df, fetched = Sys.time())
    df
}

# One row per expiry: the ATM strike (|delta - 0.5| minimal), its straddle at
# mid and at the touch, and the forward vol implied between this expiry and
# the previous one. A kink in fwd_iv is an event; a flat strip at a high
# level is a regime. hs_pct is the half-spread as a percent of the straddle
# premium - the number that decides whether an edge survives the fill.
expiry_strip <- function(ch) {
    ch %>%
        dplyr::filter(is.finite(callMidIv), is.finite(putMidIv), callMidIv > 0, putMidIv > 0,
                      callBidPrice > 0, putBidPrice > 0) %>%
        group_by(expirDate, dte) %>%
        slice_min(abs(delta - 0.5), n = 1, with_ties = FALSE) %>%
        ungroup() %>% arrange(dte) %>%
        mutate(spot   = stockPrice,
               atm_iv = (callMidIv + putMidIv) / 2,
               T      = dte / 365,
               strad_bid = callBidPrice + putBidPrice,
               strad_ask = callAskPrice + putAskPrice,
               strad_mid = (strad_bid + strad_ask) / 2,
               strad_pct = 100 * strad_mid / spot,
               hs_pct    = 100 * (strad_ask - strad_mid) / strad_mid,
               fwd_var   = (atm_iv^2 * T - lag(atm_iv)^2 * lag(T)) / (T - lag(T)),
               fwd_iv    = sqrt(pmax(fwd_var, 0)),
               oi  = callOpenInterest + putOpenInterest,
               vol = callVolume + putVolume,
               label = paste0(format(expirDate, "%b %d"), " (", dte, "d)")) %>%
        dplyr::select(expirDate, dte, label, spot, strike, atm_iv, fwd_iv, strad_bid, strad_mid,
                      strad_ask, strad_pct, hs_pct, oi, vol, callOpenInterest, putOpenInterest)
}

# Black-Scholes, one leg. cp = "call" | "put".
bs_price <- function(S, K, T, r, sig, cp) {
    if (T <= 0 || sig <= 0) return(if (cp == "call") max(S - K, 0) else max(K - S, 0))
    d1 <- (log(S / K) + (r + sig^2 / 2) * T) / (sig * sqrt(T)); d2 <- d1 - sig * sqrt(T)
    if (cp == "call") S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
    else              K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
}

# Legs of a structure as (cp, strike, pos) with pos = +1 long / -1 short for
# the SELL side; the BUY side flips every sign. k1 <= k2 by convention:
# strangle = put k1 / call k2; call vertical sold = short k1 / long k2 (bear
# call); put vertical sold = short k2 / long k1 (bull put).
structure_legs <- function(structure, side, k1, k2) {
    legs <- switch(structure,
        "Straddle"      = tibble(cp = c("call", "put"), strike = c(k1, k1), pos = c(-1, -1)),
        "Strangle"      = tibble(cp = c("put", "call"), strike = c(k1, k2), pos = c(-1, -1)),
        "Call vertical" = tibble(cp = c("call", "call"), strike = c(k1, k2), pos = c(-1, +1)),
        "Put vertical"  = tibble(cp = c("put", "put"),  strike = c(k2, k1), pos = c(-1, +1)))
    if (side == "Buy") legs$pos <- -legs$pos
    legs
}

# Join the legs to the chain rows for one expiry and price the structure at
# mid, at the touch (natural: short legs at the bid, long legs at the ask),
# and at ORATS' model value. Net premium is signed from the trader's side:
# a credit is positive when selling, a debit is positive when buying.
price_structure <- function(ch_exp, legs, side) {
    out <- vector("list", nrow(legs))
    for (i in seq_len(nrow(legs))) {
        r <- ch_exp[ch_exp$strike == legs$strike[i], ]
        if (!nrow(r)) return(NULL)
        r <- r[1, ]; call <- legs$cp[i] == "call"
        out[[i]] <- tibble(
            leg    = paste(if (legs$pos[i] < 0) "Short" else "Long", legs$strike[i], legs$cp[i]),
            cp     = legs$cp[i], strike = legs$strike[i], pos = legs$pos[i],
            bid    = if (call) r$callBidPrice else r$putBidPrice,
            ask    = if (call) r$callAskPrice else r$putAskPrice,
            model  = if (call) r$callValue    else r$putValue,
            iv     = if (call) r$callMidIv    else r$putMidIv,
            delta  = if (call) r$delta        else r$putDelta,
            gamma  = r$gamma, theta = r$theta, vega = r$vega,
            oi     = if (call) r$callOpenInterest else r$putOpenInterest)
    }
    rows <- bind_rows(out) %>%
        mutate(mid = (bid + ask) / 2, touch = if_else(pos < 0, bid, ask))
    sgn <- if (side == "Sell") -1 else 1
    net <- function(px) sgn * sum(rows$pos * px)
    list(legs = rows, credit_side = side == "Sell",
         mid = net(rows$mid), touch = net(rows$touch), model = net(rows$model),
         delta = sum(rows$pos * rows$delta), gamma = sum(rows$pos * rows$gamma),
         theta = sum(rows$pos * rows$theta), vega  = sum(rows$pos * rows$vega))
}

# The structure's Black-Scholes value under several vol assumptions - the
# realized vols from the core file, the midpoint, and the implied itself. For
# a credit structure: post at mid; the value at the midpoint vol is the floor;
# the value at HV252 is where the edge against a year of realized is gone.
fair_grid <- function(legs, S, T, vols, r = 0.04, credit_side = TRUE) {
    sgn <- if (credit_side) -1 else 1
    tibble(assumption = names(vols), vol = unname(vols)) %>%
        dplyr::filter(is.finite(vol), vol > 0) %>%
        rowwise() %>%
        mutate(value = sgn * sum(legs$pos * mapply(function(cp, K) bs_price(S, K, T, r, vol, cp),
                                                  legs$cp, legs$strike))) %>%
        ungroup()
}

# Reg-T sketch for the size line. Short straddle/strangle: 20% of spot plus
# the premium; vertical: width less credit; long anything: the debit.
approx_margin <- function(structure, side, k1, k2, S, net_premium) {
    if (side == "Buy") return(net_premium * 100)
    if (structure %in% c("Straddle", "Strangle")) return((0.20 * S + net_premium) * 100)
    (abs(k2 - k1) - net_premium) * 100
}

# The discount rate the chain block reasons at. A vertical's value is a
# DISCOUNTED probability, so anything read as a probability is divided by
# this factor before it is shown. At 30 days it moves the number by 0.3%; at
# a year it moves it by 4%, which is enough to make the put and call sides of
# the same strike look inconsistent when they are not. Matches fair_grid's r.
CHAIN_RFR <- 0.04

# A vertical spread's price is an odds statement. Its value divided by the
# distance between the strikes is the market's probability that the stock
# finishes past the MIDPOINT of the two strikes - not past the short strike,
# which is the usual misreading - on the side the long vertical wins. So a
# $15-wide put spread worth $5.17 is 1.9-to-1 on the stock below the midpoint.
# This is the form that decides whether you want the bet, because it is
# directly comparable to your own view; the fair-value grid answers the other
# question (is it cheap against realized vol).
#
# price_structure returns the value of the vertical the LONG side holds - the
# bull call for a call vertical, the bear put for a put vertical - whichever
# side the user picked, so the probability is always value/width and only the
# direction depends on the structure.
#
# value is the market value of that vertical (pr$mid or pr$touch).
vertical_odds <- function(structure, side, k1, k2, value, T) {
    W <- abs(k2 - k1)
    if (!is.finite(value) || !is.finite(W) || W <= 0) return(NULL)
    df <- exp(-CHAIN_RFR * T)
    p_long <- (value / W) / df            # P(the long vertical's side wins)
    # the direction the LONG vertical needs; the seller is betting the other way
    dir_long <- if (structure == "Call vertical") "above" else "below"
    buying <- side == "Buy"
    list(width = W, mid_strike = (k1 + k2) / 2,
         risk  = if (buying) value else W - value,
         win   = if (buying) W - value else value,
         # the event the trader at this side is betting ON, and its implied odds
         dir   = if (buying) dir_long else if (dir_long == "above") "below" else "above",
         p     = if (buying) p_long else 1 - p_long,
         # a vertical outside (0, width) is not a bet, it is a bad quote
         ok    = is.finite(p_long) && p_long > 0 && p_long < 1)
}

# The same identity read across the whole ladder, which turns it into a
# no-arbitrage check on the quotes. The adjacent put spread gives
# P(S < midpoint) and the adjacent call spread gives P(S > midpoint); each
# must sit in [0,1], the put side must rise with the strike, and the two must
# sum to 1. The change in that probability from one midpoint to the next is
# the implied density (the butterfly) and cannot be negative.
#
# A violation is never a trade. It is a crossed, stale or quote-less strike -
# the butterfly that prices below zero is the giveaway - and it says the
# pricer's numbers on the strikes it touches are not to be trusted. On SPY it
# fires never; on a thin single name it fires constantly, which is the point.
#
# Returns one row per strike. Each adjacent-spread number is attached to its
# LOWER strike, which is the row you are on when you consider that vertical;
# the density is attached to the middle strike of its butterfly. Computed on
# the FULL expiry, never on a delta-filtered slice, or the tails are cut off
# and the edge strikes are flagged for nothing.
PC_TOL <- 0.05

ladder_checks <- function(ce) {
    d <- ce %>% arrange(strike) %>%
        dplyr::filter(is.finite(strike), is.finite(putMid), is.finite(callMid),
                      putMid > 0, callMid > 0)
    n <- nrow(d)
    if (n < 3) return(NULL)
    disc <- exp(-CHAIN_RFR * d$dte[1] / 365)
    dK   <- diff(d$strike)
    mids <- (d$strike[-n] + d$strike[-1]) / 2
    p_put  <-  (diff(d$putMid)  / dK) / disc     # P(S < midpoint), put side
    p_call <- -(diff(d$callMid) / dK) / disc     # P(S > midpoint), call side
    # implied density at the middle strike of each butterfly, per unit strike
    dens <- c(NA_real_, diff(p_put) / diff(mids), NA_real_)
    tibble(strike = d$strike,
           spread_mid = c(mids, NA_real_),
           p_below = c(p_put, NA_real_),
           p_above = c(p_call, NA_real_),
           density = dens) %>%
        mutate(flag = trimws(paste0(
            ifelse(is.finite(p_below) & (p_below < 0 | p_below > 1), "put spread ", ""),
            ifelse(is.finite(p_above) & (p_above < 0 | p_above > 1), "call spread ", ""),
            ifelse(is.finite(density) & density < 0, "butterfly ", ""),
            ifelse(is.finite(p_below) & is.finite(p_above) &
                   abs(p_below + p_above - 1) > PC_TOL, "put/call ", ""))))
}

# ---- CHAIN END ----

# The shinyapp
ui <- fillPage(
    tags$head(
        tags$style(HTML(
            "html, body, .container-fluid {height:100%;}
      .sidebar {height:100vh; overflow:auto;}
      .main {height:100vh; overflow:auto;}
      .shiny-title-output {margin-top:8px;}"
        ))
    ),
    tabsetPanel(
        id = "tabs",
        tabPanel("Dashboard",
                 div(class = "container-fluid",
                     fluidRow(
                         column(width = 2, class = "sidebar",
                                wellPanel(
                                    h3("Candidates"),
                                    radioButtons("cand_stat", "Statistic",
                                                 choices = c("Percentile" = "pct", "Z-score" = "z"),
                                                 selected = "pct", inline = TRUE),
                                    selectInput("norm_lookback", "Lookback (sessions) - IV/RV rank, IV z, skew/term pct",
                                                choices = c(63, 126, 252, 500), selected = 252),
                                    # what the markers are coloured by; the skew and term
                                    # percentiles rank each name against its own history
                                    selectInput("cand_colour", "Candidates colour",
                                                choices = c("vrp_etf_v2 score" = "vrpScore",
                                                            "Put skew percentile (25d RR / ATM)" = "skewPct",
                                                            "Term percentile (30d / 90d IV)" = "termPct"),
                                                selected = "vrpScore"),
                                    textInput("cand_n_labels", "Tickers to label", value = 12),
                                    checkboxInput("cand_hide_wide", "Hide wide markets (width > 2x median)", value = FALSE),
                                    h3("Screen"),
                                    # single names dominate option volume, so
                                    # without this every top-N list is stocks
                                    selectInput("dash_universe", "Universe",
                                                choices = c("ETFs", "Single names", "All"),
                                                selected = "ETFs"),
                                    dateInput("date", "Date",
                                              value = last_data_day,
                                              format = "yyyy-mm-dd"
                                    ),
                                    selectInput("scr_xvar", "Screen X", choices = screen_vars,
                                                selected = "ivPctile1y"),
                                    selectInput("scr_yvar", "Screen Y", choices = screen_vars,
                                                selected = "VRPzscore"),
                                    textInput("scr_n_tickers", "Tickers to show", value = 50),
                                    # a floor screens on liquidity itself rather
                                    # than just taking the N most liquid; median
                                    # avgOptVolu20d is ~100, p90 ~4800
                                    textInput("scr_min_volu", "Min avg opt volume (20d)", value = 0),
                                    # bid/ask width in vol points; median is
                                    # ~6.8, so 0 (off) or something above that
                                    textInput("scr_max_width", "Max spread (vol pts, 0 = off)", value = 0),

                                    h3("Tables"),
                                    selectInput("fd_ratio", "Term structure change",
                                                choices = c("Clicks", "Ratio"), selected = "Clicks"),
                                    textInput("fd_n_tickers", "Tickers to show", value = 50)

                                )
                         ),
                         column(width = 10, class = "main",
                                div(style = "height:100vh; display:flex; flex-direction:column;",
                                    div(style = "flex: 1 1 auto; overflow:auto; padding: 8px;",
                                        htmlOutput("data_freshness"),
                                        h1("Candidates", style = "color: darkgray;"),
                                        helpText("Both panels: log(IV30 / RV20) ranked against each name's own history (y, ",
                                                 "lookback in the sidebar) - the same IV/RV as the Ticker tab. Left x: where ",
                                                 "IV itself sits (ORATS ivPctile1y, or IV's z over the lookback) - is vol rich ",
                                                 "AND high? Right x: RV5/RV20 on ORATS' intraday HV - will it hurt before it ",
                                                 "pays? Above ~1.4 realized tends to keep rising over the next 20 sessions ",
                                                 "(vrp_etf_v2: t -5.3), so expect early marks against - but it does NOT predict ",
                                                 "the P&L of a straddle held to expiry (IC ~0 on 8,213 real ETF trades), so it ",
                                                 "is a path-risk gauge for short holds and stops, not a do-not-sell. Colour is ",
                                                 "the vrp_etf_v2 score (mean rank of IV/RV, vol-of-vol ",
                                                 "and 30/60 backwardation within this universe and day); size is option volume. ",
                                                 "Click a point or a row to open it in the Chain tab."),
                                        fluidRow(
                                            column(6, plotlyOutput("cand_plot_left", height = "600px")),
                                            column(6, plotlyOutput("cand_plot_right", height = "600px"))
                                        ),
                                        DTOutput("cand_tbl"),
                                        h1("Screen", style = "color: darkgray;"),
                                        helpText("Any column against any other, coloured by the vrp score; the top of the score ",
                                                 "and the 5% tails on either axis are labelled. Click a point to open that ticker in the Ticker tab."),
                                        plotlyOutput("screen_plot", height = "600px"),
                                        h1("Term structure change", style = "color: darkgray;"),
                                        DTOutput("strike_plot", height = "calc(100vh - 200px)")
                                    )
                                )
                         )
                         
                     )
                 )
        )
        ,

        tabPanel("Ticker",
                 fluidRow(
                     column(12,
                            column(width = 2, class = "sidebar",
                                   wellPanel(
                                       h3("Ticker"),
                                       textInput("t_ticker", "Ticker", value = "SPY"),
                                       textInput("t_dte", "DTE", value = "25"),
                                       selectInput("t_vol_window", "Volatility Window", choices = c("30d", "60d", "90d", "6m", "1yr")),
                                       selectInput("t_profit", "Profit", choices = c("Percentage", "Dollars")),
                                       # Display window for the time-series plots. Rolling stats are
                                       # always computed on the full history; this only trims what is
                                       # drawn (and the lookback of the cones / percentile ribbons).
                                       selectInput("t_range", "Time Range",
                                                   choices = c("3m" = 3, "6m" = 6, "1y" = 12,
                                                               "2y" = 24, "Max" = 0),
                                                   selected = 12)

                                   )
                            ),
                            column(width = 10, class = "main",
                                   div(style = "height:100vh; display:flex; flex-direction:column;",
                                       div(style = "flex: 1 1 auto; overflow:auto; padding: 8px;",
                                           #plotlyOutput("ticker_plot", height = "calc(100vh - 200px)")

                                           plotlyOutput("ticker_plot_1"),
                                           # IV vs ORATS RV on the left, the same IV against the
                                           # OHLC range estimators on the right
                                           fluidRow(
                                               column(6, plotlyOutput("ticker_plot_2")),
                                               column(6, plotlyOutput("ticker_plot_rv_est"))
                                           ),
                                           plotlyOutput("ticker_plot_regime"),
                                           plotlyOutput("ticker_plot_3"),
                                           plotlyOutput("ticker_plot_4"),
                                           plotlyOutput("ticker_plot_5"),
                                           plotlyOutput("ticker_plot_6"),
                                           plotlyOutput("ticker_plot_7"),
                                           plotlyOutput("ticker_plot_surface", height = "460px"),
                                           plotlyOutput("ticker_plot_skew", height = "560px")
                                       )
                                   )
                            )
                     )
                 )
        )
        ,
        tabPanel("Chain",
                 div(class = "container-fluid",
                     fluidRow(
                         column(width = 2, class = "sidebar",
                                wellPanel(
                                    h3("Chain"),
                                    textInput("c_ticker", "Ticker", value = "SPY"),
                                    actionButton("c_refresh", "Refresh quotes"),
                                    helpText("ORATS delayed feed; cached 10 min per ticker."),
                                    # populated from the chain once it is fetched
                                    selectInput("c_expiry", "Expiry", choices = character(0)),
                                    sliderInput("c_delta_rng", "Ladder delta range",
                                                min = 0.02, max = 0.98, value = c(0.05, 0.95), step = 0.01),
                                    h3("Pricer"),
                                    selectInput("c_structure", "Structure",
                                                choices = c("Strangle", "Straddle", "Call vertical", "Put vertical")),
                                    selectInput("c_side", "Side", choices = c("Sell", "Buy")),
                                    selectInput("c_k1", "Strike 1 (lower)", choices = character(0)),
                                    selectInput("c_k2", "Strike 2 (upper)", choices = character(0)),
                                    helpText("Strangle: put K1 / call K2. Verticals: K1 < K2; ",
                                             "selling a call vertical is the bear call (short K1), ",
                                             "selling a put vertical is the bull put (short K2). ",
                                             "Straddle uses K1 only."),
                                    numericInput("c_qty", "Contracts", value = 1, min = 1, step = 1)
                                )
                         ),
                         column(width = 10, class = "main",
                                div(style = "height:100vh; display:flex; flex-direction:column;",
                                    div(style = "flex: 1 1 auto; overflow:auto; padding: 8px;",
                                        div(style = "background:#fff3cd; border:1px solid #ffe69c; color:#664d03; padding:10px 14px; margin:8px 0 12px 0; border-radius:4px;",
                                            tags$b("This tab calls the ORATS API."),
                                            " Every new ticker - typed here or clicked through from the Dashboard - downloads that",
                                            " name's full strikes chain (0.3-5 MB) from the delayed feed, and so does the ",
                                            tags$b("Refresh quotes"), " button. Changing expiry, strikes or structure does not:",
                                            " the chain is cached in memory for 10 minutes per ticker, after which the next",
                                            " interaction fetches it again. Nothing is written to disk."),
                                        h1("Expiry strip", style = "color: darkgray;"),
                                        htmlOutput("chain_status"),
                                        helpText("ATM IV per expiry with the FORWARD vol implied between ",
                                                 "adjacent expiries: a spike in the forward is an event ",
                                                 "priced into that week, a flat strip at a high level is a ",
                                                 "regime. Dashed lines are the realized vols from the core ",
                                                 "file. Half-spread is the ATM straddle's bid/ask half-width ",
                                                 "as a percent of its premium - compare it to the edge."),
                                        plotlyOutput("chain_strip_plot", height = "720px"),
                                        DTOutput("chain_strip_tbl"),
                                        h1("Strike ladder", style = "color: darkgray;"),
                                        helpText("Skew (call and put mid IV by strike) over open interest ",
                                                 "(calls up, puts down) for the selected expiry. Vertical ",
                                                 "line is spot."),
                                        plotlyOutput("chain_ladder_plot", height = "640px"),
                                        helpText("P(<mid) and P(>mid) are read off the adjacent verticals ",
                                                 "themselves: a spread's value over the strike distance is the ",
                                                 "market's probability of finishing past the MIDPOINT of the two ",
                                                 "strikes, so each row is the odds on the spread that starts at ",
                                                 "that strike. The two sides must sum to 1 and the density (the ",
                                                 "butterfly) must not be negative - a flagged row is a crossed or ",
                                                 "stale quote, not an opportunity. Single names are American and ",
                                                 "pay dividends, so read the probabilities as approximations that ",
                                                 "degrade deep in the money; wide strikes give a coarse density."),
                                        htmlOutput("chain_ladder_checks"),
                                        DTOutput("chain_ladder_tbl"),
                                        h1("Structure pricer", style = "color: darkgray;"),
                                        htmlOutput("chain_pricer_summary"),
                                        DTOutput("chain_pricer_legs"),
                                        plotlyOutput("chain_payoff_plot", height = "460px"),
                                        h3("Fair value across vol assumptions", style = "color: darkgray;"),
                                        helpText("Black-Scholes value of the structure at the core file's ",
                                                 "realized vols, at the midpoint between HV252 and the ",
                                                 "implied, and at the implied itself. For a credit: post at ",
                                                 "mid, the midpoint row is the floor, the HV252 row is where ",
                                                 "the edge against a year of realized is gone."),
                                        DTOutput("chain_fair_tbl")
                                    )
                                )
                         )
                     )
                 )
        )
        ,
        tabPanel("Pairs",
                 div(class = "container-fluid",
                     fluidRow(
                         column(width = 2, class = "sidebar",
                                wellPanel(
                                    h3("Ticker Pairs"),
                                    textInput("ticker_1", "First Ticker", value = "SPY"),
                                    textInput("ticker_2", "Second Ticker", value = "QQQ"),
                                    textInput("run_window", "Running Windows", value = 60)
                                )
                         ),
                         column(width = 10, class = "main",
                                div(style = "height:100vh; display:flex; flex-direction:column;",
                                    div(style = "flex: 1 1 auto; overflow:auto; padding: 8px;",
                                        plotOutput("pairs_plot", height = "calc(200vh - 200px)"),
                                        plotOutput("corr_plot", height = "calc(100vh - 100px)")
                                    )
                                )
                         )

                     )
                 )
        )
        ,
        # The VIX tab is a monitor, not a screener: it answers "what is out of
        # line today" for the index vol complex, in the shape Sinclair uses -
        # ratios rather than levels, each ranked against its own history.
        tabPanel("VIX",
                 div(class = "container-fluid",
                     fluidRow(
                         column(width = 2, class = "sidebar",
                                wellPanel(
                                    h3("VIX complex"),
                                    selectInput("v_range", "Time Range",
                                                choices = c("6m" = 6, "1y" = 12, "2y" = 24,
                                                            "5y" = 60, "Max" = 0),
                                                selected = 24),
                                    # a level means nothing without its own history to
                                    # rank it against; this is that history
                                    selectInput("v_lookback", "Rank / z-score lookback",
                                                choices = c("6m" = 126, "1y" = 252,
                                                            "2y" = 504, "3y" = 756),
                                                selected = 252),
                                    checkboxInput("v_log", "Log scale on levels", TRUE),
                                    hr(),
                                    h4("Fixed-strike vol"),
                                    selectInput("v_fs_dte", "Target DTE",
                                                choices = c(7, 14, 30, 60, 90),
                                                selected = 30),
                                    hr(),
                                    h4("Flow-spike screen"),
                                    helpText("Days when the VIX jumped and the underlying did not.",
                                             "The control - a jump WITH a spot move - is drawn",
                                             "alongside, so the flagged set is never read alone."),
                                    sliderInput("v_spike_z", "min VIX move (z)",
                                                min = 0.5, max = 4, value = 1.5, step = 0.25),
                                    sliderInput("v_spike_mv", "max |SPX move| (%)",
                                                min = 0.1, max = 2, value = 0.5, step = 0.1)
                                )
                         ),
                         column(width = 10, class = "main",
                                div(style = "height:100vh; display:flex; flex-direction:column;",
                                    div(style = "flex: 1 1 auto; overflow:auto; padding: 8px;",
                                        # The monitor table is five columns wide and
                                        # twenty rows tall, so on its own it leaves the
                                        # right half of the page empty. The curve is the
                                        # one plot that does not want full width - six
                                        # categories stretched across a page reads worse,
                                        # not better - so the two share the row and
                                        # everything else runs the full width.
                                        fluidRow(
                                            column(5,
                                                   h4("Where the complex sits today"),
                                                   DT::dataTableOutput("vix_tbl_monitor")),
                                            column(7, plotlyOutput("vix_plot_curve",
                                                                   height = "520px"))
                                        ),
                                        br(),
                                        plotlyOutput("vix_plot_xasset", height = "640px"),
                                        plotlyOutput("vix_plot_ratios", height = "520px"),
                                        plotlyOutput("vix_plot_cross",  height = "640px"),
                                        plotlyOutput("vix_plot_corr",   height = "460px"),
                                        plotlyOutput("vix_plot_fixed",  height = "520px"),
                                        plotlyOutput("vix_plot_spxvix", height = "360px"),
                                        plotlyOutput("vix_plot_spike",  height = "480px")
                                    )
                                )
                         )
                     )
                 )
        )
    )
)

# ---- Server ----
server <- function(input, output, session) {

    # ---- Ticker tab: one shared, debounced ticker slice ----
    # debounce: typing "NVDA" fires once, not once per keystroke;
    # shared reactive: the big frame is filtered once per ticker change
    # instead of once per plot.
    t_ticker_deb <- debounce(
        reactive(toupper(trimws(as.character(input$t_ticker)))), 600)
    ticker_df <- reactive({
        req(nzchar(t_ticker_deb()))
        df <- ORATS_core %>% dplyr::filter(ticker == t_ticker_deb()) %>%
            ungroup() %>% arrange(tradeDate)
        req(nrow(df) > 0)
        # ORATS sometimes publishes 0 rather than NA for a missing ex-earnings
        # IV (QQQ's exErnIv1yr is 0 for its last 25 days). Plotted as-is the
        # IV line drops to the axis, and log(0/x) poisons the VRP panels, so
        # treat a zero IV as the missing value it actually is.
        df <- df %>% mutate(across(c(exErnIv10d:exErnIv1yr, starts_with("exErnDlt"),
                                     starts_with("clsHvXern")), ~ na_if(.x, 0)))
        # pxAtmIv is the unadjusted price: a 2-for-1 reads as a -69% day.
        # pxAdj is back-adjusted for returns; plots of the price keep pxAtmIv.
        df$pxAdj <- split_adjust(df$tradeDate, na_if(df$pxAtmIv, 0), t_ticker_deb())
        df
    })

    # ---- Ticker tab: display time range ----
    # t_range is a number of months ("Max" = 0 = whole loaded history).
    # Rolling quantities (252d percent ranks, z-scores, lags, EMAs, cumulative
    # PnL) stay computed on the full ticker history; t_win() is applied only
    # to what a plot draws, so a 3m window never truncates a 252d lookback.
    t_range_months <- reactive({
        m <- suppressWarnings(as.numeric(input$t_range))
        if (length(m) != 1 || is.na(m) || m <= 0) Inf else m
    })
    ticker_end <- reactive(max(ticker_df()$tradeDate, na.rm = TRUE))

    # ---- Dashboard: which universe the top-N lists rank over ----
    # class comes from the screener join: single names are "Stock", everything
    # else is an ETF asset class (Equity, Fixed Income, Commodity, ...).
    # Defaults to ETFs, which is what this tab ranked before single names
    # were added.
    # an empty day should say so, not render a blank panel
    need_rows <- function(df) {
        validate(need(nrow(df) > 0,
                      paste0("No data for ", format(input$date),
                             ". Last day in the data is ", format(last_data_day), ".")))
        df
    }

    # The day's slice, most liquid first. validate() rather than req() so an
    # empty day says so on the page instead of rendering a blank panel.
    dash_screen <- function(n) {
        floor_volu <- suppressWarnings(as.numeric(trimws(input$scr_min_volu)))
        if (!length(floor_volu) || is.na(floor_volu)) floor_volu <- 0
        df <- dash_core() %>% dplyr::ungroup() %>%
            dplyr::filter(tradeDate == input$date) %>%
            arrange(desc(avgOptVolu20d))
        validate(need(nrow(df) > 0,
                      paste0("No data for ", format(input$date),
                             ". Last day in the data is ", format(last_data_day), ".")))
        df <- df %>% dplyr::filter(avgOptVolu20d >= floor_volu)
        validate(need(nrow(df) > 0,
                      paste0("No ticker in this universe trades ", floor_volu,
                             " contracts a day. Lower the volume floor.")))

        max_width <- suppressWarnings(as.numeric(trimws(input$scr_max_width)))
        if (length(max_width) && !is.na(max_width) && max_width > 0) {
            df <- df %>% dplyr::filter(is.finite(mktWidthVol), mktWidthVol <= max_width)
            validate(need(nrow(df) > 0,
                          paste0("Nothing quotes tighter than ", max_width,
                                 " vol points here. Raise the spread cap.")))
        }

        # liquidity gate first, then rank within what survived
        if (is.finite(n) && n > 0) df <- head(df, n = n)

        # Richness score: where a name sits, 0-100, across axes that are not
        # rotations of one another - what the straddle costs vs forecast, IV vs
        # forecast, the realized VRP z-score, and IV's own 1y percentile. High
        # = vol looks rich here, low = cheap. Ranked inside the current
        # universe and day, so it answers "relative to what I am screening",
        # not against some absolute scale. Tradability is deliberately NOT in
        # the score - a wide market is a reason to exclude a name, not a
        # reason to call it interesting, so it filters above instead.
        pct <- function(z) {
            z[!is.finite(z)] <- NA
            n_ok <- sum(!is.na(z))
            if (n_ok < 2) return(rep(NA_real_, length(z)))
            100 * (rank(z, na.last = "keep", ties.method = "average") - 0.5) / n_ok
        }
        df %>% mutate(
            straRich    = straPxM1 / fcstStraPxM1,
            ivFcstRatio = exErnIv30d / orIvFcst20d,
            richScore   = rowMeans(cbind(pct(straRich), pct(ivFcstRatio),
                                         pct(VRPzscore), pct(ivPctile1y)),
                                   na.rm = TRUE)
        ) %>% arrange(desc(richScore)) %>%
        # vrp_etf_v2's three signals, computed here from the same core fields
        # the tested screener ranks on (ex-earnings variants; identical for
        # ETFs): IV rich vs the name's own realized, vol-of-vol, and the
        # 30/60 flat-vs-standard forward ratio (low = front rich =
        # backwardation). Ranked within the current universe and day exactly
        # like richScore, so the two are comparable on the same 0-1 scale.
        mutate(
            ivrvNow  = log(na_if(exErnIv30d, 0) / na_if(clsHvXern20d, 0)),
            fwdRatio = {
                v1 <- na_if(exErnIv30d, 0); v2 <- na_if(exErnIv60d, 0)
                fstd  <- sqrt(pmax((v2^2 * 60 - v1^2 * 30) / 30, 1e-6))
                fflat <- (v2 * sqrt(60) - v1 * sqrt(30)) / (sqrt(60) - sqrt(30))
                pmin(pmax(fflat / fstd, -10), 10)
            },
            # the month's move in units of what realized vol would have predicted
            move1mSig = stkPxChng1m / (na_if(clsHvXern20d, 0) * sqrt(21 / 252)),
            # intraday family on both legs (orHvXern20d, not clsHv): a ratio of
            # two estimators would read their gap as a spike
            rv5_20   = na_if(orHvXern5d, 0) / na_if(orHvXern20d, 0),
            rv20_252 = na_if(clsHvXern20d, 0) / na_if(clsHvXern252d, 0),
            q_ivrv = pct(ivrvNow) / 100,
            q_vov  = pct(volOfVol) / 100,
            q_ffr  = 1 - pct(fwdRatio) / 100,
            vrpScore = rowMeans(cbind(q_ivrv, q_vov, q_ffr), na.rm = TRUE)
        )
    }

    # Dashboard -> Ticker tab. Screening is only useful if you can go straight
    # from a hit to the deep dive, so a click on any screen point or table row
    # loads that ticker there.
    # one piece of state both entry points write to, rather than each firing
    # its own pair of update calls
    selected_ticker <- reactiveVal(NULL)
    open_in_ticker <- function(tkr) {
        tkr <- as.character(tkr)
        if (!length(tkr) || is.na(tkr[1]) || !nzchar(tkr[1])) return(invisible(NULL))
        selected_ticker(tkr[1])
    }
    observeEvent(selected_ticker(), {
        updateTextInput(session, "t_ticker", value = selected_ticker())
        updateTabsetPanel(session, "tabs", selected = "Ticker")
    })
    observeEvent(event_data("plotly_click", source = "screen"), {
        e <- event_data("plotly_click", source = "screen")
        open_in_ticker(e$customdata)
    })

    # ---- Dashboard: data freshness ----
    # The core and dailies archives are written by ONE process, orats_downloader.py,
    # run by hand; this app and every screener only read them. So the honest
    # thing to show is what that process has delivered, and what to run when
    # it is behind. Computed once per session start (the directories do not
    # change while the app runs unless you run the downloader).
    output$data_freshness <- renderUI({
        newest_file_date <- function(dir, pattern) {
            f <- list.files(dir, pattern)
            if (!length(f)) return(as.Date(NA))
            max(as.Date(sub(".*_([0-9]{8})\\.csv\\.gz$", "\\1", f), format = "%Y%m%d"), na.rm = TRUE)
        }
        core_day    <- newest_file_date(core_dir, "^orats_core_[0-9]{8}\\.csv\\.gz$")
        dailies_day <- newest_file_date(dailies_dir, "^orats_dailies_[0-9]{8}\\.csv\\.gz$")
        loaded_day  <- last_data_day
        delayed_day <- if (file.exists(ORATS_code_delayed_file)) as.Date(file.mtime(ORATS_code_delayed_file)) else as.Date(NA)
        # the last session that should have a file: today after 18:00 ET, else the previous weekday
        now_et <- as.POSIXlt(Sys.time(), tz = "America/New_York")
        expect <- as.Date(format(now_et, "%Y-%m-%d"))
        if (now_et$hour < 18) expect <- expect - 1
        nyse_hol <- as.Date(timeDate::holidayNYSE(as.numeric(format(expect, "%Y")) + (-1:0)))
        while (format(expect, "%u") %in% c("6", "7") || expect %in% nyse_hol) expect <- expect - 1
        lag_core    <- as.numeric(expect - core_day)
        lag_dailies <- as.numeric(expect - dailies_day)
        ok <- function(lag) is.finite(lag) && lag <= 0
        warn <- !(ok(lag_core) && ok(lag_dailies))
        fmt <- function(d) if (is.na(d)) "none" else format(d)
        style <- if (warn) "background:#f8d7da; border:1px solid #f1aeb5; color:#58151c;"
                 else       "background:#d1e7dd; border:1px solid #a3cfbb; color:#0a3622;"
        # the downloader takes no arguments: it resumes from the newest CORE file
        # and runs to today, for every dataset
        cmd <- "cd ~/trading/HistoricalData/ORATS && python3 orats_downloader.py"
        HTML(paste0(
            "<div style='", style, " padding:8px 14px; margin:8px 0 4px 0; border-radius:4px; font-size:13px;'>",
            "<b>Data: </b>core <b>", fmt(core_day), "</b>",
            if (is.finite(lag_core) && lag_core > 0) sprintf(" <span style='color:#b02a37'>(%d session%s behind)</span>", lag_core, if (lag_core > 1) "s" else "") else "",
            " &nbsp;|&nbsp; dailies <b>", fmt(dailies_day), "</b>",
            if (is.finite(lag_dailies) && lag_dailies > 0) sprintf(" <span style='color:#b02a37'>(%d session%s behind)</span>", lag_dailies, if (lag_dailies > 1) "s" else "") else "",
            " &nbsp;|&nbsp; loaded in app <b>", fmt(loaded_day), "</b>",
            if (!is.na(core_day) && loaded_day < core_day) " <span style='color:#b02a37'>(restart the app to load the newer core file)</span>" else "",
            " &nbsp;|&nbsp; delayed row: ", if (is.na(delayed_day)) "none" else paste0("file from ", fmt(delayed_day)),
            " &nbsp;|&nbsp; expected latest session <b>", format(expect), "</b>",
            if (warn) paste0("<br>Both archives are written only by the downloader, run by hand. Update with: <code>", cmd,
                             "</code> &nbsp;(then <code>Rscript orats_delayed.R</code> for today's provisional row, and restart the app).") else "",
            # the downloader resumes from the newest CORE file, so a dailies
            # session that 404'd while core succeeded is never retried by it
            if (is.finite(lag_dailies) && is.finite(lag_core) && lag_dailies > lag_core)
                "<br><b>dailies is behind core:</b> the downloader resumes from the newest core file, so it will NOT refill the missing dailies sessions on its own - fetch those dates by hand." else "",
            "</div>"))
    })

    # ---- Dashboard: candidates ----
    # Dashboard -> Chain tab: a candidate is only worth anything once you have
    # seen its curve and its spreads, so the candidates panel opens there.
    open_in_chain <- function(tkr) {
        tkr <- as.character(tkr)
        if (!length(tkr) || is.na(tkr[1]) || !nzchar(tkr[1])) return(invisible(NULL))
        updateTextInput(session, "c_ticker", value = tkr[1])
        updateTabsetPanel(session, "tabs", selected = "Chain")
    }
    observeEvent(event_data("plotly_click", source = "cand"), {
        e <- event_data("plotly_click", source = "cand")
        open_in_chain(e$customdata)
    })

    cand_df <- reactive({
        # dash_day = the screen plus the rolling ranks (skew and term among them)
        df <- dash_day() %>%
            dplyr::filter(is.finite(ivrvNow))
        if (isTRUE(input$cand_hide_wide)) {
            med_w <- median(df$mktWidthVol, na.rm = TRUE)
            df <- df %>% dplyr::filter(is.finite(mktWidthVol), mktWidthVol <= 2 * med_w)
        }
        validate(need(nrow(df) > 0, "No candidates on this day / universe."))
        # what a reader would call the name, so the table needs no legend
        df %>% mutate(
            flags = paste0(
                if_else(q_ivrv >= 0.9, "RICH ", ""),
                if_else(q_ffr  >= 0.9, "BACKWARD ", ""),
                if_else(q_vov  >= 0.9, "VOV ", ""),
                if_else(is.finite(move1mSig) & abs(move1mSig) >= 2, "EXTENDED ", ""),
                if_else(ivrvNow <= -0.1, "CHEAP ", ""),
                if_else(is.finite(rv5_20) & rv5_20 >= 1.4, "RV-RISING ", ""),
                if_else(is.finite(skewPct) & skewPct >= 90, "STEEP-SKEW ", ""),
                if_else(is.finite(skewPct) & skewPct <= 10, "FLAT-SKEW ", ""),
                if_else(is.finite(mktWidthVol) & mktWidthVol > 2 * median(mktWidthVol, na.rm = TRUE), "WIDE ", ""),
                # ETFs carry a meaningless ernDate1, so the earnings hint is for single names only
                if_else(class == "Stock" & is.finite(daysSinceErn) & daysSinceErn >= 75, "ERN-DUE? ", "")) %>% trimws()
        ) %>% arrange(desc(vrpScore))
    })

    # The Candidates pair. Same y on both - log(IV/RV) ranked against the
    # name's own history - so a name sits at the same height in each panel
    # and the two x axes ask different questions of it.
    cand_pair <- function(side) {
        z  <- identical(input$cand_stat, "z"); n <- norm_n()
        cv <- input$cand_colour; if (is.null(cv) || !nzchar(cv)) cv <- "vrpScore"
        cscale <- switch(cv, skewPct = list(lab = "skew pct", lo = 0, hi = 100, fmt = ".0f"),
                             termPct = list(lab = "term pct", lo = 0, hi = 100, fmt = ".0f"),
                             list(lab = "vrp score", lo = 0, hi = 1, fmt = ".2f"))
        ycol <- if (z) "ivrv_z" else "ivrv_pct"
        xcol <- if (side == "right") "rv5_20" else if (z) "iv_z" else "ivPctile1y"
        df <- cand_df() %>% mutate(xv = .data[[xcol]], yv = .data[[ycol]]) %>%
            dplyr::filter(is.finite(xv), is.finite(yv), side == "left" | xv > 0)
        validate(need(nrow(df) > 0, paste0("No ", xcol, " / ", ycol, " on this day (need ", n, " sessions of history).")))
        n_lab <- suppressWarnings(as.integer(input$cand_n_labels)); if (!isTRUE(n_lab >= 0)) n_lab <- 12
        y_ext <- if (z) abs(df$yv) >= 2 else df$yv <= 5 | df$yv >= 95
        x_ext <- if (side == "right") df$xv >= 1.4 | df$xv <= 0.7
                 else if (z) abs(df$xv) >= 2 else df$xv <= 5 | df$xv >= 95
        df <- df %>% mutate(label = if_else(ticker %in% head(ticker, n_lab) | y_ext | x_ext, ticker, ""),
                            sz = 6 + 3 * pmax(log10(pmax(avgOptVolu20d, 1)) - 2, 0))
        stat_lab <- if (z) "z-score" else "percentile"
        # guides: the y tails on both panels; x tails on the left, the RV5/RV20
        # thresholds on the right
        yg <- if (z) c(-2, 0, 2) else c(5, 50, 95)
        shp <- lapply(yg, function(v) guide(0, 1, v, v, xref = "paper",
                                            dash = if (v %in% c(0, 50)) "dot" else "dash",
                                            col = if (v %in% c(0, 50)) "lightgray" else "gray"))
        xg <- if (side == "right") c(0.7, 1, 1.4) else if (z) c(-2, 0, 2) else c(5, 50, 95)
        shp <- c(shp, lapply(xg, function(v) guide(v, v, 0, 1, yref = "paper",
            dash = if (v %in% c(0, 1, 50)) "dot" else "dash",
            col = if (side == "right" && v == 1.4) "firebrick" else if (side == "right" && v == 0.7) "seagreen"
                  else if (v %in% c(0, 1, 50)) "lightgray" else "gray")))
        ann <- if (side == "right") list(
            list(x = log10(1.4), y = 1, yref = "paper", text = "realized still rising: early marks against >", showarrow = FALSE,
                 xanchor = "left", yanchor = "top", font = list(color = "firebrick")),
            list(x = log10(0.7), y = 1, yref = "paper", text = "< realized cooling", showarrow = FALSE,
                 xanchor = "right", yanchor = "top", font = list(color = "seagreen"))) else list()
        xlab <- if (side == "right") "RV5 / RV20, ORATS intraday HV (log scale)"
                else if (z) paste0("IV30 z-score vs own ", n, "d") else "IV percentile, 1y (ORATS ivPctile1y)"
        rngy <- if (z) c(-4, 4) else c(-5, 105)
        plot_ly(df, x = ~xv, y = ~yv, type = "scatter", mode = "markers+text",
                text = ~label, textposition = "top center", textfont = list(size = 11),
                hovertext = ~sprintf(paste0("%s  (%s)<br>IV/RV %.2f: pct %.0f, z %+.1f   iv30 %.1f<br>",
                                            "rv5 (intraday) %.1f  rv20 %.1f  rv252 %.1f   RV5/RV20 (intraday) %.2f  RV20/RV252 %.2f<br>",
                                            "IV pct1y %.0f   IV/IVVOL pct %.0f   skew pct %.0f   width %.1f   %s"),
                                     ticker, class, exp(ivrvNow), ivrv_pct, ivrv_z, exErnIv30d,
                                     orHvXern5d, clsHvXern20d, clsHvXern252d, rv5_20, rv20_252,
                                     ivPctile1y, iv_ivvol_pct, skewPct, mktWidthVol, flags),
                customdata = ~ticker, source = "cand",
                marker = list(size = ~sz, opacity = 0.85, color = df[[cv]],
                              colorscale = "Viridis", cmin = cscale$lo, cmax = cscale$hi,
                              showscale = side == "right", colorbar = list(title = cscale$lab),
                              line = list(width = 0.5, color = "gray30")),
                hovertemplate = paste0("%{hovertext}<br>", cscale$lab, " %{marker.color:", cscale$fmt, "}<extra></extra>")) %>%
            layout(xaxis = list(title = xlab, zeroline = FALSE,
                                type = if (side == "right") "log" else "linear",
                                range = if (side == "right") NULL else rngy),
                   yaxis = list(title = paste0("log(IV30/RV20), ", stat_lab, " vs own ", n, "d"),
                                range = rngy, zeroline = FALSE),
                   shapes = shp, annotations = ann, font = list(size = 13),
                   title = plot_title(if (side == "right") "Will it hurt before it pays? IV/RV vs RV5/RV20 (intraday)"
                                      else "Is vol rich and high? IV/RV vs IV level"))
    }
    output$cand_plot_left  <- renderPlotly(cand_pair("left"))
    output$cand_plot_right <- renderPlotly(cand_pair("right"))

    cand_tbl_df <- reactive({
        cand_df() %>% dplyr::transmute(
            ticker, class, flags,
            vrp = round(vrpScore, 2), ivrv = round(q_ivrv, 2), vov = round(q_vov, 2), bwd = round(q_ffr, 2),
            rich = round(richScore, 0),
            `IV/RV` = round(exp(ivrvNow), 2), ivrvPct = round(ivrv_pct, 0), ivrvZ = round(ivrv_z, 1),
            iv30 = round(exErnIv30d, 1), `rv5 intraday` = round(orHvXern5d, 1), rv20 = round(clsHvXern20d, 1),
            `RV5/RV20 intraday` = round(rv5_20, 2),
            ivPct1y = round(ivPctile1y, 0), rvPct1y = round(rvPctile1y, 0),
            `1m sig` = round(move1mSig, 1),
            `1w%` = round(stkPxChng1wk, 1), `1m%` = round(stkPxChng1m, 1), `6m%` = round(stkPxChng6m, 1),
            fwd30_60 = round(fwdRatio, 2), slope = round(slope, 2),
            # 25d RR / ATM today, and where it and the 30/90 term ratio sit in
            # the name's own history (100 = steepest put skew / most front-rich)
            `RR/ATM` = round(skew_rr, 3), skewPct = round(skewPct, 0), termPct = round(termPct, 0),
            # ORATS' own forecast as the yardstick: straddle vs forecast straddle, IV vs forecast IV
            straRich = round(straRich, 3), ivFcst = round(ivFcstRatio, 3),
            ivPctSpy = round(ivPctileSpy, 0),
            widthVol = round(mktWidthVol, 2), conf = round(confidence, 0),
            daysSinceErn = round(daysSinceErn, 0), ernMove = round(absAvgErnMv, 1),
            optVolu20d = round(avgOptVolu20d, 0))
    })
    output$cand_tbl <- renderDT({
        DT::datatable(cand_tbl_df(), rownames = FALSE, selection = "single",
                      options = list(pageLength = 25, scrollX = TRUE)) %>%
            DT::formatStyle("vrp", background = DT::styleColorBar(c(0, 1), "rgba(68,1,84,0.25)"))
    })
    observeEvent(input$cand_tbl_rows_selected, {
        open_in_chain(cand_tbl_df()$ticker[input$cand_tbl_rows_selected])
    })

    dash_core <- reactive({
        switch(as.character(input$dash_universe),
               "Single names" = dplyr::filter(ORATS_core, class == "Stock"),
               "All"          = ORATS_core,
               dplyr::filter(ORATS_core, class != "Stock"))
    })

    # one title style for every chart on the tab, so nine stacked plots are
    # identifiable without reading the code
    # plotly renders HTML in title text, and its title font has no weight
    # property, so bold via the tag rather than the font spec
    plot_title <- function(txt) list(text = paste0("<b>", txt, "</b>"),
                                     x = 0, xanchor = "left",
                                     font = list(size = 15))

    # DTE arrives from a textInput, so parse it once here instead of letting
    # each plot compare a number against a string - dtExM1 == "25" only works
    # by coercion, and silently matches nothing on " 25" or "25.0".
    t_dte_num <- reactive({
        v <- suppressWarnings(as.numeric(trimws(as.character(input$t_dte))))
        req(is.finite(v), v > 0)
        v
    })
    t_win <- function(d) {
        m <- t_range_months()
        if (is.infinite(m) || nrow(d) == 0) return(d)
        dplyr::filter(d, tradeDate >= ticker_end() - m * 30.5)
    }

    # ---- Ticker tab: OHLC-based realized-vol estimators ----
    # The range estimators (Parkinson, Garman-Klass, Rogers-Satchell,
    # Yang-Zhang) need the open/hi/lo the core file does not carry, so they
    # read ORATS_ohlc (built from the dailies at startup). Same downloader and
    # same trade dates as the core, so no second data provenance to reconcile.
    # Cross-check: TTR close-to-close on this OHLC reproduces ORATS
    # clsHvXern20d to 2dp on SPY (13.62 both).
    # TTR wants an OHLC object, so hand it an xts with the canonical names.
    # xts is NOT attached on purpose - it would mask dplyr's first()/last(),
    # which the plots below rely on.
    ticker_ohlc <- reactive({
        tkr <- t_ticker_deb()
        req(nzchar(tkr))
        d <- ORATS_ohlc %>% dplyr::filter(ticker == tkr) %>% arrange(tradeDate)
        if (nrow(d) < 30) return(NULL)
        # unadjusted prices (see ohlc_cols), back-adjusted off the close; the
        # same factor applies to the day's open/high/low
        k <- split_adjust(d$tradeDate, na_if(d$unadjClsPx, 0), tkr) / na_if(d$unadjClsPx, 0)
        k <- zoo::na.locf(zoo::na.locf(k, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE)
        d <- d %>% mutate(open = unadjOpen * k, hiPx = unadjHiPx * k,
                          loPx = unadjLoPx * k, clsPx = unadjClsPx * k)
        # The dailies carry the odd mangled high/low - SPY 2026-02-02 prints a
        # low of 69 against a 695 close, a dropped digit. Close-to-close never
        # reads it, but one such row blows up every range estimator for the
        # whole n-day window around it, so bound a high/low that contradicts
        # the day's own open/close back onto them. Deliberately conservative:
        # it narrows that one day's range rather than discarding the row and
        # taking an NA hole through the next n days.
        d <- d %>% mutate(
            hiPx = if_else(hiPx < pmax(open, clsPx) | hiPx > 2 * clsPx,
                           pmax(open, clsPx), hiPx),
            loPx = if_else(loPx > pmin(open, clsPx) | loPx < 0.5 * clsPx,
                           pmin(open, clsPx), loPx))
        m <- as.matrix(d[, c("open", "hiPx", "loPx", "clsPx")])
        colnames(m) <- c("Open", "High", "Low", "Close")
        xts::xts(m, order.by = d$tradeDate)
    })

    # Realized-vol lookback, in trading days, matched to the IV horizon at
    # 21 trading days per month. Plot 2 cannot do this - ORATS ships
    # clsHvXern 5/10/20/60/90/120/252d and nothing at 42d or 63d, so its 60d
    # and 90d settings put a 2- and 3-month IV against a 3- and 4.3-month
    # realized window. Computing RV ourselves frees the window, so here the
    # two horizons actually line up. Consequence: on 30d this is a 21d window,
    # so it no longer reproduces ORATS clsHvXern20d to the decimal.
    rv_n <- reactive({
        switch(as.character(input$t_vol_window),
               "30d" = 21, "60d" = 42, "90d" = 63,
               "6m" = 126, "1yr" = 252, 21)
    })

    # the five estimators, annualised and in percent to match the ORATS scale
    rv_estimators <- reactive({
        x <- ticker_ohlc()
        if (is.null(x) || nrow(x) < 30) return(NULL)
        n <- rv_n()
        calcs <- c("Close-to-close"  = "close",
                   "Parkinson"       = "parkinson",
                   "Garman-Klass"    = "garman.klass",
                   "Rogers-Satchell" = "rogers.satchell",
                   "Yang-Zhang"      = "yang.zhang")
        est <- lapply(calcs, function(cc) {
            v <- as.numeric(suppressWarnings(
                TTR::volatility(x, n = n, calc = cc, N = 252))) * 100
            # Rogers-Satchell's daily terms are each >= 0, so a window of flat
            # days (O=H=L=C, common in thin ETFs - BNDD prints 101 of them)
            # sums to exactly zero in theory and to a tiny negative in
            # floating point, and sqrt() of that is NaN. Yang-Zhang embeds RS
            # and inherits the holes. The true value there is zero vol, so say
            # zero rather than leaving a gap in the line. Only NaN is touched;
            # the leading NAs before the window fills stay NA.
            v[is.nan(v)] <- 0
            v
        })
        dplyr::bind_cols(tibble(tradeDate = as.Date(zoo::index(x))),
                         tibble::as_tibble(est))
    })

    # ---- Pairs tab: debounced inputs + memoized correlation matrix ----
    # the all-tickers return-correlation matrix is expensive and does not
    # depend on any input -> computed lazily once per session, then cached
    pair_1_deb <- debounce(
        reactive(toupper(trimws(as.character(input$ticker_1)))), 600)
    pair_2_deb <- debounce(
        reactive(toupper(trimws(as.character(input$ticker_2)))), 600)
    returns_cor_mat <- reactive({
        # split-adjusted per ticker, or one -69% split day dominates the pair
        wide_ret <- ORATS_core %>% group_by(ticker) %>% arrange(tradeDate, .by_group = TRUE) %>%
            mutate(log_ret = c(NA, diff(log(split_adjust(tradeDate, na_if(pxAtmIv, 0), cur_group()$ticker))))) %>%
            select(tradeDate, ticker, log_ret) %>%
            pivot_wider(names_from = ticker, values_from = log_ret)
        wide_ret %>% select(-tradeDate) %>% cor(use = "pairwise.complete.obs")
    })



    
    
    # ---- Dashboard: rolling stats, computed ONCE per universe ----
    # The panels these feed used to each re-run 252-day z-scores and percent
    # ranks over the whole 500-day panel on every render. One pass, keyed on
    # the universe, and the date filter is applied afterwards.
    norm_n <- reactive({
        n <- suppressWarnings(as.integer(input$norm_lookback)); if (!isTRUE(n >= 20)) 252L else n
    })
    dash_rolling <- reactive({
        n <- norm_n()
        dash_core() %>% group_by(ticker) %>% arrange(tradeDate) %>%
            mutate(
                # the app's one IV/RV: 30d ex-earnings IV over 20d close-to-close
                # ex-earnings HV, as in the Candidates axis and the Ticker tab's
                # IV/RV panel. ORATS' ivHvXernRatio is a different pair (closest
                # match orIvXern20d / orHvXern20d) and disagreed with both.
                ivrv_log = roll_safe(log(na_if(exErnIv30d, 0) / na_if(clsHvXern20d, 0))),
                ivrv_z   = runZscore(ivrv_log, n),
                ivrv_pct = TTR::runPercentRank(ivrv_log, n) * 100,
                # IV's own z over the lookback, the z-mode partner of ivPctile1y
                iv_z     = runZscore(roll_safe(log(na_if(exErnIv30d, 0))), n),
                iv_ivvol_log = {
                    w <- ew_sd_roll(replace_na(c(NA, diff(log(na_if(exErnIv30d, 0)))), 0), 20)
                    roll_safe(log(na_if(exErnIv30d, 0) / w))
                },
                iv_ivvol_z   = runZscore(iv_ivvol_log, n),
                iv_ivvol_pct = TTR::runPercentRank(iv_ivvol_log, n) * 100,
                # forward factor for every front/back pair the sidebar offers;
                # fexErn<back>_<front> is ORATS' forward vol between the two
                ff_30_60  = exErnIv30d / na_if(fexErn60_30, 0) - 1,
                ff_30_90  = exErnIv30d / na_if(fexErn90_30, 0) - 1,
                ff_60_90  = exErnIv60d / na_if(fexErn90_60, 0) - 1,
                ff_90_180 = exErnIv90d / na_if(fexErn180_90, 0) - 1,
                back_pct_60  = TTR::runPercentRank(roll_safe(na_if(exErnIv60d, 0)), n) * 100,
                back_pct_90  = TTR::runPercentRank(roll_safe(na_if(exErnIv90d, 0)), n) * 100,
                back_pct_180 = TTR::runPercentRank(roll_safe(na_if(exErnIv6m, 0)), n) * 100,
                # 30d ex-earnings 25d risk reversal (put minus call, see
                # cols_to_extract for ORATS' call-delta labels) over ATM, so a
                # vol spike does not read as rich skew; and the 30d/90d term
                # ratio, high = front rich. Both ranked against the name's own
                # history; a name ORATS never filled wings for has no rank.
                skew_rr  = (na_if(exErnDlt75Iv30d, 0) - na_if(exErnDlt25Iv30d, 0)) / na_if(exErnIv30d, 0),
                skewPct  = tryCatch(TTR::runPercentRank(roll_safe(skew_rr), n) * 100,
                                    error = function(e) NA_real_),
                termPct  = tryCatch(TTR::runPercentRank(roll_safe(log(na_if(exErnIv30d, 0) / na_if(exErnIv90d, 0))), n) * 100,
                                    error = function(e) NA_real_)
            ) %>% ungroup() %>%
            dplyr::select(ticker, tradeDate, ivrv_z, ivrv_pct, iv_z, iv_ivvol_z, iv_ivvol_pct,
                          starts_with("ff_"), starts_with("back_pct_"), skew_rr, skewPct, termPct)
    })
    # the day's screen (liquidity/width gated, vrp-scored) joined to its rolling stats
    dash_day <- reactive({
        dash_screen(as.numeric(input$scr_n_tickers)) %>%
            left_join(dash_rolling() %>% dplyr::filter(tradeDate == input$date), by = c("ticker", "tradeDate"))
    })

    cand_marker <- function(df) list(size = 11, opacity = 0.85, color = df$vrpScore,
                                     colorscale = "Viridis", cmin = 0, cmax = 1, showscale = TRUE,
                                     colorbar = list(title = "vrp score"), line = list(width = 0.5, color = "gray30"))
    guide <- function(x0, x1, y0, y1, xref = "x", yref = "y", dash = "dot", col = "gray")
        list(type = "line", x0 = x0, x1 = x1, y0 = y0, y1 = y1, xref = xref, yref = yref,
             line = list(dash = dash, color = col))

    output$screen_plot <- renderPlotly({
        n_tickers <- as.numeric(input$scr_n_tickers)
        xcol <- input$scr_xvar; ycol <- input$scr_yvar
        req(nzchar(xcol), nzchar(ycol), is.finite(n_tickers))
        # dash_day carries the rolling z-scores and forward factors as well
        df <- dash_day() %>% dplyr::filter(is.finite(.data[[xcol]]), is.finite(.data[[ycol]]))
        validate(need(nrow(df) > 0, paste0("Nothing finite for ", xcol, " vs ", ycol, " on this day.")))
        n_lab <- suppressWarnings(as.integer(input$cand_n_labels)); if (!isTRUE(n_lab >= 0)) n_lab <- 12
        # same convention as the panels above: the top of the vrp score plus
        # the 5% tails of whichever two columns are on the axes
        xv <- df[[xcol]]; yv <- df[[ycol]]
        ext <- xv >= quantile(xv, 0.95, na.rm = TRUE) | xv <= quantile(xv, 0.05, na.rm = TRUE) |
               yv >= quantile(yv, 0.95, na.rm = TRUE) | yv <= quantile(yv, 0.05, na.rm = TRUE)
        df <- df %>% mutate(label = if_else(ticker %in% head(ticker, n_lab) | ext, ticker, ""))
        plot_ly(df, x = ~.data[[xcol]], y = ~.data[[ycol]],
                type = "scatter", mode = "markers+text",
                text = ~label, textposition = "top center", textfont = list(size = 12),
                hovertext = ~sprintf("%s  (%s)", ticker, class),
                customdata = ~ticker, source = "screen", marker = cand_marker(df),
                hovertemplate = paste0("%{hovertext}<br>", xcol, ": %{x:.2f}<br>",
                                       ycol, ": %{y:.2f}<br>vrp score %{marker.color:.2f}<extra></extra>")) %>%
            layout(xaxis = list(title = xcol), yaxis = list(title = ycol),
                   font = list(size = 14),
                   title = plot_title(paste(ycol, "vs", xcol, "(colour = vrp score)")))
    })

    output$strike_plot <- DT::renderDT({
        type <- input$fd_ratio
        n_tickers <- as.numeric(input$fd_n_tickers)
        date <- input$date
        if(type == "Ratio") vals <- c(-10, 0, 10) else vals <- c(-20, 0, 20)
        brks <- quantile(vals, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
        cols <- colorRampPalette(c("#d20231", "orange1", "lightblue", "#218be7"))(4)
        df <- dash_core() %>% group_by(ticker) %>% arrange(tradeDate) #%>% slice_tail(n=2)
        tops <- df %>% group_by(ticker) %>% slice_tail(n=1) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        df_table <- df %>% dplyr::filter(ticker %in% tops$ticker) %>% 
            dplyr::select(ticker, tradeDate, class, exErnIv10d:exErnIv1yr) %>% 
            group_by(ticker) %>% arrange(tradeDate) %>% 
            mutate(across(exErnIv10d:exErnIv1yr, ~case_when(type == "Clicks" ~ round(.x - lag(.x),2),
                                                      type == "Ratio" ~ round((.x / lag(.x) - 1)*100,2),
                                                      TRUE ~ NA
                                                      ))) 
        df_table <- df_table %>% dplyr::filter(tradeDate == date) %>% ungroup
        need_rows(df_table)
        # biggest 30d move first: an event arriving (10d/30d up, 1y flat) or the crush after one
        DT::datatable(df_table, rownames = FALSE,
                      options = list(pageLength = n_tickers,
                                     order = list(list(which(names(df_table) == "exErnIv30d") - 1, "desc")))) %>%  formatStyle(
            c("exErnIv10d", "exErnIv30d", "exErnIv60d", "exErnIv90d", "exErnIv6m", "exErnIv1yr"),
            backgroundColor = styleInterval(
                brks[-c(1, length(brks))],  # internal breakpoints only
                cols
            )
            )
        
    })
    
    # Regime timeline (Sharpe Two replica): GMM regimes scored on the full
    # ORATS history; see regime_timeline/src/regime_shiny.R
    output$ticker_plot_regime <- renderPlotly({
        # the regime store goes back to 2015, so "Max" is capped at 100 years
        # (i.e. everything) rather than passed as Inf
        regime_timeline_plotly(t_ticker_deb(), months = min(t_range_months(), 1200))
    })

    output$ticker_plot_1 <- renderPlotly({
        t_dte <- t_dte_num()
        df <- ticker_df()

        # Stock Price
        p_price <- plot_ly(
            data = t_win(df),
            x = ~tradeDate,
            y = ~pxAtmIv,
            type = "scatter",
            mode = "lines",
            hovertemplate = paste(
                "Date: %{x}<br>",
                "Price: %{y:.4f}<extra></extra>"
            )
        ) %>%
            layout(
                xaxis = list(title = ""),
                yaxis = list(title = "Price"),
                hovermode = "x unified"
            )
        
        # Volume
        p_vol <- plot_ly(
            t_win(df),
            x = ~tradeDate,
            y = ~cVolu+pVolu,
            type = "bar",
            name = "Volume",
            hovertemplate = "Date: %{x}<br>Volume: %{y:,}<extra></extra>"
        ) %>%
            layout(
                xaxis = list(title = ""),
                yaxis = list(title = "Volume")
            )
        
        # Momentum
        p_mom <- plot_ly(
            df %>% mutate(
                mom_m = (stkPxChng1m * 0.12) %>% EMA,
                mom_w = (stkPxChng1wk * 0.52) %>% EMA,
                mom_6m = (stkPxChng6m * 0.02) %>% EMA,
            ) %>% t_win,
            x = ~tradeDate,
            hovertemplate = "Date: %{x}<br>Momentum: %{y:,}<extra></extra>"
        ) %>%  
            add_lines(y = ~mom_w, line = list(color = "darkgray")) %>%
            add_lines(y = ~mom_m, line = list(color = "gray")) %>%
            add_lines(y = ~mom_6m, line = list(color = "lightgray")) %>%
            layout(
                xaxis = list(title = ""),
                yaxis = list(title = "Momentum",showlegend = FALSE)
            )
        
        p1 <- subplot(
            p_price,
            p_vol,
            p_mom,
            nrows = 3,
            shareX = TRUE,
            heights = c(0.3, 0.2, 0.5),
            titleX = TRUE,titleY = TRUE
        ) %>%
            layout(
                hovermode = "x unified",
                showlegend = FALSE
            )
        
        # Return/IV correlation
        p2 <- plot_ly(
            data = df %>% mutate(iv_chg = c(0,diff(exErnIv30d)), log_ret = c(0, diff(log(pxAdj)))) %>% filter(dtExM1 <= t_dte) %>% t_win,
            x = ~log_ret,
            y = ~iv_chg,
            text = ~tradeDate,
            type = "scatter",
            mode = "markers",
            hovertemplate = paste(
                "Date: %{text}<br>"
                )
        ) %>%
            layout(
                xaxis = list(title = "Price Return"),
                yaxis = list(title = "IV Change"),
                hovermode = "x unified"
            )
        
        
        p <- subplot(
            p1, p2, 
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16), showlegend = FALSE,
            title = plot_title("Price, volume and momentum | IV change vs return")
        )
        p
        
    })
    
    output$ticker_plot_2 <- renderPlotly({
        t_vol_window <- as.character(input$t_vol_window)
        df <- ticker_df()


        p <- plot_ly(df %>% mutate(
                IV = case_when(
                    t_vol_window == "30d" ~ exErnIv30d,
                    t_vol_window == "60d" ~ exErnIv60d,
                    t_vol_window == "90d" ~ exErnIv90d,
                    t_vol_window == "6m" ~ exErnIv6m,
                    t_vol_window == "1yr" ~ exErnIv1yr,
                    TRUE ~ NA),
                RV = case_when(
                    t_vol_window == "30d" ~ clsHvXern20d,
                    t_vol_window == "60d" ~ clsHvXern60d,
                    t_vol_window == "90d" ~ clsHvXern90d,
                    t_vol_window == "6m" ~ clsHvXern120d,
                    t_vol_window == "1yr" ~ clsHvXern252d,
                    TRUE ~ NA)
        ) %>% t_win,
        x = ~tradeDate) %>%
            add_lines(y = ~IV, name = "IV", line = list(color = "blue")) %>%
            add_lines(y = ~RV, name = "RV", line = list(color = "red")) %>%
            layout(
                xaxis = list(title = ""), yaxis = list(title = ""),
                legend = list(x = 0.1, y = 0.9),font = list(size = 16),
                title = plot_title("IV vs ORATS realized vol")
            )
        p
    })

    # Same IV as plot 2, against realized vol measured five different ways off
    # OHLC. All six series are annualised vol in %, so they share one axis.
    # The range estimators (Parkinson onwards) read intraday range and so sit
    # below close-to-close whenever moves are trending rather than gapping.
    output$ticker_plot_rv_est <- renderPlotly({
        est <- rv_estimators()
        if (is.null(est))
            return(plot_ly() %>% layout(
                title = list(text = paste("No OHLC available for", t_ticker_deb()),
                             font = list(size = 14)),
                font = list(size = 16)))

        t_vol_window <- as.character(input$t_vol_window)
        iv <- ticker_df() %>% dplyr::transmute(
            tradeDate,
            IV = case_when(
                t_vol_window == "30d" ~ exErnIv30d,
                t_vol_window == "60d" ~ exErnIv60d,
                t_vol_window == "90d" ~ exErnIv90d,
                t_vol_window == "6m" ~ exErnIv6m,
                t_vol_window == "1yr" ~ exErnIv1yr,
                TRUE ~ NA))

        # fixed slot order, so a series keeps its colour no matter which ones
        # are on screen; IV stays blue to match plot 2 on the left
        cols <- c("IV"              = "#2a78d6",
                  "Close-to-close"  = "#eb6834",
                  "Parkinson"       = "#1baf7a",
                  "Garman-Klass"    = "#eda100",
                  "Rogers-Satchell" = "#e87ba4",
                  "Yang-Zhang"      = "#008300")

        long <- est %>% left_join(iv, by = "tradeDate") %>% t_win %>%
            pivot_longer(-tradeDate, names_to = "series", values_to = "vol") %>%
            mutate(series = factor(series, levels = names(cols)))

        plot_ly(long, x = ~tradeDate, y = ~vol, color = ~series, colors = cols,
                type = "scatter", mode = "lines", line = list(width = 2),
                hovertemplate = "%{y:.2f}<extra>%{fullData.name}</extra>") %>%
            layout(
                xaxis = list(title = ""),
                # name the window: plot 2 next door uses a different one at
                # the 60d and 90d settings, and the two are easily confused
                yaxis = list(title = paste0("Annualised vol (%) - ", rv_n(), "d")),
                hovermode = "x unified",
                legend = list(orientation = "h", x = 0.5, xanchor = "center",
                              y = -0.15),
                font = list(size = 16),
                title = plot_title("IV vs realized vol, five estimators")
            )
    })

    output$ticker_plot_3 <- renderPlotly({
        # The cone is a distribution, not a time series: at a 3m Time Range it
        # would be drawing min/max off ~63 observations. Pin it to 2y and
        # ignore the selector, and say so in the title.
        cone_years <- 2
        df <- ticker_df() %>%
            dplyr::filter(tradeDate >= ticker_end() - cone_years * 365.25)



        stats <- list(
            min = ~min(.x, na.rm = TRUE),
            max   = ~max(.x, na.rm = TRUE),
            q25  = ~quantile(.x, 0.25, na.rm = TRUE),
            q75  = ~quantile(.x, 0.75, na.rm = TRUE),
            current = ~last(.x)
        )
        
        IV_expiries <- c("10d","30d","60d","90d","6m","1y")
        cone_df_IV <- map_dfr(stats, function(f) {
            summarise(df,
                      across(exErnIv10d:exErnIv1yr, f))
        }, .id = "stat") %>% t %>% as.data.frame() 
        colnames(cone_df_IV) <- cone_df_IV[1,]; cone_df_IV <- cone_df_IV[-1,]
        cone_df_IV$horizon <-  factor(IV_expiries, levels = IV_expiries)
        cone_df_IV <- cone_df_IV %>% mutate(across(min:current, ~as.numeric(.x)))
        
        RV_expiries <- c("10d","20d", "60d","90d","120d","252d")
        cone_df_RV <- map_dfr(stats, function(f) {
            summarise(df,
                      across(clsHvXern10d:clsHvXern252d, f))
        }, .id = "stat") %>% t %>% as.data.frame()
        colnames(cone_df_RV) <- cone_df_RV[1,]; cone_df_RV <- cone_df_RV[-1,];
        cone_df_RV$horizon <-  factor(RV_expiries, levels = RV_expiries)
        cone_df_RV <- cone_df_RV %>% mutate(across(min:current, ~as.numeric(.x)))
        
        cone_df_IV$RV <- cone_df_RV$current
        cone_df_RV$IV <- cone_df_IV$current
        
        p1 <- plot_ly(cone_df_IV, x = ~horizon) %>%
            add_ribbons(ymin = ~min, ymax = ~max,
                        name = "Min–Max",
                        line = list(width = 0),
                        fillcolor = "rgba(0,0,255,0.2)",
                        visible=FALSE
            ) %>%
            add_ribbons(ymin = ~q25, ymax = ~q75,
                        name = "IQR (25–75%)",
                        line = list(width = 0),
                        fillcolor = "rgba(0,0,255,0.3)") %>%
            add_trace(
                y = ~current,
                line=list(color='blue'),marker=list(color='blue'),
                name = "IV",
                type = "scatter",
                mode = "lines+markers"
            )%>%
            add_trace(
                y = ~RV,
                line=list(color='red'),marker=list(color='red'),
                name = "RV",
                type = "scatter",
                mode = "lines+markers"
            )%>%
            layout(
                yaxis = list(title = "Implied Volatility (%)"),
                xaxis = list(title = "")
            )
        
        
        p2 <- plot_ly(cone_df_RV, x = ~horizon) %>%
            add_ribbons(ymin = ~min, ymax = ~max,
                        name = "Min–Max",
                        line = list(width = 0),
                        fillcolor = "rgba(255,0,0,0.2)",
                        visible=FALSE) %>%
            add_ribbons(ymin = ~q25, ymax = ~q75,
                        name = "IQR (25–75%)",
                        line = list(width = 0),
                        fillcolor = "rgba(255,0,0,0.3)") %>%
            add_trace(
                y = ~IV,
                line=list(color='blue'),marker=list(color='blue'),
                name = "IV",
                type = "scatter",
                mode = "lines+markers"
            )%>%
            add_trace(
                y = ~current,
                line=list(color='red'),marker=list(color='red'),
                name = "RV",
                type = "scatter",
                mode = "lines+markers"
            )%>%
            layout(
                yaxis = list(title = "Realized Volatility (%)"),
                xaxis = list(title = "")
            )
        p1 <- p1 %>% layout(
            updatemenus = list(
                list(
                    type = "buttons",
                    direction = "right",
                    x = 0.5,              # <-- center horizontally
                    xanchor = "center",
                    y = 1.12,             # <-- place above plot
                    yanchor = "top",
                    pad = list(l = 0, r = 0, t = 0, b = 0),  # <-- remove whitespace padding
                    buttons = list(
                        list(
                            label = "Hide Min–Max",
                            method = "restyle",
                            args = list("visible", list(FALSE, TRUE, TRUE))
                        ),
                        list(
                            label = "Show All",
                            method = "restyle",
                            args = list("visible", list(TRUE, TRUE, TRUE))
                        )
                    )
                )
            )
        )
        p <- subplot(
            p1, p2,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16), showlegend = FALSE,
            title = plot_title("Volatility cone - fixed 2y lookback, ignores Time Range")
        )
        p
        
    })
    
    output$ticker_plot_4 <- renderPlotly({
        t_vol_window <- as.character(input$t_vol_window)
        # Each IV tenor against the ORATS ex-earnings HV nearest its horizon.
        # The horizons do not match exactly: IV tenors are calendar days, HV
        # windows trading days, and ORATS has no 42d or 63d HV - so 10d IV
        # (~7 sessions) meets a 10-session RV, and 60d IV (~42 sessions) is
        # lagged 60 rows against a 60-session RV. 30d, 6m and 1y line up.
        # A zero HV is ORATS's missing value, not zero vol.
        hv <- function(x) na_if(x, 0)
        df <- ticker_df() %>%
            mutate(
                # realized: today's IV against the RV of the n sessions that
                # FOLLOW it (the n-session HV printed n rows later), dated on
                # the IV's day. So the line stops n sessions before today.
                VRP_10 = log(exErnIv10d / lead(hv(clsHvXern10d), 10)),
                VRP_30 = log(exErnIv30d / lead(hv(clsHvXern20d), 20)),
                VRP_60 = log(exErnIv60d / lead(hv(clsHvXern60d), 60)),
                VRP_90 = log(exErnIv90d / lead(hv(clsHvXern60d), 60)), # there is no RV for 40 days
                VRP_120 = log(exErnIv6m / lead(hv(clsHvXern120d), 120)),
                VRP_252 = log(exErnIv1yr / lead(hv(clsHvXern252d), 252)),
                # now: today's IV against the trailing RV, same pairing
                NOW_10 = log(exErnIv10d / hv(clsHvXern10d)),
                NOW_30 = log(exErnIv30d / hv(clsHvXern20d)),
                NOW_60 = log(exErnIv60d / hv(clsHvXern60d)),
                NOW_90 = log(exErnIv90d / hv(clsHvXern60d)),
                NOW_120 = log(exErnIv6m / hv(clsHvXern120d)),
                NOW_252 = log(exErnIv1yr / hv(clsHvXern252d))) %>%
            mutate(across(c(starts_with("VRP_"), starts_with("NOW_")),
                          ~ if_else(is.finite(.x), .x, NA_real_)))

        # leads need the full history; trim only once they are computed
        df <- t_win(df)

        h <- switch(t_vol_window, "30d" = "30", "60d" = "60", "90d" = "90",
                    "6m" = "120", "1yr" = "252")
        zero <- list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
                     line = list(color = "gray40", dash = "dot", width = 1))

        p1 <- plot_ly(df, x = ~tradeDate) %>%
            add_lines(y = df[[paste0("VRP_", h)]], name = "realized (IV today / RV over the next n sessions)",
                      line = list(color = "darkorange"),
                      hovertemplate = "%{x}<br>realized %{y:.2f}<extra></extra>") %>%
            add_lines(y = df[[paste0("NOW_", h)]], name = "now (IV today / RV today)",
                      line = list(color = "steelblue"),
                      hovertemplate = "%{x}<br>now %{y:.2f}<extra></extra>") %>%
            layout(xaxis = list(title = ""),
                   yaxis = list(title = paste0("log(IV / RV), ", t_vol_window)),
                   shapes = list(zero))

        last_fin <- function(x) { x <- x[is.finite(x)]; if (length(x)) x[length(x)] else NA_real_ }
        hs <- c("10", "30", "60", "90", "120", "252")
        # band: 25-75% of the realized premium over the displayed window
        vrp_term_structure <- tibble(
            horizon = factor(hs, levels = hs),
            q25 = sapply(hs, function(k) quantile(df[[paste0("VRP_", k)]], 0.25, na.rm = TRUE, names = FALSE)),
            q75 = sapply(hs, function(k) quantile(df[[paste0("VRP_", k)]], 0.75, na.rm = TRUE, names = FALSE)),
            current = sapply(hs, function(k) last_fin(df[[paste0("VRP_", k)]])),
            now = sapply(hs, function(k) last_fin(df[[paste0("NOW_", k)]])))

        p2 <- plot_ly(vrp_term_structure, x = ~horizon) %>%
            add_ribbons(ymin = ~q25, ymax = ~q75,
                        name = "realized IQR (25-75%)",
                        line = list(width = 0),
                        fillcolor = "rgba(0,255,0,0.3)",
                        hovertemplate = "%{x}: IQR<extra></extra>") %>%
            add_trace(y = ~current, name = "realized, latest complete",
                      line = list(color = "darkorange"), marker = list(color = "darkorange"),
                      type = "scatter", mode = "lines+markers",
                      hovertemplate = "%{x} realized %{y:.2f}<extra></extra>") %>%
            add_trace(y = ~now, name = "now, last",
                      line = list(color = "steelblue"), marker = list(color = "steelblue"),
                      type = "scatter", mode = "lines+markers",
                      hovertemplate = "%{x} now %{y:.2f}<extra></extra>") %>%
            layout(xaxis = list(title = ""),
                   yaxis = list(title = "log(IV / RV)"),
                   shapes = list(zero))

        p <- subplot(
            p1, p2,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06
        ) %>% layout(
            font = list(size = 16), showlegend = TRUE,
            legend = list(orientation = "h", y = -0.12),
            title = plot_title(paste0("Volatility risk premium - level and term structure | ",
                                      "ORATS HV: 10 and 60 horizons only approximately matched"))
        )
        p

    })
    
    output$ticker_plot_5 <- renderPlotly({
        df <- ticker_df() %>%
            mutate(
                iv_hv_ratio = (exErnIv30d / clsHvXern20d) %>% log,
                iv_hv_ratio_pct = runPercentRank(roll_safe(iv_hv_ratio), 252) * 100,
                volOfIvol_w = ew_sd_roll(c(NA, diff(log(exErnIv30d))) %>% replace_na(0), 20) *100,
                IVVVOL_ratio = exErnIv30d / volOfIvol_w,
                IVVVOL_ratio_pct = runPercentRank(roll_safe(IVVVOL_ratio), 252)

            ) %>% t_win   # 252d ranks computed on the full history first

        #### IV / IVVOL ratio
        p1_1 <- plot_ly(df ,
                        x = ~tradeDate) %>%
            add_lines(y = ~IVVVOL_ratio, name = "IV/VVOL ratio", line = list(color = "cyan3")) %>%
            layout(
                xaxis = list(title = ""), yaxis = list(title = "IV/VVOL ratio"),font = list(size = 16)
            )
        p1_2 <- plot_ly(df ,
                        x = ~tradeDate) %>%
            add_lines(y = ~IVVVOL_ratio_pct, name = "IV/VVOL ratio pct", line = list(color = "cyan3")) %>%
            layout(
                xaxis = list(title = ""), yaxis = list(title = "IV/VVOL ratio pct"),font = list(size = 16)
            )
        p1 <- subplot(
            p1_1, p1_2,
            nrows = 2,
            shareX = TRUE,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16),showlegend = FALSE
        )
        
        ### IV / RV ratio
        p2_1 <- plot_ly(df ,
                        x = ~tradeDate) %>%
            add_lines(y = ~iv_hv_ratio, name = "IV/RV ratio", line = list(color = "blue")) %>%
            layout(
                xaxis = list(title = ""), yaxis = list(title = "IV/RV ratio"),font = list(size = 16)
            )
        p2_2 <- plot_ly(df ,
                        x = ~tradeDate) %>%
            add_lines(y = ~iv_hv_ratio_pct, name = "IV/RV ratio pct", line = list(color = "cyan3")) %>%
            layout(
                xaxis = list(title = ""), yaxis = list(title = "IV/RV ratio pct"),font = list(size = 16)
            )
        p2 <- subplot(
            p2_1, p2_2,
            nrows = 2,
            shareX = TRUE,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16),showlegend = FALSE
        )

        p <- subplot(
            p1, p2,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16), showlegend = FALSE,
            title = plot_title("IV/VVOL and IV/RV ratios, with 252d percentiles")
        )
        
        p
        
    })
    
    output$ticker_plot_6 <- renderPlotly({
        df <- ticker_df()

        df_ff <- df %>% mutate(
            # ORATS prints 0 for a missing forward vol (8% of rows)
            ff_60_30 = exErnIv30d / na_if(fexErn60_30, 0) - 1,
            ff_90_30 = exErnIv30d / na_if(fexErn90_30, 0) - 1,
            ff_90_60 = exErnIv60d / na_if(fexErn90_60, 0) - 1,
            ff_180_90 = exErnIv90d / na_if(fexErn180_90, 0) - 1
        ) %>% t_win

        p1 <- plot_ly(df_ff %>% tail(1) %>% dplyr::select(ticker,ff_60_30:ff_180_90) %>% pivot_longer(-ticker) %>% 
            mutate(name=factor(name, levels=c("ff_60_30", "ff_90_30", "ff_90_60", "ff_180_90"))),
        x = ~name, y = ~value, type = "bar") %>%
            layout(
                shapes = list(
                    # x1 = Inf serialises to null and the line never drew
                    list(type = "line", xref = "paper",
                         x0 = 0, x1 = 1,
                         y0 = 0.2, y1 = 0.2,
                         line = list(color = "red", dash = "dash")),
                    list(type = "line", xref = "paper",
                         x0 = 0, x1 = 1,
                         y0 = -0.2, y1 = -0.2,
                         line = list(color = "red", dash = "dash"))
                ),
                xaxis = list(title = ""), yaxis = list(title = "Forward Factor"),
                legend = list(x = 0.1, y = 0.9),font = list(size = 16)
            ) 
        p2 <- plot_ly(
            df_ff,
            x = ~tradeDate,
            y = ~ff_60_30,
            type = "scatter",
            mode = "lines+markers",
            hovertemplate = "Date: %{x}<br>FF: %{y:,}<extra></extra>"
        ) %>%
            layout(
                shapes = list(
                    list(type = "line",
                         x0 = min(df_ff$tradeDate), x1 = max(df_ff$tradeDate),
                         y0 = 0.2, y1 = 0.2,
                         line = list(color = "red", dash = "dash")),
                    list(type = "line",
                         x0 = min(df_ff$tradeDate), x1 = max(df_ff$tradeDate),
                         y0 = -0.2, y1 = -0.2,
                         line = list(color = "red", dash = "dash"))
                ),
                xaxis = list(title = ""),
                yaxis = list(title = "FF_60_30")
            )
        p <- subplot(
            p1, p2,
            #shareX = TRUE,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16), showlegend = FALSE,
            title = plot_title("Forward volatility factors")
        )
        p
    })
    
    output$ticker_plot_7 <- renderPlotly({
        t_dte <- t_dte_num()
        t_profit <- input$t_profit
        df <- ticker_df()

        # was a hardcoded 1y tail; now follows the Time Range selector
        df <- df %>% t_win %>% mutate(trace_days = factor(round(as.numeric(last(tradeDate)-tradeDate)/90)))
        
        today_dot <- df %>% tail(1)
        p1 <-  plot_ly( df,
            x = ~rvPctile1y,
            y = ~VRP,
            text = ~tradeDate,
            type = "scatter",
            mode = "markers",
            hovertemplate = paste(
                "Date: %{text}<br>"
            ),
            marker = list(
                size=12,
                color = ~trace_days,
                colorscale = "Blues",
                colorbar = list(title = "Value")
            )
        ) %>% add_markers(
            x = today_dot$rvPctile1y,
            y = today_dot$VRP,
            marker = list(size = 18,color = "orange", line = list(color = "black",  width = 2  )) 
        )
        
        df <- df %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(
            expiryDate1 = case_when(dtExM1 > 0 ~ tradeDate + dtExM1 - 1, TRUE ~ NA),
            expiryDate2 = case_when(dtExM2 > 0 ~ tradeDate + dtExM2 - 1, TRUE ~ NA),
            .after = tradeDate)
        
        df <- df %>% arrange(tradeDate) %>% mutate(pxAtmIvM1 = pxAtmIv[match(expiryDate1, tradeDate)], pxAtmIvM2 = pxAtmIv[match(expiryDate2, tradeDate)] ,.after = pxAtmIv) %>% ungroup
        df <- df %>% arrange(tradeDate) %>% mutate(pxAtmIvM1 = case_when(is.na(pxAtmIvM1) ~ pxAtmIv[match(expiryDate1-1, tradeDate)], TRUE ~ pxAtmIvM1), pxAtmIvM2 = case_when(is.na(pxAtmIvM2) ~ pxAtmIv[match(expiryDate2-1, tradeDate)], TRUE ~ pxAtmIvM2),.after = pxAtmIv) %>% ungroup
        # A straddle held across a split settles an unadjusted strike against
        # a price on the new share basis. Exclude those by the split sessions
        # split_adjust() located - known at entry, unlike the old filter here,
        # which dropped any trade whose underlying moved more than ~10% x
        # sqrt(days) by expiry: a gate on the outcome that deleted the best
        # genuine long-straddle results along with the splits.
        jumps <- attr(ticker_df()$pxAdj, "jumps")
        spans <- function(from, to) vapply(seq_along(from), function(i)
            is.finite(to[i]) && any(jumps > from[i] & jumps <= to[i]), NA)
        df <- df %>% mutate(span1 = spans(tradeDate, expiryDate1),
                            span2 = spans(tradeDate, expiryDate2))
        df <- df %>% mutate(
            straProM1 = abs(pxAtmIvM1 - hiStrikeM1) - straPxM1, 
            straProM2 = abs(pxAtmIvM2 - hiStrikeM2) - straPxM2,
            straRetM1 = straProM1 / pxAtmIv, 
            straRetM2 = straProM2 / pxAtmIv) %>%  
            mutate(
                straProM1 = case_when(span1 | straPxM1 > pxAtmIv*10 | straPxM1 == 0 | dtExM1 == 1 ~ NA, TRUE ~ straProM1),
                straProM2 = case_when(span2 | straPxM2 > pxAtmIv*10 | straPxM2 == 0 | dtExM2 == 1 ~ NA, TRUE ~ straProM2),
                straRetM1 = case_when(span1 | straPxM1 > pxAtmIv*10 | straPxM1 == 0 | dtExM1 == 1 ~ NA, TRUE ~ straRetM1),
                straRetM2 = case_when(span2 | straPxM2 > pxAtmIv*10 | straPxM2 == 0 | dtExM2 == 1 ~ NA, TRUE ~ straRetM2)#,
            )
        if(t_profit == "Percentage") {
            df <- df %>% filter(dtExM1 == t_dte) %>% 
                mutate(PnL1 = cumsum(replace_na(straRetM1, 0))*100, 
                       PnL2 = cumsum(replace_na(straRetM2, 0))*100)
        } else {
            df <- df %>% filter(dtExM1 == t_dte) %>% 
                mutate(PnL1 = cumsum(replace_na(straProM1, 0))*100,
                       PnL2 = cumsum(replace_na(straProM2, 0))*100)
        }

        # Sharpe of the two PnL lines, computed on the increments actually
        # drawn, so it always describes the curve on screen (periods with no
        # usable straddle enter as a flat 0, exactly as they do in the cumsum).
        # This series is one observation per expiry cycle at the chosen DTE,
        # not daily, so annualise from the observed sampling frequency rather
        # than assuming 252. Percentage and Dollars give slightly different
        # numbers on purpose: Percentage divides each period by that day's
        # price, so the two series are not proportional.
        sharpe <- function(pnl) {
            r <- diff(c(0, pnl)); r <- r[is.finite(r)]
            yrs <- as.numeric(diff(range(df$tradeDate))) / 365.25
            if (length(r) < 3 || yrs <= 0 || sd(r) == 0) return(NA_real_)
            mean(r) / sd(r) * sqrt(length(r) / yrs)
        }
        sr <- sprintf("Sharpe  M1 %.2f   M2 %.2f", sharpe(df$PnL1), sharpe(df$PnL2))

        p2 <- plot_ly(df,
            x = ~tradeDate) %>%
            add_lines(y = ~PnL1, name = "PnL1", line = list(color = "darkgray")) %>%
            add_lines(y = ~PnL2, name = "PnL2", line = list(color = "lightgray")) %>%
            layout(
                xaxis = list(title = ""),
                # both branches scale by 100: percent for returns, the 100x
                # contract multiplier for dollars. Name which one is on screen.
                yaxis = list(title = if (t_profit == "Percentage")
                                 "Straddle PnL (%)" else "Straddle PnL ($/contract)")
            )
        
        p <- subplot(
            p1, p2,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16), showlegend = FALSE,
            title = plot_title("VRP vs RV percentile | cumulative straddle PnL"),
            # Sharpe on the chart itself, over the PnL panel on the right
            annotations = list(list(
                text = sr, xref = "paper", yref = "paper",
                x = 1, xanchor = "right", y = 1.02, yanchor = "bottom",
                showarrow = FALSE, font = list(size = 14)))
        )
        p
    })
    
    
    # ---- Ticker tab: vol surface z-scores and skew history ----
    # The ex-earnings surface as a tenor x delta grid (see cols_to_extract for
    # the call-delta convention). ATM is the exErnIv column of the same tenor.
    surf_tenors  <- c("10d", "30d", "60d", "90d", "6m", "1y")
    surf_buckets <- c("5d call" = "exErnDlt5Iv", "25d call" = "exErnDlt25Iv", "ATM" = "exErnIv",
                      "25d put" = "exErnDlt75Iv", "5d put" = "exErnDlt95Iv")
    surf_col <- function(bucket, tenor)
        paste0(bucket, if (bucket == "exErnIv" && tenor == "1y") "1yr" else tenor)
    # TTR's percent rank errors on a series with no values at all, which a
    # ticker whose wings ORATS never filled would hand it
    pct252 <- function(x) tryCatch(runPercentRank(roll_safe(x), 252) * 100,
                                   error = function(e) rep(NA_real_, length(x)))
    # display smoothing for the skew percentiles: day-to-day wing quotes are
    # noisy enough to make the raw rank saw-tooth. Applied after ranking, on
    # the full history, so the rank itself is unchanged.
    skew_ema_n <- 10
    ema_safe <- function(x) tryCatch(as.numeric(TTR::EMA(roll_safe(x), skew_ema_n)),
                                     error = function(e) rep(NA_real_, length(x)))

    output$ticker_plot_surface <- renderPlotly({
        df <- ticker_df()
        # the last day with a usable 30d smile; the provisional delayed row
        # carries no wing columns and would otherwise blank the grid
        ok <- which(is.finite(df$exErnIv30d) & is.finite(df$exErnDlt75Iv30d))
        validate(need(length(ok) > 0, "No ex-earnings surface for this ticker."))
        win <- df[max(1, max(ok) - 251):max(ok), ]
        # z of the last value against the trailing 252 rows that include it,
        # the same convention as runZscore; fewer than 126 points is no history
        zlast <- function(x) {
            if (sum(is.finite(x)) < 126 || !is.finite(last(x))) return(NA_real_)
            (last(x) - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
        }
        nT <- length(surf_tenors); nB <- length(surf_buckets)
        lvl <- lvl_z <- shp <- shp_z <- matrix(NA_real_, nT, nB)
        atm30 <- win$exErnIv30d
        for (i in seq_len(nT)) {
            atm <- win[[surf_col("exErnIv", surf_tenors[i])]]
            for (j in seq_len(nB)) {
                x <- win[[surf_col(surf_buckets[j], surf_tenors[i])]]
                lvl[i, j] <- last(x); lvl_z[i, j] <- zlast(x)
                # shape takes the level out: a wing against its own tenor's
                # ATM (skew), the ATM column against 30d ATM (term structure)
                s <- if (surf_buckets[j] == "exErnIv") {
                    if (surf_tenors[i] == "30d") NULL else x - atm30
                } else x - atm
                if (!is.null(s)) { shp[i, j] <- last(s); shp_z[i, j] <- zlast(s) }
            }
        }
        heat <- function(z, val, unit, show_scale) {
            xs <- names(surf_buckets)
            g <- expand.grid(i = seq_len(nT), j = seq_len(nB))
            lab <- ifelse(is.na(z[as.matrix(g)]), "",
                          sprintf("%s<br>%+.1f z", sprintf(unit, val[as.matrix(g)]), z[as.matrix(g)]))
            plot_ly(x = xs, y = surf_tenors, z = pmax(pmin(z, 3), -3), type = "heatmap",
                    zmin = -3, zmax = 3, zmid = 0, colorscale = "RdBu", reversescale = TRUE,
                    showscale = show_scale, colorbar = list(title = "z, 252d"),
                    customdata = round(z, 2),
                    hovertemplate = "%{y} %{x}<br>z %{customdata}<extra></extra>") %>%
                add_annotations(x = xs[g$j], y = surf_tenors[g$i], text = lab,
                                showarrow = FALSE, font = list(size = 12)) %>%
                layout(xaxis = list(title = "", side = "top"),
                       yaxis = list(title = "", categoryorder = "array",
                                    categoryarray = rev(surf_tenors)))
        }
        subplot(heat(lvl_z, lvl, "%.1f", FALSE),
                heat(shp_z, shp, "%+.1f", TRUE),
                margin = 0.04) %>%
            layout(font = list(size = 14),
                   title = plot_title(paste0(
                       "Vol surface z-scores, ", format(win$tradeDate[nrow(win)]),
                       ": level (left) | shape (right: wing minus same-tenor ATM; ATM column minus 30d ATM)")),
                   margin = list(t = 90))
    })

    output$ticker_plot_skew <- renderPlotly({
        tn <- sub("1yr", "1y", as.character(input$t_vol_window))
        df <- ticker_df()
        atm  <- df[[surf_col("exErnIv", tn)]]
        c25  <- df[[paste0("exErnDlt25Iv", tn)]]
        p25  <- df[[paste0("exErnDlt75Iv", tn)]]
        p5   <- df[[paste0("exErnDlt95Iv", tn)]]
        df <- df %>%
            mutate(rr = p25 - c25,                      # 25d risk reversal, put over call
                   fly = (p25 + c25) / 2 - atm,          # 25d butterfly
                   pwing = p5 - p25,                     # put-tail convexity
                   # ranked on skew / ATM: in a vol spike the vol-point skew
                   # widens with the level, which would read as rich skew
                   rr_pct = ema_safe(pct252(rr / atm)),
                   fly_pct = ema_safe(pct252(fly / atm)),
                   pwing_pct = ema_safe(pct252(pwing / atm))) %>%
            t_win   # 252d ranks computed on the full history first
        validate(need(any(is.finite(df$rr)), paste("No", tn, "skew for this ticker.")))
        cols <- c(rr = "#d62728", fly = "#1f77b4", pwing = "#9467bd")
        nm   <- c(rr = "25d RR (put - call)", fly = "25d fly", pwing = "put wing (5d - 25d put)")
        p1 <- plot_ly(df, x = ~tradeDate)
        p2 <- plot_ly(df, x = ~tradeDate)
        for (k in names(cols)) {
            p1 <- p1 %>% add_lines(y = df[[k]], name = nm[[k]], legendgroup = k,
                                   line = list(color = cols[[k]]))
            p2 <- p2 %>% add_lines(y = df[[paste0(k, "_pct")]], name = nm[[k]], legendgroup = k,
                                   showlegend = FALSE, line = list(color = cols[[k]]))
        }
        p1 <- p1 %>% layout(yaxis = list(title = "vol pts"))
        p2 <- p2 %>% layout(yaxis = list(title = sprintf("pctile of skew/ATM, %dd EMA", skew_ema_n), range = c(0, 100)))
        lst <- df %>% dplyr::filter(is.finite(rr)) %>% tail(1)
        subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE, heights = c(0.55, 0.45),
                margin = 0.04) %>%
            layout(font = list(size = 14), legend = list(orientation = "h", y = 1.08),
                   title = plot_title(sprintf(
                       "%s ex-earnings skew | last: RR %+.1f, fly %+.1f, put wing %+.1f vol pts | percentiles smoothed with a %dd EMA",
                       tn, lst$rr, lst$fly, lst$pwing, skew_ema_n)),
                   margin = list(t = 80))
    })
    
    # ---- Chain tab ----
    # One fetch per ticker (debounced) or per Refresh click; every panel on
    # the tab reads the same cached frame.
    c_ticker_deb <- debounce(reactive(toupper(trimws(as.character(input$c_ticker)))), 600)
    chain_df <- reactive({
        input$c_refresh
        tk <- c_ticker_deb(); req(nzchar(tk))
        force <- isolate(input$c_refresh) > 0 && !is.null(chain_cache[[tk]]) &&
            difftime(Sys.time(), chain_cache[[tk]]$fetched, units = "secs") > 5
        res <- tryCatch(fetch_chain(tk, force = force), error = function(e) e)
        if (inherits(res, "error"))
            validate(need(FALSE, paste0("Could not fetch the chain for ", tk, ": ", conditionMessage(res))))
        res
    })
    chain_strip <- reactive(expiry_strip(chain_df()))
    # the ticker's last core row: realized vols and the 30d IV for context
    chain_core <- reactive({
        ORATS_core %>% ungroup() %>% dplyr::filter(ticker == c_ticker_deb()) %>%
            arrange(tradeDate) %>% slice_tail(n = 1) %>%
            mutate(across(c(clsHvXern20d, clsHvXern60d, clsHvXern252d, exErnIv30d), ~ na_if(.x, 0)))
    })
    chain_hv <- reactive({
        cc <- chain_core()
        if (!nrow(cc)) return(c(HV20 = NA_real_, HV60 = NA_real_, HV252 = NA_real_))
        # close-to-close ex-earnings, the same realized vol as every IV/RV
        # number in the app; orHvXern is a different estimator and sits a
        # median 4.5 vol pts away per ticker
        c(HV20 = cc$clsHvXern20d / 100, HV60 = cc$clsHvXern60d / 100, HV252 = cc$clsHvXern252d / 100)
    })

    # expiry choices follow the chain; default to the one nearest 30 days
    observeEvent(chain_strip(), {
        st <- chain_strip()
        ch <- setNames(as.character(st$expirDate), st$label)
        sel <- as.character(st$expirDate[which.min(abs(st$dte - 30))])
        updateSelectInput(session, "c_expiry", choices = ch, selected = sel)
    })
    chain_exp <- reactive({
        req(input$c_expiry)
        chain_df() %>% dplyr::filter(expirDate == as.Date(input$c_expiry)) %>% arrange(strike)
    })
    # strike choices follow the expiry; default to the ~25-delta put and call
    # for a strangle, ATM for a straddle, 30/15-delta for verticals
    observeEvent(list(chain_exp(), input$c_structure), {
        ce <- chain_exp(); req(nrow(ce) > 0)
        ks <- ce$strike
        pick <- function(target_delta) ce$strike[which.min(abs(ce$delta - target_delta))]
        atm <- pick(0.5)
        def <- switch(input$c_structure,
                      "Strangle"      = c(pick(0.75), pick(0.25)),   # put 25d = call delta 0.75
                      "Straddle"      = c(atm, atm),
                      "Call vertical" = c(pick(0.30), pick(0.15)),
                      "Put vertical"  = c(pick(0.85), pick(0.70)))   # put 15d / put 30d
        updateSelectInput(session, "c_k1", choices = ks, selected = def[1])
        updateSelectInput(session, "c_k2", choices = ks, selected = def[2])
    })

    output$chain_status <- renderUI({
        ch <- chain_df(); st <- chain_strip(); hv <- chain_hv(); cc <- chain_core()
        iv30 <- if (nrow(cc)) cc$exErnIv30d else NA
        pct  <- if (nrow(cc)) cc$ivPctile1y else NA
        HTML(sprintf(
            "<b>%s</b> spot <b>%.2f</b> &nbsp; quote %s UTC &nbsp;|&nbsp; core %s: IV30 %s (1y pctile %s), HV20 %s, HV60 %s, HV252 %s &nbsp;|&nbsp; %d expiries, %d strikes",
            c_ticker_deb(), ch$stockPrice[1], format(ch$quoteTime[1], "%Y-%m-%d %H:%M"),
            if (nrow(cc)) format(as.Date(cc$tradeDate)) else "n/a",
            if (is.finite(iv30)) sprintf("%.1f%%", iv30) else "n/a",
            if (is.finite(pct)) sprintf("%.0f", pct) else "n/a",
            if (is.finite(hv["HV20"])) sprintf("%.1f%%", 100 * hv["HV20"]) else "n/a",
            if (is.finite(hv["HV60"])) sprintf("%.1f%%", 100 * hv["HV60"]) else "n/a",
            if (is.finite(hv["HV252"])) sprintf("%.1f%%", 100 * hv["HV252"]) else "n/a",
            nrow(st), nrow(ch)))
    })

    output$chain_strip_plot <- renderPlotly({
        st <- chain_strip(); hv <- chain_hv()
        validate(need(nrow(st) > 1, "Fewer than two quoted expiries - nothing to draw."))
        st <- st %>% mutate(label = factor(label, levels = label))
        hline <- function(y, name, col) {
            if (!is.finite(y)) return(NULL)
            list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 100 * y, y1 = 100 * y,
                 line = list(dash = "dash", color = col, width = 1))
        }
        p1 <- plot_ly(st, x = ~label) %>%
            add_bars(y = ~100 * fwd_iv, name = "forward vol (from prior expiry)",
                     marker = list(color = "rgba(214,39,40,0.45)"),
                     text = ~sprintf("fwd %.1f%%", 100 * fwd_iv), textposition = "none",
                     hovertemplate = "%{x}<br>forward vol %{y:.1f}%<extra></extra>") %>%
            add_lines(y = ~100 * atm_iv, name = "ATM IV", line = list(color = "#1f77b4", width = 2),
                      hovertemplate = "%{x}<br>ATM IV %{y:.1f}%<extra></extra>") %>%
            add_markers(y = ~100 * atm_iv, name = "ATM IV", showlegend = FALSE,
                        marker = list(color = "#1f77b4", size = 8),
                        text = ~sprintf("K %s  straddle %.2f (%.1f%%)", strike, strad_mid, strad_pct),
                        hovertemplate = "%{x}<br>%{text}<extra></extra>") %>%
            layout(yaxis = list(title = "vol (%)"),
                   shapes = Filter(Negate(is.null), list(
                       hline(hv["HV20"], "HV20", "gray"),
                       hline(hv["HV252"], "HV252", "black"))),
                   annotations = Filter(Negate(is.null), list(
                       if (is.finite(hv["HV20"])) list(x = 1, xref = "paper", y = 100 * hv["HV20"],
                            text = "HV20", showarrow = FALSE, xanchor = "left", font = list(color = "gray")),
                       if (is.finite(hv["HV252"])) list(x = 1, xref = "paper", y = 100 * hv["HV252"],
                            text = "HV252", showarrow = FALSE, xanchor = "left"))))
        p2 <- plot_ly(st, x = ~label) %>%
            add_bars(y = ~strad_pct, name = "ATM straddle, % of spot",
                     marker = list(color = "rgba(31,119,180,0.5)"),
                     hovertemplate = "%{x}<br>straddle %{y:.2f}% of spot<extra></extra>") %>%
            layout(yaxis = list(title = "straddle % spot"))
        p3 <- plot_ly(st, x = ~label) %>%
            add_bars(y = ~hs_pct, name = "half-spread, % of premium",
                     marker = list(color = ~ifelse(hs_pct > 5, "rgba(214,39,40,0.7)", "rgba(44,160,44,0.6)")),
                     hovertemplate = "%{x}<br>half-spread %{y:.1f}% of premium<br>OI %{text}<extra></extra>",
                     text = ~format(oi, big.mark = ",")) %>%
            layout(yaxis = list(title = "half-spread %"))
        subplot(p1, p2, p3, nrows = 3, shareX = TRUE, heights = c(0.5, 0.25, 0.25), titleY = TRUE) %>%
            layout(title = plot_title(paste0(c_ticker_deb(), " expiry strip: ATM IV, forward vol, straddle cost, spread")),
                   legend = list(orientation = "h", y = 1.06), margin = list(t = 80)) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    output$chain_strip_tbl <- renderDT({
        chain_strip() %>%
            transmute(expiry = label, strike, `ATM IV %` = round(100 * atm_iv, 1),
                      `fwd IV %` = round(100 * fwd_iv, 1),
                      `straddle bid` = strad_bid, `straddle mid` = round(strad_mid, 2), `straddle ask` = strad_ask,
                      `% spot` = round(strad_pct, 2), `half-spread %` = round(hs_pct, 1),
                      `OI c/p` = paste0(format(callOpenInterest, big.mark = ","), " / ", format(putOpenInterest, big.mark = ",")),
                      volume = vol)
    }, options = list(pageLength = 15, dom = "t", scrollX = TRUE), rownames = FALSE)

    output$chain_ladder_plot <- renderPlotly({
        ce <- chain_exp(); req(nrow(ce) > 0)
        rng <- input$c_delta_rng
        S <- ce$stockPrice[1]
        lad <- ce %>% dplyr::filter(delta >= rng[1], delta <= rng[2])
        validate(need(nrow(lad) > 1, "No strikes inside the delta range."))
        p1 <- plot_ly(lad, x = ~strike) %>%
            add_lines(y = ~100 * na_if(callMidIv, 0), name = "call IV", line = list(color = "#2ca02c"),
                      text = ~sprintf("d %.2f  %.2f/%.2f", delta, callBidPrice, callAskPrice),
                      hovertemplate = "K %{x}<br>call IV %{y:.1f}%  %{text}<extra></extra>") %>%
            add_lines(y = ~100 * na_if(putMidIv, 0), name = "put IV", line = list(color = "#d62728"),
                      text = ~sprintf("d %.2f  %.2f/%.2f", putDelta, putBidPrice, putAskPrice),
                      hovertemplate = "K %{x}<br>put IV %{y:.1f}%  %{text}<extra></extra>") %>%
            layout(yaxis = list(title = "mid IV (%)"),
                   shapes = list(list(type = "line", x0 = S, x1 = S, y0 = 0, y1 = 1, yref = "paper",
                                      line = list(dash = "dot", color = "black"))))
        p2 <- plot_ly(lad, x = ~strike) %>%
            add_bars(y = ~callOpenInterest, name = "call OI", marker = list(color = "rgba(44,160,44,0.6)"),
                     hovertemplate = "K %{x}<br>call OI %{y:,}<extra></extra>") %>%
            add_bars(y = ~-putOpenInterest, name = "put OI", marker = list(color = "rgba(214,39,40,0.6)"),
                     text = ~format(putOpenInterest, big.mark = ","),
                     hovertemplate = "K %{x}<br>put OI %{text}<extra></extra>") %>%
            layout(barmode = "overlay", yaxis = list(title = "open interest (puts down)"),
                   shapes = list(list(type = "line", x0 = S, x1 = S, y0 = 0, y1 = 1, yref = "paper",
                                      line = list(dash = "dot", color = "black"))))
        subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(0.55, 0.45), titleY = TRUE) %>%
            layout(title = plot_title(paste0(c_ticker_deb(), " ", input$c_expiry, " (", ce$dte[1],
                                             "d): skew and open interest, spot ", sprintf("%.2f", S))),
                   legend = list(orientation = "h", y = 1.06), margin = list(t = 80)) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    # The no-arbitrage read of the ladder. Computed on the whole expiry - the
    # delta slider only decides what is DISPLAYED, and running the check on the
    # visible rows alone would both lose the tails and flag the two edge
    # strikes, whose neighbour is simply not in the slice.
    chain_ladder_chk <- reactive({
        ce <- chain_exp(); req(nrow(ce) > 0)
        ladder_checks(ce)
    })

    output$chain_ladder_checks <- renderUI({
        chk <- chain_ladder_chk()
        if (is.null(chk)) return(HTML("<p><i>Too few quoted strikes in this expiry to check.</i></p>"))
        bad <- sum(nzchar(chk$flag))
        # the denominator is what was checkable, not what was quoted
        n   <- sum(is.finite(chk$p_below) | is.finite(chk$density))
        if (!bad)
            return(HTML(sprintf("<p style='color:#0a7d33'><b>Ladder is arbitrage-free</b> across all %d checkable strikes.</p>", n)))
        HTML(sprintf(paste0("<p style='color:#b00020'><b>%d of %d checkable strikes violate a no-arbitrage bound</b> ",
                            "(%s). Those quotes are stale, crossed or absent - do not price a structure on them.</p>"),
                     bad, n, paste(sort(unique(unlist(strsplit(chk$flag[nzchar(chk$flag)], " +")))), collapse = ", ")))
    })

    output$chain_ladder_tbl <- renderDT({
        ce <- chain_exp(); req(nrow(ce) > 0)
        rng <- input$c_delta_rng
        chk <- chain_ladder_chk()
        out <- ce %>% dplyr::filter(delta >= rng[1], delta <= rng[2]) %>%
            transmute(strike,
                      `call bid/ask` = sprintf("%.2f / %.2f", callBidPrice, callAskPrice),
                      `call IV %` = round(100 * callMidIv, 1), `call OI` = callOpenInterest, `call vol` = callVolume,
                      delta = round(delta, 2),
                      `put bid/ask` = sprintf("%.2f / %.2f", putBidPrice, putAskPrice),
                      `put IV %` = round(100 * putMidIv, 1), `put OI` = putOpenInterest, `put vol` = putVolume)
        if (!is.null(chk))
            out <- out %>% left_join(
                chk %>% transmute(strike,
                                  `mid K` = round(spread_mid, 2),
                                  `P(<mid)` = round(p_below, 3),
                                  `P(>mid)` = round(p_above, 3),
                                  density = round(density, 4),
                                  flag),
                by = "strike") %>%
                # a strike the check could not reach (no two-sided quote) is not
                # a violation - blank it so the row styling leaves it alone
                mutate(flag = ifelse(is.na(flag), "", flag))
        dt <- DT::datatable(out, rownames = FALSE,
                            options = list(pageLength = 25, dom = "tp", scrollX = TRUE))
        if (!is.null(chk))
            dt <- dt %>% DT::formatStyle("flag", target = "row",
                                         backgroundColor = DT::styleEqual("", NA, default = "#ffe3e3"))
        dt
    })

    # ---- pricer ----
    chain_priced <- reactive({
        ce <- chain_exp(); req(nrow(ce) > 0, input$c_k1, input$c_k2)
        k1 <- as.numeric(input$c_k1); k2 <- as.numeric(input$c_k2)
        if (input$c_structure != "Straddle" && k1 > k2) { tmp <- k1; k1 <- k2; k2 <- tmp }
        legs <- structure_legs(input$c_structure, input$c_side, k1, k2)
        pr <- price_structure(ce, legs, input$c_side)
        validate(need(!is.null(pr), "One of the strikes is not in this expiry."))
        pr$k1 <- k1; pr$k2 <- k2; pr$legs_spec <- legs
        pr$S <- ce$stockPrice[1]; pr$T <- ce$dte[1] / 365; pr$dte <- ce$dte[1]
        atm <- ce[which.min(abs(ce$delta - 0.5)), ]
        pr$atm_iv <- (atm$callMidIv + atm$putMidIv) / 2
        pr
    })

    output$chain_pricer_legs <- renderDT({
        pr <- chain_priced()
        pr$legs %>% transmute(leg, bid, ask, mid = round(mid, 2), model = round(model, 2),
                              `IV %` = round(100 * iv, 1), delta = round(delta, 2), OI = oi)
    }, options = list(dom = "t"), rownames = FALSE)

    output$chain_pricer_summary <- renderUI({
        pr <- chain_priced(); hv <- chain_hv(); qty <- max(1, as.integer(input$c_qty))
        what <- if (pr$credit_side) "credit" else "debit"
        sig  <- pr$atm_iv * sqrt(pr$T)
        dist <- function(K) sprintf("%s: %+.1f%% (%.1f sigma at ATM IV)", K, 100 * (K / pr$S - 1),
                                    abs(log(K / pr$S)) / sig)
        strikes <- unique(pr$legs$strike)
        margin <- approx_margin(input$c_structure, input$c_side, pr$k1, pr$k2, pr$S, pr$mid)
        fg <- fair_grid(pr$legs_spec, pr$S, pr$T,
                        c(HV252 = unname(hv["HV252"]), mid = unname((hv["HV252"] + pr$atm_iv) / 2)),
                        credit_side = pr$credit_side)
        v_hv252 <- if ("HV252" %in% fg$assumption) fg$value[fg$assumption == "HV252"] else NA
        v_mid   <- if ("mid" %in% fg$assumption) fg$value[fg$assumption == "mid"] else NA
        # edge from the trader's side: a credit is worth taking above fair
        # value, a debit below it
        edge <- if (pr$credit_side) pr$mid - v_hv252 else v_hv252 - pr$mid
        hs   <- 100 * (pr$mid - pr$touch) / abs(pr$mid)
        # pr$theta is already signed from the trader's side (positive = decay earned)
        HTML(paste0(
            sprintf("<p style='font-size:16px'><b>%s %s %s %s</b> &nbsp; expiry %s (%dd), spot %.2f</p>",
                    input$c_side, input$c_structure, if (input$c_structure == "Straddle") pr$k1 else paste0(pr$k1, "/", pr$k2),
                    c_ticker_deb(), input$c_expiry, pr$dte, pr$S),
            sprintf("<p><b>%s: mid %.2f &nbsp; natural %.2f &nbsp; ORATS model %.2f</b> &nbsp; (half-spread %.1f%% of premium)</p>",
                    what, pr$mid, pr$touch, pr$model, hs),
            sprintf("<p>Per contract: delta %+.2f (%+.0f shares) &nbsp; gamma %+.3f &nbsp; theta %+.3f/day &nbsp; vega %+.3f &nbsp;|&nbsp; x%d: delta %+.0f shares, theta %+.0f $/day</p>",
                    pr$delta, 100 * pr$delta, pr$gamma, pr$theta, pr$vega,
                    qty, 100 * pr$delta * qty, 100 * pr$theta * qty),
            "<p>Strike distance: ", paste(sapply(strikes, dist), collapse = " &nbsp;|&nbsp; "), "</p>",
            # What the vertical's price says as a bet. Quoted at the touch too,
            # because the half-spread is paid out of exactly this: the odds you
            # are offered at mid are not the odds you get.
            local({
                if (!grepl("vertical", input$c_structure)) return("")
                om <- vertical_odds(input$c_structure, input$c_side, pr$k1, pr$k2, pr$mid, pr$T)
                ot <- vertical_odds(input$c_structure, input$c_side, pr$k1, pr$k2, pr$touch, pr$T)
                if (is.null(om)) return("")
                if (is.null(ot)) ot <- om
                if (!om$ok)
                    return(sprintf(paste0("<p style='color:#b00020'><b>This vertical is worth %.2f on a %.2f-wide ",
                                          "spread</b> - outside (0, width), so it is not a bet, it is a bad quote. ",
                                          "Check the ladder flags.</p>"), pr$mid, om$width))
                sprintf(paste0("<p><b>As a bet: risk %.2f to make %.2f - %.2f-to-1 that %s is %s %.2f</b> ",
                               "at expiry (the midpoint of the strikes, not the short strike); ",
                               "the market's implied probability of that is %.1f%%. ",
                               "At the touch the same bet pays <b>%.2f-to-1</b> (risk %.2f to make %.2f) - ",
                               "that gap is the half-spread.</p>"),
                        om$risk, om$win, om$win / om$risk, c_ticker_deb(), om$dir, om$mid_strike,
                        100 * om$p, ot$win / ot$risk, ot$risk, ot$win)
            }),
            if (is.finite(edge))
                sprintf("<p>Edge vs HV252 (%.1f%%): <b>%+.2f</b> per contract = $%.0f on ~$%.0f Reg-T margin (%.1f%% for %d days)%s</p>",
                        100 * hv["HV252"], edge, 100 * edge * qty, margin * qty,
                        100 * (100 * edge * qty) / max(margin * qty, 1), pr$dte,
                        if (pr$credit_side) sprintf(" &nbsp;|&nbsp; <b>ladder: post %.2f, floor %.2f (midpoint vol), no edge below %.2f (HV252)</b>",
                                                    pr$mid, v_mid, v_hv252) else "")
            else "<p>No HV252 in the core file for this ticker - fair-value grid uses the implied only.</p>"))
    })

    # P&L against spot for the structure as priced: at expiry, today, and
    # halfway there. Each leg keeps its own mid IV (sticky strike) and enters
    # at mid, so the today line passes near zero at spot - the gap is mid vs
    # Black-Scholes at that IV. Per-leg P&L is pos x (value - mid), so the
    # same sum serves both sides; x100 and x contracts for dollars.
    output$chain_payoff_plot <- renderPlotly({
        pr <- chain_priced(); qty <- max(1, as.integer(input$c_qty))
        lg <- pr$legs; S0 <- pr$S; T0 <- pr$T
        sd_move <- max(pr$atm_iv * sqrt(max(T0, 1 / 365)), 0.03)
        S <- seq(S0 * exp(-3 * sd_move), S0 * exp(3 * sd_move), length.out = 241)
        pnl_at <- function(t) {
            vapply(S, function(s) sum(lg$pos * (mapply(function(cp, K, iv)
                bs_price(s, K, t, CHAIN_RFR, if (is.finite(iv) && iv > 0) iv else pr$atm_iv, cp),
                lg$cp, lg$strike, lg$iv) - lg$mid)), 0) * 100 * qty
        }
        curves <- tibble(S = S, expiry = pnl_at(0), half = pnl_at(T0 / 2), today = pnl_at(T0))
        # break-evens at expiry: where the expiry line changes sign
        e <- curves$expiry; ix <- which(diff(sign(e)) != 0)
        be <- S[ix] - e[ix] * (S[ix + 1] - S[ix]) / (e[ix + 1] - e[ix])
        vline <- function(x, col, dash) list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
                                             line = list(color = col, dash = dash, width = 1))
        plot_ly(curves, x = ~S) %>%
            add_lines(y = ~expiry, name = "at expiry", line = list(color = "black", width = 2.5),
                      hovertemplate = "S %{x:.2f}<br>expiry %{y:$,.0f}<extra></extra>") %>%
            add_lines(y = ~half, name = sprintf("halfway (%dd left)", round(pr$dte / 2)),
                      line = list(color = "#7f7f7f", dash = "dash"),
                      hovertemplate = "S %{x:.2f}<br>halfway %{y:$,.0f}<extra></extra>") %>%
            add_lines(y = ~today, name = "today", line = list(color = "#1f77b4"),
                      hovertemplate = "S %{x:.2f}<br>today %{y:$,.0f}<extra></extra>") %>%
            layout(xaxis = list(title = "underlying at the horizon"),
                   yaxis = list(title = sprintf("P&L, $ (x%d, entered at mid)", qty), zeroline = TRUE),
                   shapes = c(list(vline(S0, "#1f77b4", "dot"),
                                   list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
                                        line = list(color = "gray40", width = 1))),
                              lapply(be, vline, col = "#d62728", dash = "dash")),
                   legend = list(orientation = "h", y = -0.18), font = list(size = 13),
                   title = plot_title(paste0(
                       "Payoff: ", input$c_side, " ", input$c_structure, " | spot ", sprintf("%.2f", S0),
                       if (length(be)) paste0(" | break-even ", paste(sprintf("%.2f (%+.1f%%)", be, 100 * (be / S0 - 1)), collapse = ", "))
                       else "", " | range = +-3 sd at ATM IV"))) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    output$chain_fair_tbl <- renderDT({
        pr <- chain_priced(); hv <- chain_hv()
        vols <- c(HV20 = unname(hv["HV20"]), HV60 = unname(hv["HV60"]), HV252 = unname(hv["HV252"]),
                  `midpoint (HV252, IV)` = unname((hv["HV252"] + pr$atm_iv) / 2),
                  `ATM implied` = pr$atm_iv)
        fair_grid(pr$legs_spec, pr$S, pr$T, vols, credit_side = pr$credit_side) %>%
            transmute(assumption, `vol %` = round(100 * vol, 1), value = round(value, 2),
                      `edge at mid` = round(if (pr$credit_side) pr$mid - value else value - pr$mid, 2))
    }, options = list(dom = "t"), rownames = FALSE)

    output$pairs_plot <- renderPlot({
        ticker_1 <- pair_1_deb()
        ticker_2 <- pair_2_deb()
        corr_window <- as.numeric(input$run_window)
        df1 <- ORATS_core %>% dplyr::filter(ticker == ticker_1) %>% ungroup %>% arrange(tradeDate) %>%
            mutate(retAtmIv = c(NA, diff(log(split_adjust(tradeDate, na_if(pxAtmIv, 0), ticker_1)))), retAtmIv = remove_outliers(retAtmIv) * 100) %>% 
            mutate(retIv = c(NA, diff(log(exErnIv30d))), retIv = remove_outliers(retIv) * 100) 
        df2 <- ORATS_core %>% dplyr::filter(ticker == ticker_2) %>% ungroup %>% arrange(tradeDate) %>%
            mutate(retAtmIv = c(NA, diff(log(split_adjust(tradeDate, na_if(pxAtmIv, 0), ticker_2)))), retAtmIv = remove_outliers(retAtmIv) * 100) %>% 
            mutate(retIv = c(NA, diff(log(exErnIv30d))), retIv = remove_outliers(retIv) * 100) 
        df <- inner_join(df1, df2, by="tradeDate") %>% arrange(tradeDate)
        df <- df %>% 
            mutate(run_corr_price = runCor(df$retAtmIv.x %>% replace_na(0), df$retAtmIv.y %>% replace_na(0), corr_window)) %>% 
            mutate(run_corr_iv = runCor(df$retIv.x %>% replace_na(0), df$retIv.y %>% replace_na(0), corr_window))
        req(nrow(df)>0)
        corr_price <- round(cor(df$retAtmIv.x, df$retAtmIv.y, use = "pairwise.complete.obs") * 100, 1)
        corr_iv <- round(cor(df$retIv.x, df$retIv.y, use = "pairwise.complete.obs") * 100, 1)

        p0_price <- ggplot(df %>% dplyr::select(tradeDate, pxAtmIv.x, pxAtmIv.y) %>% pivot_longer(-tradeDate) %>% 
                               mutate(name = recode(name, "pxAtmIv.x"=ticker_1, "pxAtmIv.y"=ticker_2)) %>% group_by(name) %>% 
                               mutate(scaled_price = scale(value))   , aes(tradeDate, scaled_price, color=name)) +  
            geom_line(linewidth=2)  +  ggtitle("Scaled Price") 
        p0_iv <- ggplot(df %>% dplyr::select(tradeDate, exErnIv30d.x, exErnIv30d.y) %>% pivot_longer(-tradeDate) %>% 
                               mutate(name = recode(name, "exErnIv30d.x"=ticker_1, "exErnIv30d.y"=ticker_2)) %>% 
                               mutate(IV = value) , aes(tradeDate, IV, color=name)) +  
            geom_line(linewidth=2)  +  ggtitle("IV") 
        
        p1 <- ggplot(df, aes(retAtmIv.x, retAtmIv.y)) +
            geom_point(color="blue") + geom_smooth(method="lm") + ggtitle("Returns Correlation")+
            xlab(ticker_1) + ylab(ticker_2) +  annotate(
                "label",
                x = Inf, y = -Inf,
                label = paste0("Corr = ", corr_price, "%"),
                hjust = 1.05, vjust = -0.5,
                size = 8
            )
        p2 <- ggplot(df, aes(tradeDate, run_corr_price)) + geom_line(color="blue", linewidth = 2) +
            geom_hline(yintercept = 0, linetype = "dashed") + ylim(c(-1,1)) + ylab("Running Corr") + xlab("") + ggtitle("Returns Correlation")
        
        p3 <- ggplot(df, aes(retIv.x, retIv.y)) + geom_point(color="blue") + geom_smooth(method="lm") + ggtitle("IV Correlation") + 
            xlab(ticker_1) + ylab(ticker_2)+  annotate(
                "label",
                x = Inf, y = -Inf,
                label = paste0("Corr = ", corr_iv, "%"),
                hjust = 1.05, vjust = -0.5,
                size = 8
            )
        p4 <- ggplot(df, aes(tradeDate, run_corr_iv)) + geom_line(color="blue", linewidth = 2) +
            geom_hline(yintercept = 0, linetype = "dashed") + ylim(c(-1,1)) + ylab("Running Corr") + xlab("") + 
            ggtitle("IV Correlation") 
        
        IV_expiries <- c("10d","30d","60d","90d","6m","1yr");
        df_iv <- df %>% group_by(tradeDate) %>% last() %>% ungroup %>% dplyr::select(contains("exErnIv"))
        df_iv <- df_iv %>% t %>% as.data.frame() %>% rownames_to_column() %>% 
            separate(rowname, sep="\\.", into=c("horizon", "ticker")) %>% rename(IV=V1) %>% 
            mutate(horizon = factor(sub("exErnIv", "", horizon), levels=IV_expiries),  ticker=case_when(ticker=="x" ~ ticker_1,ticker=="y" ~ ticker_2, TRUE~NA))
        p5 <- ggplot(df_iv, aes(horizon, IV, color=ticker, group=ticker)) + geom_line(linewidth=1.5) + geom_point(size=3)  + xlab("")
        
        RV_expiries <- c("10d","20d", "60d","90d","120d","252d")
        df_rv <- df %>% group_by(tradeDate) %>% last() %>% ungroup %>% dplyr::select(contains("clsHvXern"))
        df_rv <- df_rv %>% t %>% as.data.frame() %>% rownames_to_column() %>% 
            separate(rowname, sep="\\.", into=c("horizon", "ticker")) %>% rename(RV=V1) %>% 
            mutate(horizon = factor(sub("clsHvXern", "", horizon), levels=RV_expiries),  ticker=case_when(ticker=="x" ~ ticker_1,ticker=="y" ~ ticker_2, TRUE~NA))
        p6 <- ggplot(df_rv, aes(horizon, RV, color=ticker, group=ticker)) + geom_line(linewidth=1.5) + geom_point(size=3)  + xlab("")
        
        
        (p0_price / p0_iv) / (p1 + p2) / (p3 + p4) / (p5 + p6) 

        
    })
    
    
    output$corr_plot <- renderPlot({
        ticker_1 <- pair_1_deb()
        ticker_2 <- pair_2_deb()

        cor_mat <- returns_cor_mat()   # memoized: heavy, input-independent
        req(ticker_1 %in% colnames(cor_mat), ticker_2 %in% colnames(cor_mat))
        # corr mat ticker 1
        ticker_col <- cor_mat[,ticker_1] %>% sort(decreasing = TRUE)
        ticker_closest <- names(ticker_col[c(1:25, (length(ticker_col)-25):length(ticker_col))]) 
        cor_mat_closest1 <- cor_mat[ticker_closest,ticker_closest %>% rev]
        # corr mat ticker 1
        ticker_col <- cor_mat[,ticker_2] %>% sort(decreasing = TRUE)
        ticker_closest <- names(ticker_col[c(1:25, (length(ticker_col)-25):length(ticker_col))])
        cor_mat_closest2 <- cor_mat[ticker_closest,ticker_closest %>% rev]
        p7 <-  ggcorrplot(cor_mat_closest1, show.legend = FALSE, type = "upper") 
        p8 <-  ggcorrplot(cor_mat_closest2, show.legend = FALSE, type = "upper")
        (p7 + p8)
    })
    # ---------------------------------------------------------------------
    # VIX tab
    #
    # Native plot_ly throughout, for the reason already noted on screen_plot:
    # ggplotly() is broken against this app's ggplot2.
    # ---------------------------------------------------------------------
    vix_lb <- reactive(as.numeric(input$v_lookback))

    # ranks are recomputed on the chosen lookback, not sliced from a fixed one:
    # a 1y percentile and a 3y percentile of the same ratio are different
    # statements, and the sidebar is choosing which statement to make
    vix_d <- reactive(build_vix_derived(vix_complex, lookback = vix_lb()))

    vix_start <- reactive({
        m <- as.numeric(input$v_range)
        if (m == 0) min(vix_complex$date) else max(vix_complex$date) - m * 30.44
    })
    vix_win <- reactive(vix_d() %>% dplyr::filter(date >= vix_start()))

    vix_axis <- function(log_ok = FALSE)
        list(title = "", type = if (log_ok && isTRUE(input$v_log)) "log" else "linear")

    # The four full-width plots are read down the page as one picture, so their
    # panels have to start and end at the same pixel. Two things break that on
    # their own: plotly sizes the left margin from whatever tick labels a plot
    # happens to have ("100" is wider than "1.4"), and the cross-product panel
    # is limited by the ORATS window while the others run on the CBOE history.
    # So the margin is pinned and the date range is pinned - the cross panel
    # then shows its short history as empty space on the left, which is the
    # honest way to draw it rather than silently rescaling.
    VIX_MARGIN <- list(l = 72, r = 30, t = 68, b = 44)
    vix_xrange <- reactive(c(format(as.Date(vix_start())),
                             format(max(vix_complex$date))))
    vix_datex  <- function() list(title = "", range = vix_xrange())

    # --- today's reading of everything, which is the whole point of the tab
    output$vix_tbl_monitor <- DT::renderDataTable({
        d <- vix_d(); last <- d %>% dplyr::slice_tail(n = 1)
        # VIX and VVIX moved into VIX_RATIOS, where they get a percentile and a
        # z; listing them here as well would print each of them twice
        plain <- setdiff(VIX_ALL, VIX_RATIOS$key)
        lev <- tibble::tibble(
            group = "level", what = plain,
            value = purrr::map_dbl(plain, ~ as.numeric(last[[.x]])),
            pct = NA_real_, z = NA_real_)
        rat <- VIX_RATIOS %>% dplyr::transmute(
            group, what = label,
            value = purrr::map_dbl(key, ~ as.numeric(last[[.x]])),
            pct   = purrr::map_dbl(key, ~ as.numeric(last[[paste0(.x, "_pct")]])),
            z     = purrr::map_dbl(key, ~ as.numeric(last[[paste0(.x, "_z")]])))
        out <- dplyr::bind_rows(lev, rat) %>%
            dplyr::mutate(dplyr::across(c(value, pct, z), ~ round(.x, 2)))
        DT::datatable(out, rownames = FALSE,
                      options = list(pageLength = 20, dom = "t"),
                      colnames = c("", "series", "level / ratio",
                                   paste0("pctile (", vix_lb(), "d)"), "z")) %>%
            # colour the tails only: the middle of the range is not information
            DT::formatStyle("pct", backgroundColor = DT::styleInterval(
                c(5, 20, 80, 95),
                c("#f4a582", "#fddbc7", "white", "#d1e5f0", "#92c5de")))
    })

    # --- today's curve in the context of its own history
    #
    # The first version drew min/q25/median/q75/max as five LINES and they came
    # out jagged, for two compounding reasons. (1) A pointwise extreme is not a
    # curve: VIX1D's maximum and VIX3M's maximum happen on different days, so
    # joining them draws a path through six unrelated sessions and it wanders.
    # (2) On a long window the series do not even share a sample - VIX1D starts
    # 2022 and VIX9D 2011 against VIX's 1990 - so the "max" line was comparing
    # one era's print with another's and zig-zagged between them.
    #
    # So: restrict to days where the WHOLE curve exists, draw the pointwise
    # spread as a shaded envelope (which is what it honestly is), and let the
    # only lines be curves actually observed on a single session - today, and
    # the days whose VIX sat at the 10th / 50th / 90th percentile of the window.
    output$vix_plot_curve <- renderPlotly({
        d <- vix_win() %>% dplyr::select(date, dplyr::all_of(VIX_TERM)) %>%
            tidyr::drop_na()
        validate(need(nrow(d) > 20, "not enough days carry the whole curve"))
        m <- as.matrix(d[VIX_TERM])
        band <- function(p) as.numeric(apply(m, 2, quantile, probs = p, na.rm = TRUE))
        # the curve as it stood on the day whose VIX sat at that percentile
        day <- function(p) as.numeric(m[which.min(abs(d$VIX -
                               quantile(d$VIX, p, na.rm = TRUE))), ])
        now <- as.numeric(vix_d() %>% dplyr::slice_tail(n = 1) %>%
                              dplyr::select(dplyr::all_of(VIX_TERM)))

        # x is the tenor's POSITION, not its name. A character x makes plotly
        # sort the categories alphabetically - VIX, VIX1D, VIX1Y, VIX3M, VIX6M,
        # VIX9D - and while categoryorder="array" puts the axis back in tenor
        # order, the line is still drawn in trace order, so it crosses itself
        # 3-1-6-4-5-2 across the page. Numeric x with tick labels removes the
        # categorical axis, and with it the reordering.
        xi <- seq_along(VIX_TERM)
        ax <- list(title = "", tickmode = "array", tickvals = xi,
                   ticktext = VIX_TERM, range = c(0.85, length(xi) + 0.15))

        plot_ly() %>%
            # the envelope first, so every line draws over it
            add_lines(x = xi, y = band(0.05), name = "5-95 pctile (per tenor)",
                      legendgroup = "band", line = list(color = "transparent"),
                      hoverinfo = "skip", showlegend = FALSE) %>%
            add_lines(x = xi, y = band(0.95), name = "5-95 pctile (per tenor)",
                      legendgroup = "band", line = list(color = "transparent"),
                      fill = "tonexty", fillcolor = "rgba(148,163,184,0.28)",
                      hoverinfo = "skip") %>%
            add_lines(x = xi, y = day(0.10), name = "a calm day (VIX p10)",
                      line = list(color = "#94a3b8", width = 1.4, dash = "dot"),
                      hovertemplate = "%{y:.2f}<extra>calm day</extra>") %>%
            add_lines(x = xi, y = day(0.50), name = "a median day (VIX p50)",
                      line = list(color = "#475569", width = 1.8),
                      hovertemplate = "%{y:.2f}<extra>median day</extra>") %>%
            add_lines(x = xi, y = day(0.90), name = "a stressed day (VIX p90)",
                      line = list(color = "#94a3b8", width = 1.4, dash = "dash"),
                      hovertemplate = "%{y:.2f}<extra>stressed day</extra>") %>%
            add_trace(x = xi, y = now, name = "today", type = "scatter",
                      mode = "lines+markers",
                      line = list(color = "#c2410c", width = 3),
                      marker = list(color = "#c2410c", size = 9),
                      hovertemplate = "%{y:.2f}<extra>today</extra>") %>%
            layout(xaxis = ax, yaxis = vix_axis(TRUE), hovermode = "x unified",
                   font = list(size = 14),
                   legend = list(orientation = "h", x = 0, xanchor = "left",
                                 y = -0.12),
                   title = plot_title(paste0("VIX curve in context  (",
                                             nrow(d), " days with the full curve)"))) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    # --- cross-asset implied vol: is this an equity event, or everyone's?
    #
    # Two panes on one date axis. Levels on top, because "OVX at 56 while VIX
    # sits at 16" is itself the reading; percentiles below, ranked on the
    # sidebar lookback like everything else, for "is each one high for itself".
    #
    # Okabe-Ito hues, so the five stay apart under colour-blindness. Even so the
    # two closest pairs (OVX/VXEEM for deutans, GVZ/VIX in normal vision) sit
    # near the floor, so one of each pair is also dashed - identity never rests
    # on hue alone.
    XASSET_COL  <- c(VIX = "#D55E00", VXTLT = "#0072B2", GVZ = "#E69F00",
                     OVX = "#009E73", VXEEM = "#CC79A7")
    XASSET_DASH <- c(VIX = "solid", VXTLT = "solid", GVZ = "dash",
                     OVX = "solid", VXEEM = "dot")
    output$vix_plot_xasset <- renderPlotly({
        d  <- vix_win()
        xa <- VIX_XASSET %>% dplyr::filter(key %in% names(d))
        validate(need(nrow(xa) > 1, "cross-asset vol indices not in the cache"))
        last <- vix_d() %>% dplyr::slice_tail(n = 1)
        # today's level and percentile ride in the legend, so the pane answers
        # "where is each one now" without hovering
        lab <- purrr::pmap_chr(xa, function(key, label, ...) {
            v <- last[[key]]; p <- last[[paste0(key, "_pct")]]
            sprintf("%s  %s · p%s", label,
                    if (is.finite(v)) sprintf("%.1f", v) else "-",
                    if (is.finite(p)) sprintf("%.0f", p) else "-")
        })
        top <- plot_ly(); bot <- plot_ly()
        for (i in seq_len(nrow(xa))) {
            k <- xa$key[i]; col <- XASSET_COL[[k]]; dsh <- XASSET_DASH[[k]]
            top <- top %>% add_lines(x = d$date, y = d[[k]], name = lab[i],
                                     legendgroup = k, connectgaps = FALSE,
                                     line = list(color = col, width = 1.3, dash = dsh),
                                     hovertemplate = paste0("%{y:.1f}<extra>", k, "</extra>"))
            bot <- bot %>% add_lines(x = d$date, y = d[[paste0(k, "_pct")]],
                                     name = lab[i], legendgroup = k, showlegend = FALSE,
                                     line = list(color = col, width = 1.3, dash = dsh),
                                     hovertemplate = paste0("%{y:.0f}<extra>", k, "</extra>"))
        }
        bot <- bot %>%
            add_lines(x = d$date, y = rep(5, nrow(d)), showlegend = FALSE, hoverinfo = "skip",
                      line = list(color = "#b3b3b3", width = 0.8, dash = "dot")) %>%
            add_lines(x = d$date, y = rep(95, nrow(d)), showlegend = FALSE, hoverinfo = "skip",
                      line = list(color = "#b3b3b3", width = 0.8, dash = "dot"))
        subplot(top %>% layout(yaxis = vix_axis(TRUE)),
                bot %>% layout(yaxis = list(title = "pctile", range = c(0, 100))),
                nrows = 2, shareX = TRUE, titleY = TRUE, heights = c(0.55, 0.45)) %>%
            layout(xaxis = vix_datex(), margin = VIX_MARGIN,
                   hovermode = "x unified", font = list(size = 13),
                   legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.08),
                   title = plot_title(paste0("Cross-asset implied vol: level (top), ",
                                             vix_lb(), "-day percentile (bottom)"))) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    # --- every ratio as a percentile of its own history, one panel per kind
    RATIO_COL <- c("#0072B2", "#D55E00", "#009E73")
    output$vix_plot_ratios <- renderPlotly({
        d <- vix_win()
        shown <- VIX_RATIOS %>% dplyr::filter(plot)
        grps <- unique(shown$group)
        panes <- lapply(grps, function(g) {
            ks <- shown %>% dplyr::filter(group == g)
            p <- plot_ly()
            for (i in seq_len(nrow(ks))) {
                y <- d[[paste0(ks$key[i], "_pct")]]
                if (all(!is.finite(y))) next
                # the same three validated colour-blind-safe hues in every
                # pane, by position, instead of plotly's default red/green cycle
                p <- p %>% add_lines(x = d$date, y = y, name = ks$label[i],
                                     legendgroup = g,
                                     line = list(width = 1.3,
                                                 color = RATIO_COL[(i - 1) %% 3 + 1]),
                                     hovertemplate = "%{y:.0f}<extra>%{fullData.name}</extra>")
            }
            p %>% add_lines(x = d$date, y = rep(5, nrow(d)), showlegend = FALSE,
                            line = list(color = "#b3b3b3", width = 0.8, dash = "dot")) %>%
                add_lines(x = d$date, y = rep(95, nrow(d)), showlegend = FALSE,
                          line = list(color = "#b3b3b3", width = 0.8, dash = "dot")) %>%
                layout(yaxis = list(title = VIX_PANE[[g]], range = c(0, 100)))
        })
        subplot(panes, nrows = length(panes), shareX = TRUE, titleY = TRUE) %>%
            layout(xaxis = vix_datex(), margin = VIX_MARGIN,
                   hovermode = "x unified", font = list(size = 13),
                   title = plot_title(paste0("Ratios as percentiles of their own ",
                                             vix_lb(), "-day history (dotted = 5 / 95)"))) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    # --- the cross-product guard. An implied ratio at an extreme is NOT a
    # trade: "you don't just want the implied spread to be off, you really need
    # the VRPs to be mispriced as well" - the NASDAQ trades above the S&P
    # because it moves more. So the implied ratio is drawn against the REALISED
    # ratio of the same two underlyings (ORATS 20d close-to-close on the ETF
    # proxies). Divergence between the two lines is the signal; the implied
    # line on its own is not.
    output$vix_plot_cross <- renderPlotly({
        proxies <- unname(VIX_PROXY)
        rv <- ORATS_core %>% ungroup() %>%
            dplyr::filter(ticker %in% proxies) %>%
            dplyr::transmute(date = as.Date(tradeDate), ticker,
                             rv = na_if(clsHvXern20d, 0)) %>%
            tidyr::pivot_wider(names_from = ticker, values_from = rv)
        missing <- setdiff(proxies, names(rv))
        validate(need(length(missing) == 0,
                      paste("ORATS_core is missing", paste(missing, collapse = ", "))))
        rea <- rv %>% dplyr::transmute(date, VXN_VIX = QQQ / SPY,
                                       RVX_VIX = IWM / SPY, VXD_VIX = DIA / SPY)
        d <- vix_d() %>%
            dplyr::select(date, i_VXN = VXN_VIX, i_RVX = RVX_VIX, i_VXD = VXD_VIX) %>%
            dplyr::inner_join(rea, by = "date")
        validate(need(nrow(d) > 0, "no overlap between the CBOE and ORATS windows"))
        keys <- c("VXN_VIX", "RVX_VIX", "VXD_VIX")
        panes <- lapply(seq_along(keys), function(i) {
            k <- keys[i]; lab <- VIX_RATIOS$label[match(k, VIX_RATIOS$key)]
            plot_ly(d, x = ~date) %>%
                add_lines(y = d[[paste0("i_", sub("_VIX$", "", k))]],
                          name = "implied", legendgroup = "implied",
                          showlegend = (i == 1),
                          line = list(color = "#0369a1", width = 1.4),
                          hovertemplate = "implied %{y:.2f}<extra></extra>") %>%
                add_lines(y = d[[k]], name = "realised", legendgroup = "realised",
                          showlegend = (i == 1),
                          line = list(color = "#c2410c", width = 1.4),
                          hovertemplate = "realised %{y:.2f}<extra></extra>") %>%
                layout(yaxis = list(title = sub(" .*", "", lab)))
        })
        subplot(panes, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
            layout(xaxis = vix_datex(), margin = VIX_MARGIN,
                   hovermode = "x unified", font = list(size = 13),
                   title = plot_title(paste("Cross-product vol vs SPX: implied (CBOE)",
                                            "against realised (ORATS 20d) -",
                                            "an extreme implied ratio is only a trade",
                                            "if the realised one disagrees"))) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    # --- implied correlation and dispersion.
    # The term the cross-product panel is missing: index vol can sit cheap
    # against single-name vol either because index vol is cheap or because
    # correlation is low, and nothing else on the tab separates those. Drawn as
    # levels, not percentiles - a correlation of 9 means something on its own,
    # unlike a ratio.
    output$vix_plot_corr <- renderPlotly({
        d <- vix_win()
        validate(need(any(is.finite(d$COR1M)), "no COR1M in the window"))
        p1 <- plot_ly(d, x = ~date) %>%
            add_lines(y = ~COR1M, name = "COR1M",
                      line = list(color = "#0369a1", width = 1.4),
                      hovertemplate = "%{y:.2f}<extra>COR1M</extra>") %>%
            add_lines(y = ~COR3M, name = "COR3M",
                      line = list(color = "#7dd3fc", width = 1.4),
                      hovertemplate = "%{y:.2f}<extra>COR3M</extra>") %>%
            layout(yaxis = list(title = "implied corr"))
        p2 <- plot_ly(d, x = ~date) %>%
            add_lines(y = ~DSPX, name = "DSPX",
                      line = list(color = "#c2410c", width = 1.4),
                      hovertemplate = "%{y:.2f}<extra>DSPX</extra>") %>%
            layout(yaxis = list(title = "dispersion"))
        subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
            layout(xaxis = vix_datex(), margin = VIX_MARGIN,
                   hovermode = "x unified", font = list(size = 13),
                   title = plot_title(paste("Implied correlation and dispersion -",
                                            "why index vol is where it is"))) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    # --- fixed-strike volatility, two consecutive sessions.
    # ATM IV cannot distinguish "someone bought vol" from "spot slid along an
    # unchanged smile", because the ATM strike moves with spot. Holding the
    # strike fixed separates them. Market up AND vol up at fixed strikes means
    # hedges are being bought.
    output$vix_plot_fixed <- renderPlotly({
        fx <- fixed_strike_pair(as.numeric(input$v_fs_dte))
        validate(need(!is.null(fx) && nrow(fx$data) > 5,
                      "no SPY chain pair available for that tenor"))
        j <- fx$data %>% dplyr::arrange(strike) %>%
            dplyr::filter(strike >= fx$s_now * 0.80, strike <= fx$s_now * 1.15)
        chg <- (j$iv_now - j$iv_prev) * 100
        smile <- plot_ly(x = j$strike) %>%
            add_lines(y = j$iv_prev * 100, name = format(fx$d_prev),
                      line = list(color = "#94a3b8", width = 1.6),
                      hovertemplate = "%{y:.2f}<extra>prior</extra>") %>%
            add_lines(y = j$iv_now * 100, name = format(fx$d_now),
                      line = list(color = "#0369a1", width = 2),
                      hovertemplate = "%{y:.2f}<extra>latest</extra>") %>%
            layout(yaxis = list(title = "IV, vol pts"),
                   shapes = list(
                       list(type = "line", x0 = fx$s_prev, x1 = fx$s_prev, yref = "paper",
                            y0 = 0, y1 = 1, line = list(color = "#94a3b8", dash = "dot")),
                       list(type = "line", x0 = fx$s_now, x1 = fx$s_now, yref = "paper",
                            y0 = 0, y1 = 1, line = list(color = "#0369a1", dash = "dot"))))
        delta <- plot_ly(x = j$strike) %>%
            add_bars(y = chg, name = "change",
                     marker = list(color = ifelse(chg >= 0, "#c2410c", "#0369a1")),
                     showlegend = FALSE,
                     hovertemplate = "%{y:+.2f} vol pts<extra>%{x}</extra>") %>%
            layout(yaxis = list(title = "change, vol pts"))
        subplot(smile, delta, nrows = 2, shareX = TRUE, titleY = TRUE,
                heights = c(0.62, 0.38)) %>%
            layout(xaxis = list(title = "strike"), margin = VIX_MARGIN,
                   hovermode = "x unified", font = list(size = 13),
                   title = plot_title(sprintf(
                       "SPY fixed-strike vol, %s exp  -  %s to %s, spot %.2f to %.2f (%+.2f%%)%s",
                       format(fx$expir), format(fx$d_prev), format(fx$d_now),
                       fx$s_prev, fx$s_now, (fx$s_now / fx$s_prev - 1) * 100,
                       if (fx$d_now < Sys.Date() - 7)
                           "<br><sup>the SPY chain archive stops here - run the ORATS downloader to bring it current</sup>"
                       else ""))) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    # --- the SPX/VIX relationship, measured continuously.
    # The flow-spike screen below thresholds single days; this is the same
    # question asked as a running correlation, so a regime where vol stops
    # tracking spot shows up before any one day crosses a threshold.
    output$vix_plot_spxvix <- renderPlotly({
        d <- vix_win() %>% dplyr::filter(is.finite(spx_vix_cor))
        validate(need(nrow(d) > 10, "not enough overlap to correlate"))
        plot_ly(d, x = ~date) %>%
            add_lines(y = ~spx_vix_cor, name = "60d corr",
                      line = list(color = "#0369a1", width = 1.5),
                      hovertemplate = "%{y:.3f}<extra></extra>") %>%
            add_lines(y = ~rep(median(spx_vix_cor, na.rm = TRUE), nrow(d)),
                      name = "window median",
                      line = list(color = "#94a3b8", width = 1, dash = "dot")) %>%
            layout(xaxis = vix_datex(),
                   yaxis = list(title = "correlation", range = c(-1, 0.2)),
                   margin = VIX_MARGIN, hovermode = "x unified",
                   font = list(size = 14),
                   title = plot_title(paste("SPX return vs VIX change, 60-day rolling",
                                            "correlation"))) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

    # --- the flow-spike screen
    vix_spike <- reactive({
        vix_spike_screen(vix_d(), z_min = input$v_spike_z,
                         move_max = input$v_spike_mv / 100,
                         spot_df = ORATS_core %>% ungroup() %>%
                             dplyr::filter(ticker == "SPY"))
    })

    output$vix_plot_spike <- renderPlotly({
        d <- vix_spike() %>% dplyr::filter(!is.na(flag), is.finite(fwd5))
        cols <- c("VIX up, spot flat" = "#c2410c",
                  "VIX up, spot moved" = "#0369a1", "other" = "#cbd5e1")
        p <- plot_ly()
        for (f in names(cols)) {
            x <- d %>% dplyr::filter(flag == f)
            if (nrow(x) == 0) next
            p <- p %>% add_trace(x = abs(x$spx_ret) * 100, y = x$dVIX_z,
                                 type = "scatter", mode = "markers", name = f,
                                 text = format(x$date),
                                 marker = list(color = cols[[f]],
                                               size = if (f == "other") 4 else 8,
                                               opacity = if (f == "other") 0.35 else 0.9),
                                 hovertemplate = paste0("%{text}<br>|SPX| %{x:.2f}%",
                                                        "<br>dVIX z %{y:.2f}<extra></extra>"))
        }
        # the scatter classifies; without the table underneath it, the verdict
        # has to travel in the title or the panel stops answering anything
        fwd <- d %>% dplyr::group_by(flag) %>%
            dplyr::summarise(n = dplyr::n(), f5 = mean(fwd5, na.rm = TRUE),
                             .groups = "drop")
        say <- function(f) { r <- fwd[fwd$flag == f, ]
            if (nrow(r) == 0) paste0(f, ": none") else
                sprintf("%s n=%d, VIX %+0.2f over 5d", f, r$n, r$f5) }
        p %>% layout(xaxis = list(title = "|SPX move| that day, %"),
                     yaxis = list(title = "VIX 1-day change, z"),
                     font = list(size = 14),
                     margin = list(t = 86),
                     title = plot_title(paste0(
                         "Flow-spike screen: a VIX jump the underlying did not justify",
                         "<br><sup>", say("VIX up, spot flat"), "  |  ",
                         say("VIX up, spot moved"), "  (the control)</sup>"))) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    })

}

# ---- Run app ----


shinyApp(ui = ui, server = server, options = list(height = 1080))

# ---- Deployment notes ----
# To run locally: put this file as app.R and run `shiny::runApp('.')` in the directory.
# To deploy to shinyapps.io:
# 1) install.packages('rsconnect') and set up account (https://docs.rstudio.com/shinyapps.io/)
# 2) rsconnect::deployApp('.')
#
# To host on a Shiny Server, copy the app folder (containing app.R and the 'data' folder) to the server's app directory.
# Make sure file permissions allow the shiny user to read the gzip files.
#
# If your CSVs are stored remotely (S3, HTTP), modify the 'f' path to either a downloaded temporary file
# (use download.file()) or use `arrow::read_csv_arrow()` / `vroom::vroom()` as appropriate.
