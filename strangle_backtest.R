# Short-strangle backtester on ORATS strikes data
#
# Daily snapshots live at:
#   /home/marco/trading/HistoricalData/ORATS/API/strikes/<TICKER>/orats_<TICKER>_YYYYMMDD.csv.gz
# ORATS `delta` column is the CALL delta; put delta = call delta - 1.
#
# Returns a long data.table indexed by (date, position_id) — one row per
# trading day a position is alive (entry row included), so it can be filtered
# / joined with arbitrary predictors before computing summary stats.

suppressPackageStartupMessages(library(data.table))

backtest_short_strangle <- function(ticker,
                                    delta_target,
                                    dte_target,
                                    hold_days,
                                    hold_unit     = c("calendar", "trading"),
                                    dte_tolerance = 3L,
                                    start_date    = NULL,
                                    end_date      = NULL,
                                    data_root     = "/home/marco/trading/HistoricalData/ORATS/API/strikes",
                                    contract_multiplier = 100L,
                                    min_dte_at_entry = 1L,
                                    monthly_only  = FALSE) {

  hold_unit <- match.arg(hold_unit)
  stopifnot(delta_target > 0, delta_target < 1, dte_target >= 1, hold_days >= 1,
            dte_tolerance >= 0)

  ticker_dir <- file.path(data_root, ticker)
  if (!dir.exists(ticker_dir)) stop(sprintf("No ORATS folder for %s at %s", ticker, ticker_dir))

  files <- list.files(ticker_dir,
                      pattern = sprintf("^orats_%s_\\d{8}\\.csv\\.gz$", ticker),
                      full.names = TRUE)
  if (!length(files)) stop("No ORATS files for ", ticker)
  dates <- as.Date(sub(sprintf("orats_%s_(\\d{8})\\.csv\\.gz", ticker), "\\1",
                       basename(files)), format = "%Y%m%d")
  ord   <- order(dates); files <- files[ord]; dates <- dates[ord]
  if (!is.null(start_date)) { k <- dates >= as.Date(start_date); files <- files[k]; dates <- dates[k] }
  if (!is.null(end_date))   { k <- dates <= as.Date(end_date);   files <- files[k]; dates <- dates[k] }
  if (!length(files)) stop("No files in date range")

  # US equity monthlies expire on the 3rd Friday of the month. ORATS, however,
  # dates expirDate on the *Saturday after* the 3rd Friday for the pre-~Feb-2015
  # settlement convention, and on the Friday itself afterwards. When that Friday
  # is a market holiday (e.g., Good Friday) expiry rolls back to the Thursday
  # before. We use the ORATS file calendar as ground truth for "trading day".
  trading_days_set <- dates
  is_monthly_expiry <- function(d) {
    first     <- as.Date(format(d, "%Y-%m-01"))
    wd        <- as.POSIXlt(first)$wday          # 0 = Sun, 5 = Fri
    third_fri <- first + ((5L - wd) %% 7L) + 14L
    d == third_fri |                              # Friday-dated (post-2015)
      d == (third_fri + 1L) |                     # Saturday-dated (pre-2015)
      (d == (third_fri - 1L) & !(third_fri %in% trading_days_set))  # holiday Friday
  }

  # ORATS `dte` is inclusive (expir - trade + 1). We override with calendar
  # days-to-expiry (exclusive), so dte_target = 9 means expiry 9 calendar days
  # after entry, matching the natural interpretation and dte_remaining downstream.
  read_day <- function(path, today) {
    dt <- tryCatch(fread(cmd = paste("zcat", shQuote(path)), showProgress = FALSE),
                   error = function(e) NULL)
    if (is.null(dt) || !nrow(dt)) return(NULL)
    dt[, `:=`(callMid   = (callBidPrice + callAskPrice) / 2,
              putMid    = (putBidPrice  + putAskPrice ) / 2,
              expirDate = as.Date(expirDate))]
    dt[, calDte := as.integer(expirDate - today)]
    dt
  }

  pick_legs <- function(snap) {
    exps <- snap[calDte >= min_dte_at_entry,
                 .(calDte = calDte[1L]), keyby = expirDate]
    if (!nrow(exps)) return(NULL)
    exps <- exps[abs(calDte - dte_target) <= dte_tolerance]
    if (monthly_only) exps <- exps[is_monthly_expiry(expirDate)]
    if (!nrow(exps)) return(NULL)   # nothing within tolerance — skip this day
    chosen <- exps[order(abs(calDte - dte_target), calDte)][1L, expirDate]

    chain <- snap[expirDate == chosen & !is.na(delta)]
    if (nrow(chain) < 2L) return(NULL)

    call_chain <- chain[callBidPrice > 0]
    put_chain  <- chain[putBidPrice  > 0]
    if (!nrow(call_chain) || !nrow(put_chain)) return(NULL)

    call_row <- call_chain[which.min(abs(delta - delta_target))]
    put_row  <- put_chain[which.min(abs(delta - (1 - delta_target)))]
    if (put_row$strike > call_row$strike) return(NULL)  # inverted

    list(expir_date     = chosen,
         call_strike    = call_row$strike,
         put_strike     = put_row$strike,
         call_delta     = call_row$delta,
         put_delta      = put_row$delta - 1,
         entry_call_mid = call_row$callMid,
         entry_put_mid  = put_row$putMid,
         stock_price    = call_row$stockPrice,
         dte_at_entry   = as.integer(chosen - as.Date(call_row$tradeDate)))
  }

  out_list   <- vector("list", length(files))
  active     <- NULL
  position_n <- 0L
  out_n      <- 0L

  for (i in seq_along(files)) {
    today <- dates[i]
    snap  <- read_day(files[[i]], today)
    if (is.null(snap)) {
      # File exists in our date list but is unreadable/empty — still a trading
      # day, so an active position's hold counter must advance.
      if (!is.null(active)) active$trd_days_held <- active$trd_days_held + 1L
      next
    }

    if (!is.null(active)) {
      leg_c <- snap[expirDate == active$expir_date & strike == active$call_strike]
      leg_p <- snap[expirDate == active$expir_date & strike == active$put_strike]
      spot  <- if (nrow(leg_c)) leg_c$stockPrice[1L]
               else if (nrow(leg_p)) leg_p$stockPrice[1L]
               else if (nrow(snap)) snap$stockPrice[1L]
               else NA_real_
      dte_rem <- as.integer(active$expir_date - today)

      if (dte_rem < 0) {
        call_mark <- pmax(spot - active$call_strike, 0)
        put_mark  <- pmax(active$put_strike - spot, 0)
      } else if (!nrow(leg_c) || !nrow(leg_p)) {
        call_mark <- NA_real_; put_mark <- NA_real_
      } else {
        # Mark to mid when both sides quoted; when bid=0 the mid (= ask/2)
        # understates the cost to close a short, so fall back to the ask
        # (what we'd actually pay to lift the offer).
        cbid <- leg_c$callBidPrice[1L]; cask <- leg_c$callAskPrice[1L]
        pbid <- leg_p$putBidPrice[1L];  pask <- leg_p$putAskPrice[1L]
        call_mark <- if (cbid > 0) (cbid + cask) / 2 else cask
        put_mark  <- if (pbid > 0) (pbid + pask) / 2 else pask
      }

      mark      <- call_mark + put_mark
      pnl_cum   <- (active$entry_credit - mark) * contract_multiplier
      pnl_daily <- if (is.na(active$last_mark) || is.na(mark)) NA_real_
                   else (active$last_mark - mark) * contract_multiplier

      cal_days_held <- as.integer(today - active$entry_date)
      trd_days_held <- active$trd_days_held + 1L  # this row is the next trading day
      days_held_for_exit <- if (hold_unit == "calendar") cal_days_held else trd_days_held
      is_exit <- dte_rem <= 0 || days_held_for_exit >= hold_days

      out_n <- out_n + 1L
      out_list[[out_n]] <- data.table(
        tradeDate         = today,
        position_id       = active$position_id,
        entry_date        = active$entry_date,
        expir_date        = active$expir_date,
        call_strike       = active$call_strike,
        put_strike        = active$put_strike,
        call_delta_entry  = active$call_delta,
        put_delta_entry   = active$put_delta,
        entry_credit      = active$entry_credit * contract_multiplier,
        entry_spot        = active$entry_spot,
        entry_notional    = active$entry_notional,
        stock_price       = spot,
        dte_remaining     = dte_rem,
        cal_days_held     = cal_days_held,
        trd_days_held     = trd_days_held,
        mark              = mark * contract_multiplier,
        pnl_cum           = pnl_cum,
        pnl_daily         = pnl_daily,
        pnl_pct_credit    = pnl_cum / (active$entry_credit * contract_multiplier),
        pnl_pct_notional  = pnl_cum / active$entry_notional,
        is_entry          = FALSE,
        is_exit           = is_exit,
        settled_at_expiry = dte_rem <= 0
      )

      if (!is.na(mark)) active$last_mark <- mark
      active$trd_days_held <- trd_days_held
      if (is_exit) active <- NULL
      next   # non-overlapping: don't open a new one on the same day
    }

    legs <- pick_legs(snap)
    if (is.null(legs)) next
    entry_credit   <- legs$entry_call_mid + legs$entry_put_mid
    position_n     <- position_n + 1L
    entry_notional <- legs$stock_price * contract_multiplier

    active <- list(position_id    = position_n,
                   entry_date     = today,
                   expir_date     = legs$expir_date,
                   call_strike    = legs$call_strike,
                   put_strike     = legs$put_strike,
                   call_delta     = legs$call_delta,
                   put_delta      = legs$put_delta,
                   entry_credit   = entry_credit,
                   entry_spot     = legs$stock_price,
                   entry_notional = entry_notional,
                   last_mark      = entry_credit,
                   trd_days_held  = 0L)

    out_n <- out_n + 1L
    out_list[[out_n]] <- data.table(
      tradeDate         = today,
      position_id       = position_n,
      entry_date        = today,
      expir_date        = legs$expir_date,
      call_strike       = legs$call_strike,
      put_strike        = legs$put_strike,
      call_delta_entry  = legs$call_delta,
      put_delta_entry   = legs$put_delta,
      entry_credit      = entry_credit * contract_multiplier,
      entry_spot        = legs$stock_price,
      entry_notional    = entry_notional,
      stock_price       = legs$stock_price,
      dte_remaining     = legs$dte_at_entry,
      cal_days_held     = 0L,
      trd_days_held     = 0L,
      mark              = entry_credit * contract_multiplier,
      pnl_cum           = 0,
      pnl_daily         = 0,
      pnl_pct_credit    = 0,
      pnl_pct_notional  = 0,
      is_entry          = TRUE,
      is_exit           = FALSE,
      settled_at_expiry = FALSE
    )
  }

  out <- rbindlist(out_list, fill = TRUE)
  setattr(out, "params",
          list(ticker = ticker, delta_target = delta_target,
               dte_target = dte_target, hold_days = hold_days,
               hold_unit = hold_unit, dte_tolerance = dte_tolerance,
               monthly_only = monthly_only,
               start_date = min(out$tradeDate, na.rm = TRUE),
               end_date   = max(out$tradeDate, na.rm = TRUE),
               contract_multiplier = contract_multiplier))
  out[]
}


# Collapse the daily panel produced by backtest_short_strangle() into a
# one-row-per-trade summary. Use this for entry-filter analyses: the predictor
# is evaluated at entry_date, the outcome is the realized close PnL, and no
# daily-drift ambiguity is introduced.
summarize_trades <- function(daily) {
  stopifnot(is.data.table(daily) || is.data.frame(daily))
  d <- as.data.table(daily)

  entry_cols <- c("entry_date", "expir_date", "call_strike", "put_strike",
                  "call_delta_entry", "put_delta_entry",
                  "entry_credit", "entry_spot", "entry_notional")

  entries <- d[is_entry == TRUE, c("position_id", entry_cols), with = FALSE]
  exits   <- d[is_exit  == TRUE,
               .(position_id,
                 exit_date              = tradeDate,
                 final_pnl              = pnl_cum,
                 final_pnl_pct_credit   = pnl_pct_credit,
                 final_pnl_pct_notional = pnl_pct_notional,
                 exit_spot              = stock_price,
                 cal_days_held, trd_days_held,
                 settled_at_expiry)]

  closed_ids <- exits$position_id
  excursion  <- d[position_id %in% closed_ids,
                  .(mae            = min(pnl_cum,        na.rm = TRUE),
                    mfe            = max(pnl_cum,        na.rm = TRUE),
                    mae_pct_credit = min(pnl_pct_credit, na.rm = TRUE),
                    mfe_pct_credit = max(pnl_pct_credit, na.rm = TRUE)),
                  by = position_id]

  out <- merge(exits, entries,   by = "position_id")
  out <- merge(out,   excursion, by = "position_id")
  setcolorder(out, c("position_id", "entry_date", "exit_date", "expir_date",
                     "call_strike", "put_strike",
                     "call_delta_entry", "put_delta_entry",
                     "entry_spot", "exit_spot",
                     "entry_credit", "entry_notional",
                     "cal_days_held", "trd_days_held", "settled_at_expiry",
                     "final_pnl", "final_pnl_pct_credit", "final_pnl_pct_notional",
                     "mae", "mfe", "mae_pct_credit", "mfe_pct_credit"))
  out[]
}
