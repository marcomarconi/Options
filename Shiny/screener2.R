# =============================================================================
# screener2.R — verified-signal daily ETF screener (companion to ui.R)
#
# Implements ONLY what survived the verification studies:
#   * f_ivrv = log(iv30d/clsHv20d), gated CROSS-SECTIONALLY (top decile) — the one
#     robust VRP signal (rank-IC +0.11). ivPctile1y is context only (IC ~ 0).
#   * Forward factor (FF) — one-sided: contango side dead; thresholds 0.2 near-mid
#     viable / 0.35 edge tail / 0.5 taker-OK. Min-debit, volume and earnings gates
#     live in the Python screener, which this app shells out to.
#   * cr_skew = z(dlt25Iv30d/iv30d) — call-wing richness FILTER (not a strategy).
#   * Clean universe by default: leveraged/inverse/vol ETPs excluded (they inflated
#     ~1/3 of the raw VRP edge in the outlier audit).
#
# Candidates tab = single source of truth: runs vrp_screener.py and
# ff_calendar_v2/screener.py and renders their CSV output (all gates included).
# Screen/Ticker tabs = visual context around those candidates.
#
# Run:  shiny::runApp('/home/marco/trading/Systems/Options/Shiny/screener2.R')
# First run: set initialize <- TRUE once to build ORATS_core_v2.pq (~minutes).
# =============================================================================

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
source("/home/marco/trading/Systems/Options/OptionsCommon.R")
source("/home/marco/trading/Systems/Common/Common.R")
}

# ---------------------------------------------------------------- config ----
{
initialize <- FALSE
orats_dir   <- "/home/marco/trading/HistoricalData/ORATS/"
core_dir    <- "/home/marco/trading/HistoricalData/ORATS/core/"
delayed_dir <- "/home/marco/trading/HistoricalData/ORATS/delayed/"
options_dir <- "/home/marco/trading/Systems/Options/Data/"
strat_dir   <- "/home/marco/trading/Systems/Strategies/"

ORATS_core_v2_file      <- paste0(options_dir, "ORATS_core_v2.pq")
ORATS_core_delayed_file <- paste0(delayed_dir, "orats_core_delayed.csv")
days_to_load <- 500
zwin         <- 252               # rolling z-score window (vs own history)
min_hist     <- zwin + 8          # tickers need enough history for the z-scores

# vrp_etf_model retired 2026-07-29 (superseded by vrp_etf_v2), sharpe_two retired
# 2026-07-30 — paths repointed into _retired/, both still run in place. The VRP
# screener should move to vrp_etf_v2/screener.py: that one prices the real
# delta-hedged straddle, this one scored a variance-swap proxy.
VRP_SCREENER <- paste0(strat_dir, "_retired/vrp_etf_model/src/vrp_screener.py")
VRP_RESULTS  <- paste0(strat_dir, "_retired/vrp_etf_model/results/")
FF_SCREENER  <- paste0(strat_dir, "ff_calendar_v2/screener.py")
FF_RESULTS   <- paste0(strat_dir, "ff_calendar_v2/results/")
S2_SRC       <- paste0(strat_dir, "_retired/sharpe_two/src")

etf_screener <- read_csv("/home/marco/trading/Systems/Options/etf-screener-weekly-options.csv", show_col_types = F)

# v1 columns + what the verified signals need (all numeric — keep ticker/tradeDate first)
cols_to_extract <- c('ticker', 'tradeDate', 'pxAtmIv', 'pxCls', 'hiStrikeM1', 'hiStrikeM2',
                     "stkPxChng1wk", "stkPxChng1m", "stkPxChng6m",
                     "straPxM1", "straPxM2", "atmIvM1", "atmIvM2", "atmIvM3", "atmIvM4",
                     "avgOptVolu20d", "cVolu", "cOi", "pVolu", "pOi",
                     "dtExM1", "dtExM2",
                     "exErnIv10d", "exErnIv30d", "exErnIv60d", "exErnIv90d", "exErnIv6m", "exErnIv1yr",
                     "volOfVol", "volOfIvol",
                     "orHvXern10d", "orHvXern20d", "orHvXern60d", "orHvXern90d", "orHvXern120d", "orHvXern252d",
                     "clsHvXern10d", "clsHvXern20d", "clsHvXern60d", "clsHvXern90d", "clsHvXern120d", "clsHvXern252d",
                     "ivPctile1y", "ivHvXernRatio", "ivSpyRatio", "correlSpy1m",
                     "fexErn30_20", "fexErn60_30", "fexErn90_60", "fexErn180_90", "fexErn90_30",
                     "slope", "contango", "deriv",
                     # v2 additions (verified-signal inputs)
                     "iv30d", "clsHv20d",           # f_ivrv exactly as backtested
                     "dlt25Iv30d",                  # cr_skew numerator
                     "daysToNextErn",               # earnings contamination flag
                     "confidence"                   # data-quality gate
)
}

# ---------------------------------------------------------- data loading ----
{
quiet_fread <- purrr::quietly(.f = fread)
load_orats_day <- function(filename, cols) {
    print(filename)
    quiet_fread(glue::glue(core_dir, "{filename}")) %>%
        purrr::pluck("result") %>%
        select(any_of(cols)) %>%
        mutate(across(3:ncol(.), ~ as.single(as.numeric(.)))) %>%
        mutate(across(where(~ inherits(., "IDate")), ~ as.Date(.)))
}

create_core_v2 <- function(core_dir) {
    print("Building ORATS core v2 from raw files")
    files <- list.files(core_dir, "orats_core_202[0-9].*gz")
    files_sorted <- files[order(as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1", basename(files)), format = "%Y%m%d"))]
    files_sorted %>% tail(days_to_load) %>%
        purrr::map_df(.f = load_orats_day, cols_to_extract) %>%
        arrange(ticker, tradeDate)
}

update_core_v2 <- function(core, core_dir) {
    core_last_day <- core %>% arrange(tradeDate) %>% tail(1) %>% pull(tradeDate) %>% as.Date
    files <- list.files(core_dir, "orats_core_202[0-9].*gz")
    files_sorted <- files[order(as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1", basename(files)), format = "%Y%m%d"))]
    last_day <- as.Date(tail(files_sorted, 1) %>% sub("orats_core_(.*)\\.csv\\.gz", "\\1", .), format = "%Y%m%d")
    if (core_last_day < last_day) {
        print(paste("Core v2 is at", core_last_day, "— appending newer days"))
        index <- grep(gsub("\\-", "", core_last_day), files_sorted)
        if (length(index) == 0 || index >= length(files_sorted)) stop("could not locate last loaded day among core files")
        tmp <- files_sorted[(index + 1):length(files_sorted)] %>% purrr::map_df(.f = load_orats_day, cols_to_extract)
        core <- bind_rows(core, tmp) %>% arrange(ticker, tradeDate)
    }
    core
}

append_delayed_v2 <- function(core, delayed_file) {
    if (!file.exists(delayed_file)) { print("Delayed file not existing"); return(core) }
    delayed <- read_csv(delayed_file, show_col_types = FALSE)
    missing <- setdiff(cols_to_extract, names(delayed))
    for (m in missing) delayed[[m]] <- NA_real_          # delayed feed may lack v2 columns
    delayed <- delayed %>% select(all_of(cols_to_extract))
    core_last <- core %>% arrange(tradeDate) %>% tail(1) %>% pull(tradeDate) %>% as.Date
    del_last  <- delayed %>% arrange(tradeDate) %>% tail(1) %>% pull(tradeDate) %>% as.Date
    if (del_last > core_last) core <- bind_rows(core, delayed) %>% arrange(ticker, tradeDate)
    core
}

# ETP/corrupt-name exclusion list — read live from s2_universe.py (single source
# of truth); frozen fallback snapshot (2026-07-04) if python is unavailable.
load_exclude <- function() {
    code <- paste0("import sys; sys.path.insert(0,'", S2_SRC, "'); ",
                   "from s2_universe import EXCLUDE; print('\\n'.join(sorted(EXCLUDE)))")
    out <- tryCatch(system2("python3", c("-c", shQuote(code)), stdout = TRUE, stderr = FALSE),
                    error = function(e) character(0))
    if (length(out) > 5) return(out)
    warning("s2_universe.py not readable — using frozen EXCLUDE snapshot")
    c('JO','OIL','IJH','JNUG','META',
      'TQQQ','SQQQ','UPRO','SPXU','SPXS','SPXL','SDS','SSO','SH','UDOW','SDOW','DDM','DXD',
      'TNA','TZA','URTY','SRTY','QID','QLD','FAS','FAZ','LABU','LABD','SOXL','SOXS','TECL','TECS',
      'YINN','YANG','BRZU','EDC','EDZ','DPST','CURE','RXL','BIB','BIS','UWM','TWM','MIDU',
      'AGQ','ZSL','BOIL','KOLD','UNG','UCO','SCO','UGL','GLL','NUGT','DUST','GUSH','DRIP','ERX','ERY',
      'BNO','UNL','UGA','DGAZ','UGAZ','OILU','OILD',
      'UVXY','SVXY','VIXY','VXX','VXZ','UVIX','SVIX',
      'BITX','BITU','BITI','CONY','AMDL','AMDY','NVDL','TSLL','MSTU','MSTX','AMDU',
      'RSX','ETHU','XELA','MNTV')
}

# TTR's runSum errors (rather than returning NA) on series with fewer than n
# observations after the leading NAs, and on interior NAs — guard both.
runZsafe <- function(x, n) {
    x <- zoo::na.locf(x, na.rm = FALSE)
    if (length(x) - sum(cumprod(is.na(x))) < n) return(rep(NA_real_, length(x)))
    runZscore(x, n)
}

# derived signals — computed in-memory each session (parquet stores RAW cols only)
init_core_v2 <- function(core, screener, exclude) {
    core %>% select(-any_of("class")) %>%
        right_join(screener %>% dplyr::select(Symbol, `Asset Class`),
                   by = c("ticker" = "Symbol"), relationship = "many-to-many") %>%
        rename(class = `Asset Class`) %>%
        group_by(ticker) %>% arrange(tradeDate) %>% dplyr::filter(n() >= min_hist) %>%
        mutate(
            class      = if_else(is.na(class), "Stock", class),
            is_etp     = ticker %in% exclude,
            # --- the one robust VRP signal, exactly as backtested ---
            f_ivrv     = log(iv30d / clsHv20d),
            ivrv_ts_z  = runZsafe(f_ivrv %>% na.locf(na.rm = F), zwin),
            # realized VRP (lagged IV vs subsequent RV) — context panel
            VRP        = log(lag(exErnIv30d, 20) / clsHvXern20d),
            VRPzscore  = runZsafe(VRP, zwin),
            # --- forward factors (ORATS ex-earn forward IVs) ---
            ff_60_30   = exErnIv30d / fexErn60_30  - 1,
            ff_90_30   = exErnIv30d / fexErn90_30  - 1,
            ff_90_60   = exErnIv60d / fexErn90_60  - 1,
            ff_180_90  = exErnIv90d / fexErn180_90 - 1,
            # --- call-wing richness vs own skew history (filter, not strategy) ---
            cskew      = dlt25Iv30d / iv30d,
            cr_skew    = runZsafe(cskew %>% na.locf(na.rm = F), zwin),
            # context only (verified weak/negative or null)
            slopeZscore = runZsafe(slope, zwin)
        ) %>% ungroup() %>%
        # cross-sectional IV/RV percentile: THE gated quantity (top decile = sell zone),
        # ranked within the clean liquid universe each day, as in the backtest
        group_by(tradeDate) %>%
        mutate(ivrv_xsec_pct = {
            ok <- !is_etp & avgOptVolu20d >= 2500 & is.finite(f_ivrv)
            r <- rep(NA_real_, n()); r[ok] <- percent_rank(f_ivrv[ok]) * 100; r
        }) %>% ungroup()
}

latest_core_date <- function() {
    f <- tail(sort(list.files(core_dir, "orats_core_202[0-9].*gz")), 1)
    as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1", f), format = "%Y%m%d")
}
}

# --------------------------------------------------------------- startup ----
if (initialize || !file.exists(ORATS_core_v2_file)) {
    ORATS_core_v2_raw <- create_core_v2(core_dir)
    write_parquet(ORATS_core_v2_raw, ORATS_core_v2_file)
    print("v2 initialization finished.")
}
if (!exists("ORATS_core_v2_raw")) {
    print(paste("Load ORATS core v2 file", ORATS_core_v2_file))
    ORATS_core_v2_raw <- read_parquet(ORATS_core_v2_file) %>% arrange(tradeDate)
    updated <- update_core_v2(ORATS_core_v2_raw, core_dir)
    if (nrow(updated) > nrow(ORATS_core_v2_raw)) {
        ORATS_core_v2_raw <- updated
        write_parquet(ORATS_core_v2_raw, ORATS_core_v2_file)
    }
    ORATS_core_v2_raw <- append_delayed_v2(ORATS_core_v2_raw, ORATS_core_delayed_file)
}
EXCLUDE_ETPS  <- load_exclude()
ORATS_core_v2 <- init_core_v2(ORATS_core_v2_raw, etf_screener, EXCLUDE_ETPS)
today_date    <- latest_core_date()

# -------------------------------------------------------------------- UI ----
ui <- fillPage(
    tags$head(tags$style(HTML(
        "html, body, .container-fluid {height:100%;}
         .sidebar {height:100vh; overflow:auto;}
         .main {height:100vh; overflow:auto;}
         .regime {font-family:monospace; font-size:14px; background:#f6f6f6;
                  border:1px solid #ddd; padding:6px 10px; margin-bottom:8px;}
         pre.console {font-size:12px; max-height:420px; overflow:auto;}"
    ))),
    tabsetPanel(
        # ---- primary daily workflow: run the verified screeners -------------
        tabPanel("Candidates",
            div(class = "container-fluid", fluidRow(
                column(width = 2, class = "sidebar", wellPanel(
                    dateInput("c_date", "Core date", value = today_date, format = "yyyy-mm-dd"),
                    h3("VRP screener"),
                    helpText("Rank = f_ivrv = log(iv30/rv20). Default gate: top decile, clean ETFs."),
                    numericInput("vrp_top", "Top N (0 = top-decile gate)", value = 0, min = 0),
                    numericInput("vrp_zmin", "z-min gate (blank/0 = off)", value = NA),
                    checkboxInput("vrp_etps", "Include lev/inv/vol ETPs (not advised)", value = FALSE),
                    actionButton("vrp_run", "Run VRP screener", class = "btn-primary"),
                    hr(),
                    h3("FF calendar screener"),
                    helpText("One-sided: FF≥0.2 near-mid viable, ≥0.35 edge tail, ≥0.5 taker-OK. Trade CLEARS rows only."),
                    selectInput("ff_pair", "Tenor pair", choices = c("30_60", "30_90", "14_30"), selected = "30_60"),
                    numericInput("ff_min", "FF floor", value = 0.25, step = 0.05),
                    numericInput("ff_min_debit", "Min debit $", value = 0.30, step = 0.05),
                    checkboxInput("ff_no_ern", "Earnings-clean only", value = FALSE),
                    radioButtons("ff_universe", "Universe",
                                 choices = c("default", "etf-only", "names-only"), selected = "default"),
                    actionButton("ff_run", "Run FF screener", class = "btn-primary")
                )),
                column(width = 10, class = "main",
                    div(style = "padding:8px;",
                        h1("VRP — short delta-hedged ATM straddles", style = "color: darkgray;"),
                        verbatimTextOutput("vrp_console"),
                        DTOutput("vrp_table"),
                        h1("FF calendars — sell front / buy back ATM call", style = "color: darkgray;"),
                        verbatimTextOutput("ff_console"),
                        DTOutput("ff_table")
                    )
                )
            ))
        ),
        # ---- cross-sectional visual screen ----------------------------------
        tabPanel("Screen",
            div(class = "container-fluid", fluidRow(
                column(width = 2, class = "sidebar", wellPanel(
                    dateInput("s_date", "Date", value = today_date, format = "yyyy-mm-dd"),
                    textInput("s_n_tickers", "Tickers to show", value = 50),
                    textInput("s_min_vol", "Min option volume (20d)", value = 2500),
                    checkboxInput("s_clean", "Clean universe (exclude lev/inv/vol ETPs)", value = TRUE),
                    checkboxInput("s_ff_viable_only", "FF plot: viable only (FF ≥ 0.2)", value = FALSE),
                    selectInput("s_ff_pair", "FF pair", choices = c("60_30", "90_30", "90_60", "180_90"), selected = "60_30")
                )),
                column(width = 10, class = "main",
                    div(style = "height:100vh; display:flex; flex-direction:column;",
                        div(style = "flex:1 1 auto; overflow:auto; padding:8px;",
                            div(class = "regime", textOutput("regime_banner")),
                            h1("VRP screen — sell the top decile (shaded)", style = "color: darkgray;"),
                            helpText("x = cross-sectional pctile of log(iv30/rv20) within the clean liquid universe (the gated quantity, rank-IC +0.11). y = same signal vs the name's own 1y history. ivPctile1y is deliberately NOT an axis (IC ~ 0)."),
                            plotOutput("vrp_screen_plot", height = "600px"),
                            h1("Forward factor screen — one-sided", style = "color: darkgray;"),
                            helpText("Lines: 0.2 = near-mid viable, 0.35 = edge tail, 0.5 = taker-OK. No line below zero — the contango side is dead. Hollow points = earnings before back expiry (not a clean FF play)."),
                            plotOutput("ff_screen_plot", height = "600px"),
                            h1("Call-wing richness (cr_skew) — filter, not a strategy", style = "color: darkgray;"),
                            helpText("cr_skew > 1 (right of line) flipped call selling positive (+0.08–0.16%/trade, t≈2). Use to pick the call side / strangle on names already gated by IV/RV."),
                            plotOutput("crskew_screen_plot", height = "600px"),
                            h1("Table", style = "color: darkgray;"),
                            DTOutput("screen_table")
                        )
                    )
                )
            ))
        ),
        # ---- single-name history of the verified signals ---------------------
        tabPanel("Ticker",
            fluidRow(column(12,
                column(width = 2, class = "sidebar", wellPanel(
                    h3("Ticker"),
                    textInput("t_ticker", "Ticker", value = "SPY")
                )),
                column(width = 10, class = "main",
                    div(style = "height:100vh; display:flex; flex-direction:column;",
                        div(style = "flex:1 1 auto; overflow:auto; padding:8px;",
                            plotlyOutput("t_price_vol", height = "500px"),
                            plotlyOutput("t_ivrv",      height = "500px"),
                            plotlyOutput("t_ff",        height = "500px"),
                            plotlyOutput("t_crskew",    height = "500px")
                        )
                    )
                )
            ))
        )
    )
)

# ---------------------------------------------------------------- server ----
server <- function(input, output, session) {

    # ---- helpers ---------------------------------------------------------
    # run a python screener, capture console, read back the CSV it announces
    run_screener <- function(script, args) {
        console <- suppressWarnings(system2("python3", c(script, args), stdout = TRUE, stderr = TRUE))
        txt <- paste(console, collapse = "\n")
        m <- regmatches(txt, gregexpr("/[^ \n]+screen_[^ \n]+\\.csv", txt))[[1]]
        tbl <- NULL
        if (length(m) > 0 && file.exists(tail(m, 1)))
            tbl <- read_csv(tail(m, 1), show_col_types = FALSE)
        list(console = txt, tbl = tbl)
    }

    day_slice <- reactive({
        d <- ORATS_core_v2 %>% dplyr::filter(tradeDate == input$s_date)
        if (input$s_clean) d <- d %>% dplyr::filter(!is_etp)
        d <- d %>% dplyr::filter(avgOptVolu20d >= as.numeric(input$s_min_vol))
        d %>% arrange(desc(avgOptVolu20d)) %>% head(n = as.numeric(input$s_n_tickers))
    })

    # ---- Candidates tab ---------------------------------------------------
    vrp_res <- eventReactive(input$vrp_run, {
        args <- c("--date", format(as.Date(input$c_date), "%Y%m%d"))
        if (!is.na(input$vrp_top)  && input$vrp_top  > 0) args <- c(args, "--top",  input$vrp_top)
        if (!is.na(input$vrp_zmin) && input$vrp_zmin != 0) args <- c(args, "--zmin", input$vrp_zmin)
        if (isTRUE(input$vrp_etps)) args <- c(args, "--with-etps")
        withProgress(message = "Running VRP screener…", value = 0.5, run_screener(VRP_SCREENER, args))
    })
    output$vrp_console <- renderText({ req(vrp_res()); vrp_res()$console })
    output$vrp_table <- renderDT({
        req(vrp_res()$tbl)
        tbl <- vrp_res()$tbl %>%
            mutate(across(where(is.numeric), ~ round(., 3))) %>%
            arrange(desc(f_ivrv))
        DT::datatable(tbl, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE) %>%
            formatStyle("ivrv_pct", backgroundColor = styleInterval(90, c("white", "#d6f5d6")))
    })

    ff_res <- eventReactive(input$ff_run, {
        args <- c("--date", format(as.Date(input$c_date), "%Y%m%d"),
                  "--pair", input$ff_pair,
                  "--ff", input$ff_min,
                  "--min-debit", input$ff_min_debit)
        if (isTRUE(input$ff_no_ern)) args <- c(args, "--no-ern")
        if (input$ff_universe == "etf-only")   args <- c(args, "--etf-only")
        if (input$ff_universe == "names-only") args <- c(args, "--names-only")
        withProgress(message = "Running FF screener…", value = 0.5, run_screener(FF_SCREENER, args))
    })
    output$ff_console <- renderText({ req(ff_res()); ff_res()$console })
    output$ff_table <- renderDT({
        req(ff_res()$tbl)
        tbl <- ff_res()$tbl %>% arrange(desc(FF))
        DT::datatable(tbl, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE) %>%
            formatStyle("netEst_pctS", backgroundColor = styleInterval(0, c("#fbe3e3", "#d6f5d6"))) %>%
            formatStyle("FF", backgroundColor = styleInterval(c(0.2, 0.35), c("white", "#eef7e6", "#d6f5d6")))
    })

    # ---- Screen tab -------------------------------------------------------
    output$regime_banner <- renderText({
        d <- ORATS_core_v2 %>% dplyr::filter(tradeDate == input$s_date, !is_etp,
                                             avgOptVolu20d >= 2500, is.finite(ff_60_30))
        req(nrow(d) > 0)
        spy <- d %>% dplyr::filter(ticker == "SPY")
        breadth <- mean(d$ff_60_30 > 0, na.rm = TRUE) * 100
        spy_txt <- if (nrow(spy) == 1)
            sprintf("SPY iv30 %.1f%%  rv20 %.1f%%  IV/RV z %+.2f  FF60/30 %+.2f | ",
                    spy$iv30d, spy$clsHv20d, spy$ivrv_ts_z, spy$ff_60_30) else ""
        warn <- if (breadth > 50) "  << BROAD BACKWARDATION: 2020-style tail regime, size down" else ""
        sprintf("[regime %s] %s%.0f%% of clean universe in backwardation (FF60/30 > 0)%s",
                format(as.Date(input$s_date)), spy_txt, breadth, warn)
    })

    output$vrp_screen_plot <- renderPlot({
        df <- day_slice() %>% dplyr::filter(is.finite(f_ivrv))
        req(nrow(df) > 0)
        ggplot(df, aes(x = ivrv_xsec_pct, y = ivrv_ts_z, label = ticker)) +
            annotate("rect", xmin = 90, xmax = 102, ymin = -Inf, ymax = Inf,
                     fill = "darkseagreen", alpha = 0.25) +
            annotate("text", x = 96, y = -3.5, label = "SELL\nzone", size = 5, color = "darkgreen") +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill = class), max.overlaps = Inf, size = 3.5) +
            geom_hline(yintercept = 0) +
            geom_vline(xintercept = 90, linetype = "dashed") +
            xlim(c(-2, 102)) +
            labs(x = "IV/RV cross-sectional percentile (gated quantity)",
                 y = "IV/RV z vs own 1y history") +
            scale_fill_brewer(palette = "Accent") + theme_bw(base_size = 18) +
            theme(legend.position = "None")
    })

    output$ff_screen_plot <- renderPlot({
        pair <- input$s_ff_pair
        df <- day_slice() %>%
            mutate(
                ff = case_when(pair == "60_30" ~ ff_60_30, pair == "90_30" ~ ff_90_30,
                               pair == "90_60" ~ ff_90_60, pair == "180_90" ~ ff_180_90,
                               TRUE ~ NA),
                back_tenor = case_when(pair == "60_30" ~ 60, pair == "90_30" ~ 90,
                                       pair == "90_60" ~ 90, TRUE ~ 180),
                backIV = case_when(pair == "60_30" ~ exErnIv60d, pair == "90_30" ~ exErnIv90d,
                                   pair == "90_60" ~ exErnIv90d, TRUE ~ exErnIv6m),
                back_ratio = backIV / mean(backIV, na.rm = TRUE) - 1,
                ern_dirty = is.finite(daysToNextErn) & daysToNextErn > 0 & daysToNextErn <= back_tenor,
                viable = ff >= 0.2
            ) %>% dplyr::filter(is.finite(ff))
        if (isTRUE(input$s_ff_viable_only)) df <- df %>% dplyr::filter(viable)
        req(nrow(df) > 0)
        ggplot(df, aes(x = back_ratio, y = ff, label = ticker)) +
            geom_point(aes(shape = ern_dirty, alpha = viable), size = 2) +
            geom_label_repel(aes(fill = class, alpha = viable), max.overlaps = Inf, size = 4) +
            geom_hline(yintercept = 0) +
            geom_hline(yintercept = 0.20, linetype = "dashed") +
            geom_hline(yintercept = 0.35, linetype = "solid", color = "darkgreen") +
            geom_hline(yintercept = 0.50, linetype = "dotdash", color = "darkgreen") +
            annotate("text", x = Inf, y = 0.20, hjust = 1.05, vjust = -0.4, size = 4.5, label = "0.20 near-mid viable") +
            annotate("text", x = Inf, y = 0.35, hjust = 1.05, vjust = -0.4, size = 4.5, color = "darkgreen", label = "0.35 edge tail") +
            annotate("text", x = Inf, y = 0.50, hjust = 1.05, vjust = -0.4, size = 4.5, color = "darkgreen", label = "0.50 taker-OK") +
            scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1)) +
            scale_alpha_manual(values = c(`FALSE` = 0.35, `TRUE` = 1)) +
            labs(x = "Back-expiry IV vs universe mean", y = paste0("Forward factor ", gsub("_", "/", pair))) +
            scale_fill_brewer(palette = "Accent") + theme_bw(base_size = 24) +
            theme(legend.position = "None")
    })

    output$crskew_screen_plot <- renderPlot({
        df <- day_slice() %>% dplyr::filter(is.finite(cr_skew))
        req(nrow(df) > 0)
        ggplot(df, aes(x = cr_skew, y = ivrv_xsec_pct, label = ticker)) +
            annotate("rect", xmin = 1, xmax = Inf, ymin = 90, ymax = 102,
                     fill = "darkseagreen", alpha = 0.25) +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill = class), max.overlaps = Inf, size = 3.5) +
            geom_vline(xintercept = 1, linetype = "dashed") +
            geom_hline(yintercept = 90, linetype = "dashed") +
            labs(x = "cr_skew: call-wing richness z (own 1y history) — gate at 1",
                 y = "IV/RV cross-sectional percentile") +
            scale_fill_brewer(palette = "Accent") + theme_bw(base_size = 18) +
            theme(legend.position = "None")
    })

    output$screen_table <- renderDT({
        df <- day_slice() %>%
            dplyr::select(ticker, class, f_ivrv, ivrv_xsec_pct, ivrv_ts_z,
                          ff_60_30, ff_90_60, cr_skew,
                          ivPctile1y, slopeZscore, daysToNextErn, avgOptVolu20d) %>%
            mutate(across(where(is.numeric), ~ round(., 2))) %>%
            arrange(desc(ivrv_xsec_pct))
        req(nrow(df) > 0)
        DT::datatable(df, options = list(pageLength = 200, scrollX = TRUE), rownames = FALSE,
                      caption = "ivPctile1y / slopeZscore are CONTEXT columns (verified null / weak-negative)") %>%
            formatStyle("ivrv_xsec_pct", backgroundColor = styleInterval(90, c("white", "#d6f5d6"))) %>%
            formatStyle("ff_60_30", backgroundColor = styleInterval(c(0.2, 0.35), c("white", "#eef7e6", "#d6f5d6"))) %>%
            formatStyle("cr_skew", backgroundColor = styleInterval(1, c("white", "#d6f5d6")))
    })

    # ---- Ticker tab -------------------------------------------------------
    ticker_df <- reactive({
        ORATS_core_v2 %>% dplyr::filter(ticker == toupper(as.character(input$t_ticker))) %>%
            arrange(tradeDate) %>% ungroup()
    })

    output$t_price_vol <- renderPlotly({
        df <- ticker_df(); req(nrow(df) > 0)
        p1 <- plot_ly(df, x = ~tradeDate) %>%
            add_lines(y = ~pxAtmIv, name = "Price", line = list(color = "black")) %>%
            layout(yaxis = list(title = "Price"))
        p2 <- plot_ly(df, x = ~tradeDate) %>%
            add_lines(y = ~iv30d, name = "IV30", line = list(color = "blue")) %>%
            add_lines(y = ~clsHv20d, name = "RV20", line = list(color = "red")) %>%
            layout(yaxis = list(title = "IV30 vs RV20"))
        subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.06) %>%
            layout(font = list(size = 16), hovermode = "x unified", legend = list(x = 0.02, y = 0.45))
    })

    output$t_ivrv <- renderPlotly({
        df <- ticker_df(); req(nrow(df) > 0)
        p1 <- plot_ly(df, x = ~tradeDate) %>%
            add_lines(y = ~f_ivrv, name = "f_ivrv", line = list(color = "darkorange")) %>%
            layout(yaxis = list(title = "log(IV30/RV20)"))
        p2 <- plot_ly(df, x = ~tradeDate) %>%
            add_lines(y = ~ivrv_xsec_pct, name = "xsec pctile", line = list(color = "darkgreen")) %>%
            layout(yaxis = list(title = "IV/RV xsec pctile", range = c(0, 100)),
                   shapes = list(list(type = "line", x0 = min(df$tradeDate), x1 = max(df$tradeDate),
                                      y0 = 90, y1 = 90, line = list(dash = "dash", color = "darkgreen"))))
        subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.06) %>%
            layout(font = list(size = 16), hovermode = "x unified", showlegend = FALSE,
                   title = list(text = "The verified VRP signal (sell when xsec pctile ≥ 90)", font = list(color = "darkgray")))
    })

    output$t_ff <- renderPlotly({
        df <- ticker_df(); req(nrow(df) > 0)
        thresh_shapes <- lapply(list(c(0.20, "dash"), c(0.35, "solid"), c(0.50, "dotdash")), function(v)
            list(type = "line", x0 = min(df$tradeDate), x1 = max(df$tradeDate),
                 y0 = as.numeric(v[1]), y1 = as.numeric(v[1]),
                 line = list(dash = v[2], color = "darkgreen", width = 1)))
        p1 <- plot_ly(df, x = ~tradeDate) %>%
            add_lines(y = ~ff_60_30, name = "FF 30/60", line = list(color = "steelblue")) %>%
            add_lines(y = ~ff_90_60, name = "FF 60/90", line = list(color = "lightblue")) %>%
            layout(yaxis = list(title = "Forward factor"), shapes = thresh_shapes)
        p1 %>% layout(font = list(size = 16), hovermode = "x unified", legend = list(x = 0.02, y = 0.98),
                      title = list(text = "Forward factor (one-sided: 0.2 / 0.35 / 0.5)", font = list(color = "darkgray")))
    })

    output$t_crskew <- renderPlotly({
        df <- ticker_df(); req(nrow(df) > 0)
        p1 <- plot_ly(df, x = ~tradeDate) %>%
            add_lines(y = ~cskew, name = "dlt25Iv/ATM", line = list(color = "purple")) %>%
            layout(yaxis = list(title = "Call-wing / ATM IV"))
        p2 <- plot_ly(df, x = ~tradeDate) %>%
            add_lines(y = ~cr_skew, name = "cr_skew z", line = list(color = "mediumpurple")) %>%
            layout(yaxis = list(title = "cr_skew z"),
                   shapes = list(list(type = "line", x0 = min(df$tradeDate), x1 = max(df$tradeDate),
                                      y0 = 1, y1 = 1, line = list(dash = "dash", color = "darkgreen"))))
        subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.06) %>%
            layout(font = list(size = 16), hovermode = "x unified", showlegend = FALSE,
                   title = list(text = "Call-wing richness — filter for call-side/strangle choice (gate at z=1)", font = list(color = "darkgray")))
    })
}

# ---- Run app ----
shinyApp(ui = ui, server = server, options = list(height = 1080))
