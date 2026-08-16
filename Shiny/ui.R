
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
}
{
    initialize <- FALSE   
    orats_dir <- "/home/marco/trading/HistoricalData/ORATS/"
    core_dir <- "/home/marco/trading/HistoricalData/ORATS/core/"
    delayed_dir <- "/home/marco/trading/HistoricalData/ORATS/delayed/"
    options_dir <- "/home/marco/trading/Systems/Options/Data/"
    ORATS_core_file <- paste0(options_dir, "ORATS_core.pq")
    ORATS_code_delayed_file <- paste0(delayed_dir, "orats_core_delayed.csv")
    days_to_load <- 500

    etf_screener <- read_csv("/home/marco/trading/Systems/Options/etf-screener-weekly-options.csv", show_col_types = F)
    stock_screener <- read_csv("/home/marco/trading/Systems/Options/stocks-screener-08-23-2025.csv", show_col_types = F)
    
today_date <- Sys.Date() 

# function for loading an ORATS core data file and returning a subset of columns
quiet_fread <- purrr::quietly((.f = fread))
load_orats_day <- function(filename, cols_to_extract) {
    print(filename)
    quiet_fread(glue::glue(core_dir, "{filename}")) %>%
        purrr::pluck("result")  %>%  
        select(all_of(cols_to_extract)) %>%   
        mutate(across(3:ncol(.), ~ as.single(.))) %>% # choose the right starting numeric column
        mutate(across(
            where(~ inherits(., "IDate")),
            ~ as.Date(.)
        ))
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
                     "slope", "contango", "deriv"
)


create_ORATS_core <- function(core_dir){
    print("Force load ORATS core file")
    files <- list.files(core_dir, "orats_core_202[0-9].*gz")
    files_sorted <- files[order(as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1", basename(files)),format = "%Y%m%d"))]
    ORATS_core <- files_sorted %>% tail(days_to_load) %>% purrr::map_df(.f = load_orats_day, cols_to_extract) %>% arrange(ticker, tradeDate)
    return(ORATS_core) 
}

init_ORATS_core <- function(ORATS_core, screener) {
    ORATS_core %>% select(-any_of("class")) %>% 
        right_join(screener %>% 
                       dplyr::select(Symbol, `Asset Class`), by=c("ticker" = "Symbol"),relationship="many-to-many") %>% 
        rename(class = `Asset Class`)%>% 
        group_by(ticker) %>% arrange(tradeDate) %>% dplyr::filter(n() >= days_to_load) %>% 
        mutate(
            class = if_else(is.na(class), "Stock", class),    
            VRP = log(lag(exErnIv30d, 20) / clsHvXern20d), # Realized VRP, NOT future VRP
            VRPzscore = runZscore(VRP, 252),
            rvPctile1y = TTR::runPercentRank(clsHvXern20d, 252) * 100,
            steepness_30d90d = log(exErnIv30d/exErnIv90d) %>% na.locf(na.rm=F),
            steepness_30d6m = log(exErnIv30d/exErnIv6m) %>% na.locf(na.rm=F)

        )  
}



# Slim OHLC store for the realized-vol estimators. The core file carries
# closes only; the dailies carry open/hi/lo, from the same downloader and the
# same trade dates. Seeded once from the last days_to_load + 300 dailies (the
# extra days warm up a 252d estimator at the left edge of the widest window),
# then only new days are appended on each app start - the regime_iv.pq
# pattern. Restricted to the core universe (~1.2k tickers of the 6.3k in a
# daily file), so it stays a few tens of MB.
dailies_dir <- paste0(orats_dir, "dailies/")
ORATS_ohlc_file <- paste0(options_dir, "ORATS_ohlc.pq")
ohlc_cols <- c("ticker", "tradeDate", "open", "hiPx", "loPx", "clsPx")

load_orats_daily <- function(filename, universe) {
    quiet_fread(glue::glue(dailies_dir, "{filename}")) %>%
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
    todo <- if (is.null(have)) files else files[dates > max(have$tradeDate)]
    if (length(todo)) {
        print(paste("Loading", length(todo), "ORATS dailies for OHLC"))
        have <- rbind(have, purrr::map_df(todo, load_orats_daily, universe)) %>%
            arrange(ticker, tradeDate)
        write_parquet(have, ORATS_ohlc_file)
    }
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
        tmp <- files_sorted[(index+1):length(files_sorted)] %>% purrr::map_df(.f = load_orats_day, cols_to_extract)
        ORATS_core <- rbind(ORATS_core, tmp) %>% arrange(ticker, tradeDate)
    }
    return(ORATS_core)
}

append_ORATS_delayed <- function(ORATS_core, ORATS_code_delayed_file) {
    if(file.exists(ORATS_code_delayed_file)) {
        ORATS_code_delayed <- read_csv(ORATS_code_delayed_file, show_col_types = FALSE) %>% dplyr::select(all_of(cols_to_extract))
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
if (initialize || !exists("ORATS_core")) {
    ORATS_core <- load_ORATS_core(ORATS_core_file)
    files <- list.files(core_dir, "orats_core_202[0-9].*\\.csv\\.gz")
    last_real_day <- max(as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1",
                                     basename(files)), format = "%Y%m%d"))
    loaded_last_day <- max(as.Date(ORATS_core$tradeDate))
    delayed_is_new <- file.exists(ORATS_code_delayed_file) &&
        as.Date(file.mtime(ORATS_code_delayed_file)) > loaded_last_day
    if (initialize || loaded_last_day < last_real_day || delayed_is_new) {
        print(paste("Updating ORATS_core:", loaded_last_day, "->", last_real_day))
        ORATS_core <- ORATS_core %>% ungroup() %>%
            dplyr::select(any_of(cols_to_extract))
        ORATS_core <- update_ORATS_core(ORATS_core, core_dir)
        ORATS_core <- append_ORATS_delayed(ORATS_core, ORATS_code_delayed_file)
        ORATS_core <- init_ORATS_core(ORATS_core, screener = etf_screener)
        write_parquet(ORATS_core %>% dplyr::filter(as.Date(tradeDate) <= last_real_day),
                      ORATS_core_file)
    }
    print(paste("ORATS_core ready, last day:", max(as.Date(ORATS_core$tradeDate))))
}

# OHLC for the realized-vol estimators. First run seeds the store (~30s);
# afterwards it is an append of whatever days are new.
if (initialize || !exists("ORATS_ohlc")) {
    ORATS_ohlc <- update_ORATS_ohlc(unique(ORATS_core$ticker))
    print(paste("ORATS_ohlc ready, last day:", max(ORATS_ohlc$tradeDate)))
}

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
        tabPanel("Dashboard",
                 div(class = "container-fluid",
                     fluidRow(
                         column(width = 2, class = "sidebar",
                                wellPanel(
                                    checkboxInput("hide_center","Hide points in central region",value = FALSE ),
                                    
                                    h3("IV plot"),
                                    dateInput("date", "Select date (this selects orats_core_YYYYMMDD.csv.gz)",
                                              value = today_date,
                                              format = "yyyy-mm-dd"
                                    ),
                                    textInput("iv_dir", "Data directory (optional)", value = core_dir),
                                    textInput("iv_n_tickers", "Tickers to show", value = 50),
                                    # selectInput("iv_xvar", "X variable", choices = IV_x_choices, selected = IV_x_choices[1]),
                                    # selectInput("iv_yvar", "Y variable", choices = IV_y_choices, selected = IV_y_choices[1]),
                                    h3("RV plot"),
                                    textInput("rv_n_tickers", "Tickers to show", value = 50),
                                    #selectInput("rv_xvar", "X variable", choices = RV_x_choices, selected = RV_x_choices[1]),
                                    #selectInput("rv_yvar", "Y variable", choices = RV_y_choices, selected = RV_y_choices[1]),
                                    selectInput("rv_time_window", "Time window", choices = c(20, 60, 90, 120, 252), selected = 20),
                                    actionButton("load", "Load file"),
                                    h3("Calendars plot"),
                                    textInput("cal_n_tickers", "Tickers to show", value = 50),
                                    selectInput("cal_front", "Front Expiry",  choices = c(30, 60, 90, 180), selected = 30),
                                    selectInput("cal_back", "Back Expiry",  choices = c(30, 60, 90, 180), selected = 60),
                                    h3("Ratio plot"),
                                    textInput("ratio_n_tickers", "Tickers to show", value = 50)

                                )
                         ),
                         column(width = 10, class = "main",
                                div(style = "height:100vh; display:flex; flex-direction:column;",
                                    div(style = "flex: 0 0 auto; padding: 8px;", verbatimTextOutput("file_info")),
                                    div(style = "flex: 1 1 auto; overflow:auto; padding: 8px;",
                                        h1("IV Plot", style = "color: darkgray;"),
                                        plotOutput("iv_plot", height = "600px"),
                                        h1("RV Plot", style = "color: darkgray;"),
                                        plotOutput("rv_plot", height = "600px"),
                                        h1("Ratio Plot", style = "color: darkgray;"),
                                        plotOutput("ratio_plot", height = "600px"),
                                        h1("Calendar Plot", style = "color: darkgray;"),
                                        plotOutput("cal_plot", height = "600px"),
                                        h1("Table", style = "color: darkgray;"),
                                        DTOutput("table_plot", height = "calc(100vh - 200px)")
                                    ),
                                    div(style = "flex: 0 0 auto; padding: 8px;", DT::dataTableOutput("table"))
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
                                                   selected = 24)

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
                                           plotlyOutput("ticker_plot_8")
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
        df %>% mutate(across(exErnIv10d:exErnIv1yr, ~ na_if(.x, 0)))
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
        est <- lapply(calcs, function(cc)
            as.numeric(TTR::volatility(x, n = n, calc = cc, N = 252)) * 100)
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
        wide_ret <- ORATS_core %>% group_by(ticker) %>%
            mutate(log_ret = c(NA, diff(log(pxAtmIv)))) %>%
            select(tradeDate, ticker, log_ret) %>%
            pivot_wider(names_from = ticker, values_from = log_ret)
        wide_ret %>% select(-tradeDate) %>% cor(use = "pairwise.complete.obs")
    })

    # Reactive to build filename when user clicks Load
    file_reactive <- eventReactive(input$load, {
        folder <- if (nzchar(input$iv_dir)) input$iv_dir else core_dir
        file_date <- format(as.Date(input$date), "%Y%m%d")
        fname <- file.path(folder, paste0("orats_core_", file_date, ".csv.gz"))
        list(path = fname, date = file_date)
    }, ignoreNULL = FALSE)


    
    
    output$test_plot<- renderPlotly({
        n_tickers <- as.numeric(input$iv_n_tickers)
        date <- input$date
        df <- ORATS_core %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        req(nrow(df) > 0)
        
        # ---- Optional: hide points in central box ----
        if (input$hide_center) {
            df <- df |>
                dplyr::filter(!( VRPzscore > -2 & VRPzscore < 2 & ivPctile1y > 25 & ivPctile1y < 75))
        }
        
        p1 <- plot_ly(
            df,
            x = ~ivPctile1y,
            y = ~VRPzscore,
            type = "scatter",
            mode = "markers",
            text = ~ticker,   # <-- column to show
            color = ~class,
            hovertemplate = paste(
                "Ticker: %{text}<br>",
                "x: %{x}<br>",
                "y: %{y}<extra></extra>"
            ),
            marker = list(size = 12, opacity = 1)
        ) %>%
        layout(
            xaxis = list(range = c(-5, 105)),
            yaxis = list(range = c(-4, 4)),
            shapes = list(
                list(type="rect", y0= 2, y1= 4, x0=75, x1=100,
                     fillcolor="lightgray", opacity=0.4, line=list(width=0)),
                list(type="rect", y0=-4, y1=-2, x0= 0, x1=25,
                     fillcolor="lightgray", opacity=0.4, line=list(width=0)),
                list(type="rect", y0= 2, y1= 4, x0= 0, x1=25,
                     fillcolor="lightgray", opacity=0.4, line=list(width=0)),
                list(type="rect", y0=-4, y1=-2, x0=75, x1=100,
                     fillcolor="lightgray", opacity=0.4, line=list(width=0)),
                list(type="line", y0=-2, y1=-2, x0=0, x1=100,
                     line=list(dash="dash")),
                list(type="line", y0= 2, y1= 2, x0=0, x1=100,
                     line=list(dash="dash")),
                list(type="line", y0=-4, y1=4, x0=25, x1=25,
                     line=list(dash="dash")),
                list(type="line", y0=-4, y1=4, x0=75, x1=75,
                     line=list(dash="dash"))
            )
        )
        p <- subplot(
            p1, p1,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16),showlegend = FALSE
        )
        p
        
    })
    
    output$iv_plot <- renderPlot({
        
        n_tickers <- as.numeric(input$iv_n_tickers)
        date <- input$date
        df <- ORATS_core %>% group_by(ticker) %>% 
            mutate(
                IV_mom = RSI2(exErnIv30d %>% na.locf(na.rm=F), 20, maType="EMA"), 
                steepness_30d90d_mom = RSI2(steepness_30d90d %>% replace(., is.infinite(.), NA) %>% na.locf(na.rm=F), 20, maType="EMA"), 
                steepness_30d90d_Pctile1y = runPercentRank(steepness_30d90d, 252) * 100,
            ) %>% ungroup()
        df <- df %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        if(nrow(df) == 0)
            print("Empty data frame after filtering")
        req(nrow(df) > 0)
        p1 <- ggplot(df, aes(x = ivPctile1y, y = IV_mom, label = ticker)) +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill=class), max.overlaps = Inf, size = 3.5) + geom_hline(yintercept = 0) + #ylim(c(0,100))+
            geom_vline(xintercept = 5, linetype = 'dashed') + geom_vline(xintercept = 95, linetype = 'dashed') +
            labs(x = "IV percentile", y = "IV momentum") +scale_fill_brewer(palette = "Accent") + theme_bw(base_size = 18) + theme(legend.position = "None")
        p2 <- ggplot(df, aes(x = steepness_30d90d_Pctile1y, y = steepness_30d90d_mom, , label = ticker)) +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill=class), max.overlaps = Inf, size = 3.5) + geom_hline(yintercept = 0) +
            geom_vline(xintercept = 5, linetype = 'dashed') + geom_vline(xintercept = 95, linetype = 'dashed') +
            xlim(c(0,100)) +  labs(x = "Steepness percentile", y = "Steepness momentum") +scale_fill_brewer(palette = "Accent") + theme_bw(base_size = 18) + theme(legend.position = "None")
        
        p1 + p2
        
    })
    

    output$rv_plot <- renderPlot({

        n_tickers <- as.numeric(input$rv_n_tickers)
        time_window <- as.numeric(input$rv_time_window)
        date <- input$date
        df <- ORATS_core  %>% group_by(ticker)%>% arrange(tradeDate) %>% 
            mutate(
                RV = case_when(
                    time_window == 20 ~ clsHvXern20d,
                    time_window == 60 ~ clsHvXern60d,
                    time_window == 90 ~ clsHvXern90d,
                    time_window == 120 ~ clsHvXern120d,
                    time_window == 252 ~ clsHvXern252d,
                    TRUE ~ NA
                ),
                IV = case_when(
                    time_window == 20 ~ exErnIv30d,
                    time_window == 60 ~ exErnIv60d,
                    time_window == 90 ~ exErnIv90d,
                    time_window == 120 ~ exErnIv6m,
                    time_window == 252 ~ exErnIv1yr,
                    TRUE ~ NA
                ),
                VRP = log(lag(IV, time_window) / RV),
                VRPzscore = runZscore(VRP %>% na.locf(na.rm=F), 252), 
                RV_mom = RSI2(RV %>% na.locf(na.rm=F), 20, maType="EMA"), 
                rvPctile1y = TTR::runPercentRank(RV %>% na.locf(na.rm=F), 252) * 100
            ) 
        df <- df %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        req(nrow(df) > 0)
        p1 <- ggplot(df, aes(x = VRP, y = VRPzscore, label = ticker)) +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill=class), max.overlaps = Inf, size = 3.5) + geom_hline(yintercept = 0) +
            labs(x = "VRP", y = "VRP z-score") +scale_fill_brewer(palette = "Accent") + theme_bw(base_size = 18) + theme(legend.position = "None")
        p2 <- ggplot(df, aes(x = rvPctile1y, y = RV_mom, label = ticker)) +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill=class), max.overlaps = Inf, size = 3.5)  + geom_hline(yintercept = 0) +
            geom_vline(xintercept = 5, linetype = 'dashed') + geom_vline(xintercept = 95, linetype = 'dashed') +
            xlim(c(0,100))  +labs(x = "RV percentile", y = "RV momentum") +scale_fill_brewer(palette = "Accent") + theme_bw(base_size = 18) + theme(legend.position = "None")
        
        p1 + p2
        
    })

    
    
    output$etf_plot <- renderPlot({

        n_tickers <- as.numeric(input$etf_n_tickers)
        time_window_rv <- as.numeric(input$etf_time_window)
        date <- input$date
        df <- ORATS_core  %>% group_by(ticker)%>% arrange(tradeDate) %>% 
            mutate(
                ivSpyRatioPctile1y = TTR::runPercentRank(ivSpyRatio, 252) * 100
            ) 
        df <- df %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        req(nrow(df) > 0)
        xcol <- input$etf_xvar
        ycol <- input$etf_yvar
        
        p <- ggplot(df, aes_string(x = xcol, y = ycol, label = "ticker")) +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill=class), max.overlaps = Inf, size = 4) + geom_hline(yintercept = 0) + #xlim(c(0,100)) +
            labs(x = xcol, y = ycol) +scale_fill_brewer(palette = "Accent")  + theme_bw(base_size = 24)
        
        p
        
    })
    
    output$table_plot <- renderDT({
        n_tickers <- as.numeric(input$table_n_tickers)
        n_tickers <- 200
        date <- input$date
        df <- ORATS_core  %>% group_by(ticker)%>% arrange(tradeDate) %>% 
            select("ticker", "tradeDate",  "steepness_30d6m", "ivPctile1y", "VRP", "rvPctile1y", "avgOptVolu20d")
        df <- df %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        req(nrow(df) > 0)

        DT::datatable(df, options = list(pageLength = n_tickers), rownames = FALSE) 
        
    })
    
    output$cal_plot <- renderPlot({
        n_tickers <- as.numeric(input$cal_n_tickers)
        cal_front <- as.numeric(input$cal_front)
        cal_back <- as.numeric(input$cal_back)
        date <- input$date
        df <- ORATS_core  %>% group_by(ticker)%>% arrange(tradeDate) %>% 
            mutate(
                    ff = case_when(
                        cal_front == 30 & cal_back == 60 ~ exErnIv30d / fexErn60_30 - 1,
                        cal_front == 30 & cal_back == 90 ~ exErnIv30d / fexErn90_30 - 1,
                        cal_front == 60 & cal_back == 90 ~ exErnIv60d / fexErn90_60 - 1,
                        cal_front == 90 & cal_back == 180 ~  exErnIv90d / fexErn180_90 - 1,
                        TRUE ~ NA
                    ),
                    back_ratio = case_when(
                        cal_front == 30 & cal_back == 60 ~ exErnIv60d / mean(exErnIv60d) - 1,
                        cal_front == 30 & cal_back == 90 ~ exErnIv90d / mean(exErnIv90d) - 1,
                        cal_front == 60 & cal_back == 90 ~ exErnIv90d / mean(exErnIv90d) - 1,
                        cal_front == 90 & cal_back == 180 ~  exErnIv6m / mean(exErnIv6m) - 1,
                        TRUE ~ NA
                        
                    )
            ) 
        df <- df %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        req(nrow(df) > 0)

        p <- ggplot(df, aes(x = back_ratio, y = ff, label = ticker)) +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill=class), max.overlaps = Inf, size = 4) + 
            geom_hline(yintercept = 0) + geom_hline(yintercept = 0.2, linetype="dashed")+ geom_hline(yintercept = -0.2, linetype="dashed")+  #xlim(c(0,100)) +
            labs(x = "Back Expiry Ratio", y = "Forward Factor") +scale_fill_brewer(palette = "Accent")  + theme_bw(base_size = 24)
        
        p
        
    })
    
    
    output$ratio_plot <- renderPlot({
        n_tickers <- as.numeric(input$ratio_n_tickers)
        date <- input$date
        df <- ORATS_core  %>% group_by(ticker)%>% arrange(tradeDate) %>% 
            mutate(
                ivHvXernRatio = ivHvXernRatio %>% log,
                ivHvXernRatio_zscore = runZscore(ivHvXernRatio %>% na.locf(na.rm=F), 252),
                volOfIvol_w = ew_sd_roll(c(NA, diff(log(exErnIv30d))) %>% replace_na(0), 20),
                IV_IVVOL = log(exErnIv30d/volOfIvol_w),
                IV_IVVOL_zscore = runZscore(IV_IVVOL %>% na.locf(na.rm=F), 252)
            ) 
        df <- df %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        req(nrow(df) > 0)
        p1 <- ggplot(df, aes(x = ivHvXernRatio, y = IV_IVVOL, label = ticker)) +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill=class), max.overlaps = Inf, size = 3.5) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
            labs(x = "IV/RV log_ratio", y = "IV/IVVOL log_ratio") +scale_fill_brewer(palette = "Accent") + theme_bw(base_size = 18) + theme(legend.position = "None")
        p2 <- ggplot(df, aes(x = ivHvXernRatio_zscore, y = IV_IVVOL_zscore, label = ticker)) +
            geom_point(alpha = 1, size = 1) +
            geom_label_repel(aes(fill=class), max.overlaps = Inf, size = 3.5) + 
            geom_vline(xintercept = -2, linetype = 'dashed') + geom_vline(xintercept = 2, linetype = 'dashed') +
            geom_hline(yintercept = -2, linetype = 'dashed') + geom_hline(yintercept = 2, linetype = 'dashed') +
            xlim(c(-4,4)) + ylim(c(-4,4)) + labs(x = "IV/RV log_ratio z-score", y = "IV/IVVOL log_ratio z-score") +scale_fill_brewer(palette = "Accent") + theme_bw(base_size = 18) + theme(legend.position = "None")
        
        p1 + p2
        
    })
    
    output$strike_plot <- DT::renderDT({
        type <- input$fd_ratio
        n_tickers <- as.numeric(input$fd_n_tickers)
        date <- input$fd_date
        if(type == "Ratio") vals <- c(-10, 0, 10) else vals <- c(-20, 0, 20)
        brks <- quantile(vals, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
        cols <- colorRampPalette(c("#d20231", "orange1", "lightblue", "#218be7"))(4)
        df <- ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) #%>% slice_tail(n=2)
        tops <- df %>% group_by(ticker) %>% slice_tail(n=1) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        df_table <- df %>% dplyr::filter(ticker %in% tops$ticker) %>% 
            dplyr::select(ticker, tradeDate, class, exErnIv10d:exErnIv1yr) %>% 
            group_by(ticker) %>% arrange(tradeDate) %>% 
            mutate(across(exErnIv10d:exErnIv1yr, ~case_when(type == "Clicks" ~ round(.x - lag(.x),2),
                                                      type == "Ratio" ~ round((.x / lag(.x) - 1)*100,2),
                                                      TRUE ~ NA
                                                      ))) 
        df_table <- df_table %>% dplyr::filter(tradeDate == date) %>% ungroup
        req(nrow(df_table) > 0)
        DT::datatable(df_table, options = list(pageLength = n_tickers), rownames = FALSE) %>%  formatStyle(
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
        t_dte <- 25
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
            data = df %>% mutate(iv_chg = c(0,diff(exErnIv30d)), log_ret = c(0, diff(log(pxAtmIv)))) %>% filter(dtExM1 <= t_dte) %>% t_win,
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
            font = list(size = 16),showlegend = FALSE
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
                legend = list(x = 0.1, y = 0.9),font = list(size = 16)
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
                font = list(size = 16)
            )
    })

    output$ticker_plot_3 <- renderPlotly({
        t_dte <- 25
        # the cones are min/max/quartiles over the visible window, so the
        # lookback follows the Time Range selector
        df <- ticker_df() %>% t_win



        stats <- list(
            min = ~min(.x, na.rm = TRUE),
            max   = ~max(.x, na.rm = TRUE),
            q25  = ~quantile(.x, 0.25, na.rm = TRUE),
            q75  = ~quantile(.x, 0.75, na.rm = TRUE),
            current = ~last(.x),
            previous = ~head(tail(.x, 30), 1)
        )
        
        IV_expiries <- c("10d","30d","60d","90d","6m","1y")
        cone_df_IV <- map_dfr(stats, function(f) {
            summarise(df,
                      across(exErnIv10d:exErnIv1yr, f))
        }, .id = "stat") %>% t %>% as.data.frame() 
        colnames(cone_df_IV) <- cone_df_IV[1,]; cone_df_IV <- cone_df_IV[-1,]
        cone_df_IV$horizon <-  factor(IV_expiries, levels = IV_expiries)
        cone_df_IV <- cone_df_IV %>% mutate(across(min:previous, ~as.numeric(.x)))
        
        RV_expiries <- c("10d","20d", "60d","90d","120d","252d")
        cone_df_RV <- map_dfr(stats, function(f) {
            summarise(df,
                      across(orHvXern10d:orHvXern252d, f))
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
                title = "Volatility Cone",
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
                title = "Volatility Cone",
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
            font = list(size = 16),showlegend = FALSE
        )
        p
        
    })
    
    output$ticker_plot_4 <- renderPlotly({
        t_vol_window <- as.character(input$t_vol_window)
        df <- ticker_df() %>%
            mutate(
                VRP_10 = log(lag(exErnIv10d, 10) / clsHvXern10d),
                VRP_30 = log(lag(exErnIv30d, 20) / clsHvXern20d),
                VRP_60 = log(lag(exErnIv60d, 60) / clsHvXern60d),
                VRP_90 = log(lag(exErnIv90d, 60) / clsHvXern60d), # there is no RV for 40 days
                VRP_120 = log(lag(exErnIv6m, 120) / clsHvXern120d),
                VRP_252 = log(lag(exErnIv1yr, 252) / clsHvXern252d))

        # lags need the full history; trim only once they are computed
        df <- t_win(df)

        df <- df %>% mutate(VRP =  case_when(
                                                t_vol_window == "30d" ~ VRP_30,
                                                t_vol_window == "60d" ~ VRP_60,
                                                t_vol_window == "90d" ~ VRP_90,
                                                t_vol_window == "6m" ~ VRP_120,
                                                t_vol_window == "1yr" ~ VRP_252)
        )
        
        p1 <- plot_ly(
            df,
            x = ~tradeDate,
            y = ~VRP,
            line=list(color='darkorange'),marker=list(color='darkorange'),
            type = "scatter",
            mode = "lines+markers",
            hovertemplate = "Date: %{x}<br>VRP: %{y:,}<extra></extra>"
        ) %>%
            layout(
                xaxis = list(title = ""),
                yaxis = list(title = "logVRP")
            )
        
        
        vrp_term_structure <- df %>% 
            dplyr::select(tradeDate, VRP_10:VRP_252) %>% pivot_longer(-tradeDate) %>% 
            separate(name, sep="_", into=c("VRP", "horizon")) %>% 
            group_by(horizon) %>% mutate(value = if_else(is.infinite(value), NA, value)) %>% 
            reframe(q25=quantile(value, na.rm=T)[2],q75=quantile(value, na.rm=T)[3], current=last(value)) %>% 
            arrange(as.numeric(horizon)) %>% 
            mutate(horizon = factor(horizon, levels = unique(horizon)))
        
        p2 <- plot_ly(vrp_term_structure, x = ~horizon) %>%
            
            add_ribbons(ymin = ~q25, ymax = ~q75,
                        name = "IQR (25–75%)",
                        line = list(width = 0),
                        fillcolor = "rgba(0,255,0,0.3)") %>%
            add_trace(
                y = ~current,
                line=list(color='darkorange'),marker=list(color='darkorange'),
                type = "scatter",
                mode = "lines+markers"
            )%>%
            layout(
                xaxis = list(title = ""),
                yaxis = list(title = "logVRP")
            )
        
        p <- subplot(
            p1, p2,
            #shareX = TRUE,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16),showlegend = FALSE
        )
        p
        
    })
    
    output$ticker_plot_5 <- renderPlotly({
        df <- ticker_df() %>%
            mutate(
                iv_hv_ratio = (exErnIv30d / clsHvXern20d) %>% log,
                iv_hv_ratio_pct = runPercentRank(iv_hv_ratio %>% na.locf(na.rm=F), 252) * 100,
                volOfIvol_w = ew_sd_roll(c(NA, diff(log(exErnIv30d))) %>% replace_na(0), 20) *100,
                IVVVOL_ratio = exErnIv30d / volOfIvol_w,
                IVVVOL_ratio_pct = runPercentRank(IVVVOL_ratio, 252)

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
            font = list(size = 16),showlegend = FALSE
        )
        
        p
        
    })
    
    output$ticker_plot_6 <- renderPlotly({
        t_dte <- 25
        df <- ticker_df()

        df_ff <- df %>% mutate(
            ff_60_30 = exErnIv30d / fexErn60_30 - 1,
            ff_90_30 = exErnIv30d / fexErn90_30 - 1,
            ff_90_60 = exErnIv60d / fexErn90_60 - 1,
            ff_180_90 = exErnIv90d / fexErn180_90 - 1
        ) %>% t_win

        p1 <- plot_ly(df_ff %>% tail(1) %>% dplyr::select(ticker,ff_60_30:ff_180_90) %>% pivot_longer(-ticker) %>% 
            mutate(name=factor(name, levels=c("ff_60_30", "ff_90_30", "ff_90_60", "ff_180_90"))),
        x = ~name, y = ~value, type = "bar") %>%
            layout(
                shapes = list(
                    list(type = "line",
                         x0 = -1, x1 = Inf,
                         y0 = 0.2, y1 = 0.2,
                         line = list(color = "red", dash = "dash")),
                    list(type = "line",
                         x0 = -1, x1 = Inf,
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
            font = list(size = 16),showlegend = FALSE
        )
        p
    })
    
    output$ticker_plot_7 <- renderPlotly({
        t_dte <- input$t_dte
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
        df <- df %>% mutate(
            straProM1 = abs(pxAtmIvM1 - hiStrikeM1) - straPxM1, 
            straProM2 = abs(pxAtmIvM2 - hiStrikeM2) - straPxM2,
            straRetM1 = straProM1 / pxAtmIv, 
            straRetM2 = straProM2 / pxAtmIv) %>%  
            mutate(
                straProM1 = case_when(abs(pxAtmIvM1 - hiStrikeM1)/pxAtmIvM1/sqrt(dtExM1+1) > 0.1 | straPxM1 > pxAtmIv*10 | straPxM1 == 0 | dtExM1 == 1 ~ NA, TRUE ~ straProM1),
                straProM2 = case_when(abs(pxAtmIvM2 - hiStrikeM2)/pxAtmIvM2/sqrt(dtExM2+1) > 0.1 | straPxM2 > pxAtmIv*10 | straPxM2 == 0 | dtExM2 == 1 ~ NA, TRUE ~ straProM2),
                straRetM1 = case_when(abs(pxAtmIvM1 - hiStrikeM1)/pxAtmIvM1/sqrt(dtExM1+1) > 0.1 | straPxM1 > pxAtmIv*10 | straPxM1 == 0 | dtExM1 == 1 ~ NA, TRUE ~ straRetM1),
                straRetM2 = case_when(abs(pxAtmIvM2 - hiStrikeM2)/pxAtmIvM2/sqrt(dtExM2+1) > 0.1 | straPxM2 > pxAtmIv*10 | straPxM2 == 0 | dtExM2 == 1 ~ NA, TRUE ~ straRetM2)#,
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
        
        p2 <- plot_ly(df,
            x = ~tradeDate) %>%
            add_lines(y = ~PnL1, name = "PnL1", line = list(color = "darkgray")) %>%
            add_lines(y = ~PnL2, name = "PnL2", line = list(color = "lightgray")) %>%
            layout(
                xaxis = list(title = ""), yaxis = list(title = "Straddle Returns")            )
        
        p <- subplot(
            p1, p2,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06   
        ) %>% layout(
            font = list(size = 16),showlegend = FALSE
        )
        p
    })
    
    
    output$ticker_plot_8 <- renderPlotly({
        df <- ticker_df() %>%
            mutate(
                slope_pct = runPercentRank(slope %>% na.locf(na.rm=F), 252)
            ) %>% t_win   # 252d rank computed on the full history first

        # SPY ratio
        p1 <- plot_ly(df ,
            x = ~tradeDate) %>%
            add_lines(y = ~ivSpyRatio, name = "ivSpyRatio", line = list(color = "brown")) %>%
            add_lines(y = ~correlSpy1m, name = "correlSpy1m", line = list(color = "darkcyan")) %>%
            layout(
                xaxis = list(title = ""), yaxis = list(title = "IV ETF Ratios"))

        
        p2_1 <- plot_ly(df ,
                        x = ~tradeDate) %>%
            add_lines(y = ~slope, name = "slope", line = list(color = "pink3")) %>%
            layout(
                xaxis = list(title = ""), yaxis = list(title = "Slope"))
        p2_2 <- plot_ly(df ,
                        x = ~tradeDate) %>%
            add_lines(y = ~slope_pct, name = "slope pct", line = list(color = "purple")) %>%
            layout(
                xaxis = list(title = ""), yaxis = list(title = "Slope pct"))
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
            font = list(size = 16),showlegend = FALSE
        )
        
        #p1
    })
    
    output$pairs_plot <- renderPlot({
        ticker_1 <- pair_1_deb()
        ticker_2 <- pair_2_deb()
        corr_window <- as.numeric(input$run_window)
        df1 <- ORATS_core %>% dplyr::filter(ticker == ticker_1) %>% ungroup %>% 
            mutate(retAtmIv = c(NA, diff(log(pxAtmIv))), retAtmIv = remove_outliers(retAtmIv) * 100) %>% 
            mutate(retIv = c(NA, diff(log(exErnIv30d))), retIv = remove_outliers(retIv) * 100) 
        df2 <- ORATS_core %>% dplyr::filter(ticker == ticker_2) %>% ungroup %>% 
            mutate(retAtmIv = c(NA, diff(log(pxAtmIv))), retAtmIv = remove_outliers(retAtmIv) * 100) %>% 
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
        df_rv <- df %>% group_by(tradeDate) %>% last() %>% ungroup %>% dplyr::select(contains("orHvXern"))
        df_rv <- df_rv %>% t %>% as.data.frame() %>% rownames_to_column() %>% 
            separate(rowname, sep="\\.", into=c("horizon", "ticker")) %>% rename(RV=V1) %>% 
            mutate(horizon = factor(sub("orHvXern", "", horizon), levels=RV_expiries),  ticker=case_when(ticker=="x" ~ ticker_1,ticker=="y" ~ ticker_2, TRUE~NA))
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
    
    output$sim_strangle_plot <- renderPlot({
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
