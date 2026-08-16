
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
    # a cached parquet holds whatever universe was current when it was
    # written; these record it so a widened screener can be detected
    ORATS_universe_file <- paste0(options_dir, "ORATS_universe.rds")
    ORATS_ohlc_universe_file <- paste0(options_dir, "ORATS_ohlc_universe.rds")
    days_to_load <- 500

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
    quiet_fread(glue::glue(core_dir, "{filename}")) %>%
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
                     "ernDate1", "absAvgErnMv", "impliedIee"
)

# columns in cols_to_extract that must not be coerced to numeric on load
date_cols <- c("ticker", "tradeDate", "ernDate1")


# Widening the screener cannot be repaired by appending days: the new tickers
# have no history in the cache at all. Compare the universe a cache was built
# for against the one being asked for, so the rebuild triggers itself.
# TTR's rolling functions reject a series with interior NAs outright, and NaN
# counts as NA to them. Across ~4k single names there are many ways to produce
# one (a 0 or a negative through a log, a ratio of two 0s, a gap in coverage),
# so normalise instead of chasing each cause: anything non-finite becomes NA
# and is carried forward, leaving only leading NAs.
roll_safe <- function(x) {
    x[!is.finite(x)] <- NA
    na.locf(x, na.rm = FALSE)
}

.as_sig <- function(x) if (is.list(x)) x else sort(unique(x))
universe_changed <- function(sig_file, universe) {
    !file.exists(sig_file) || !identical(readRDS(sig_file), .as_sig(universe))
}
save_universe <- function(sig_file, universe) saveRDS(.as_sig(universe), sig_file)

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
        have <- rbind(have, purrr::map_df(todo, load_orats_daily, universe)) %>%
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
        tmp <- files_sorted[(index+1):length(files_sorted)] %>% purrr::map_df(.f = load_orats_day, cols_to_extract)
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
if (initialize || !exists("ORATS_core")) {
    ORATS_core <- load_ORATS_core(ORATS_core_file)
    files <- list.files(core_dir, "orats_core_202[0-9].*\\.csv\\.gz")
    last_real_day <- max(as.Date(sub("orats_core_([0-9]{8})\\.csv\\.gz", "\\1",
                                     basename(files)), format = "%Y%m%d"))
    loaded_last_day <- max(as.Date(ORATS_core$tradeDate))
    delayed_is_new <- file.exists(ORATS_code_delayed_file) &&
        as.Date(file.mtime(ORATS_code_delayed_file)) > loaded_last_day
    core_sig <- list(universe = sort(unique(combined_screener$Symbol)),
                     cols = sort(cols_to_extract))
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
if (initialize || !exists("ORATS_ohlc")) {
    ORATS_ohlc <- update_ORATS_ohlc(unique(ORATS_core$ticker))
    print(paste("ORATS_ohlc ready, last day:", max(ORATS_ohlc$tradeDate)))
}

# The Dashboard filters on an exact tradeDate, so its date box has to default
# to the last day in the data rather than the calendar date: on any weekend,
# holiday, or morning before the file lands, Sys.Date() matches nothing and
# every plot on the tab renders empty with no explanation.
last_data_day <- max(as.Date(ORATS_core$tradeDate))

# what the free-form screen scatter can plot against what
screen_vars <- c("ivPctile1y", "rvPctile1y", "VRP", "VRPzscore",
                 "steepness_30d90d", "steepness_30d6m", "ivHvXernRatio",
                 "ivSpyRatio", "correlSpy1m", "slope", "contango",
                 "volOfVol", "volOfIvol", "avgOptVolu20d",
                 "exErnIv30d", "clsHvXern20d",
                 "daysSinceErn", "absAvgErnMv", "impliedIee")

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
                                    checkboxInput("hide_center","Hide points in central region",value = FALSE ),
                                    selectInput("scr_xvar", "Screen X", choices = screen_vars,
                                                selected = "ivPctile1y"),
                                    selectInput("scr_yvar", "Screen Y", choices = screen_vars,
                                                selected = "VRPzscore"),
                                    textInput("scr_n_tickers", "Tickers to show", value = 50),
                                    # a floor screens on liquidity itself rather
                                    # than just taking the N most liquid; median
                                    # avgOptVolu20d is ~100, p90 ~4800
                                    textInput("scr_min_volu", "Min avg opt volume (20d)", value = 0),

                                    h3("IV plot"),
                                    textInput("iv_n_tickers", "Tickers to show", value = 50),
                                    # selectInput("iv_xvar", "X variable", choices = IV_x_choices, selected = IV_x_choices[1]),
                                    # selectInput("iv_yvar", "Y variable", choices = IV_y_choices, selected = IV_y_choices[1]),
                                    h3("RV plot"),
                                    textInput("rv_n_tickers", "Tickers to show", value = 50),
                                    #selectInput("rv_xvar", "X variable", choices = RV_x_choices, selected = RV_x_choices[1]),
                                    #selectInput("rv_yvar", "Y variable", choices = RV_y_choices, selected = RV_y_choices[1]),
                                    selectInput("rv_time_window", "Time window", choices = c(20, 60, 90, 120, 252), selected = 20),
                                    h3("Calendars plot"),
                                    textInput("cal_n_tickers", "Tickers to show", value = 50),
                                    selectInput("cal_front", "Front Expiry",  choices = c(30, 60, 90, 180), selected = 30),
                                    selectInput("cal_back", "Back Expiry",  choices = c(30, 60, 90, 180), selected = 60),
                                    h3("Ratio plot"),
                                    textInput("ratio_n_tickers", "Tickers to show", value = 50),
                                    h3("Tables"),
                                    textInput("table_n_tickers", "Rows to show", value = 200),
                                    selectInput("fd_ratio", "Term structure change",
                                                choices = c("Clicks", "Ratio"), selected = "Clicks"),
                                    textInput("fd_n_tickers", "Tickers to show", value = 50)

                                )
                         ),
                         column(width = 10, class = "main",
                                div(style = "height:100vh; display:flex; flex-direction:column;",
                                    div(style = "flex: 1 1 auto; overflow:auto; padding: 8px;",
                                        h1("Screen", style = "color: darkgray;"),
                                        helpText("Click any point or table row to open that ticker in the Ticker tab."),
                                        plotlyOutput("test_plot", height = "600px"),
                                        plotlyOutput("screen_plot", height = "600px"),
                                        h1("IV Plot", style = "color: darkgray;"),
                                        plotOutput("iv_plot", height = "600px"),
                                        h1("RV Plot", style = "color: darkgray;"),
                                        plotOutput("rv_plot", height = "600px"),
                                        h1("Ratio Plot", style = "color: darkgray;"),
                                        plotOutput("ratio_plot", height = "600px"),
                                        h1("Calendar Plot", style = "color: darkgray;"),
                                        plotOutput("cal_plot", height = "600px"),
                                        h1("Table", style = "color: darkgray;"),
                                        DTOutput("table_plot", height = "calc(100vh - 200px)"),
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
        if (is.finite(n) && n > 0) head(df, n = n) else df
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
        wide_ret <- ORATS_core %>% group_by(ticker) %>%
            mutate(log_ret = c(NA, diff(log(pxAtmIv)))) %>%
            select(tradeDate, ticker, log_ret) %>%
            pivot_wider(names_from = ticker, values_from = log_ret)
        wide_ret %>% select(-tradeDate) %>% cor(use = "pairwise.complete.obs")
    })



    
    
    output$test_plot<- renderPlotly({
        df <- dash_screen(as.numeric(input$scr_n_tickers))

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
            customdata = ~ticker,   # what the click handler reads
            source = "screen",
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
        p1 %>% layout(font = list(size = 16), showlegend = TRUE,
                      title = plot_title("Screen: IV percentile vs VRP z-score"))
    })
    
    output$iv_plot <- renderPlot({
        
        n_tickers <- as.numeric(input$iv_n_tickers)
        date <- input$date
        df <- dash_core() %>% group_by(ticker) %>% 
            mutate(
                IV_mom = RSI2(roll_safe(exErnIv30d), 20, maType="EMA"), 
                steepness_30d90d_mom = RSI2(roll_safe(steepness_30d90d), 20, maType="EMA"), 
                steepness_30d90d_Pctile1y = runPercentRank(roll_safe(steepness_30d90d), 252) * 100,
            ) %>% ungroup()
        df <- df %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        if(nrow(df) == 0)
            print("Empty data frame after filtering")
        need_rows(df)
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
        df <- dash_core()  %>% group_by(ticker)%>% arrange(tradeDate) %>% 
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
                VRPzscore = runZscore(roll_safe(VRP), 252), 
                RV_mom = RSI2(roll_safe(RV), 20, maType="EMA"), 
                rvPctile1y = TTR::runPercentRank(roll_safe(RV), 252) * 100
            ) 
        df <- df %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        need_rows(df)
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

    
    
    # Free-form screen: any column against any other, across the universe.
    # Native plot_ly rather than ggplotly(), which this app's ggplot2 breaks,
    # and native is what carries the click events through to the Ticker tab.
    output$screen_plot <- renderPlotly({
        n_tickers <- as.numeric(input$scr_n_tickers)
        xcol <- input$scr_xvar; ycol <- input$scr_yvar
        req(nzchar(xcol), nzchar(ycol), is.finite(n_tickers))
        df <- dash_screen(n_tickers)
        plot_ly(df, x = ~.data[[xcol]], y = ~.data[[ycol]],
                type = "scatter", mode = "markers",
                text = ~ticker, color = ~class, customdata = ~ticker,
                source = "screen",
                hovertemplate = paste0("%{text}<br>", xcol, ": %{x:.2f}<br>",
                                       ycol, ": %{y:.2f}<extra></extra>"),
                marker = list(size = 11, opacity = 0.9)) %>%
            layout(xaxis = list(title = xcol), yaxis = list(title = ycol),
                   font = list(size = 16),
                   title = plot_title(paste(ycol, "vs", xcol)))
    })

    # kept as its own reactive so the row-click handler can look up which
    # ticker a selected row belongs to
    table_df <- reactive({
        dash_screen(as.numeric(input$table_n_tickers)) %>%
            dplyr::transmute(
                ticker, tradeDate, class,
                iv30 = round(exErnIv30d, 1), rv20 = round(clsHvXern20d, 1),
                ivRvRatio = round(ivHvXernRatio, 2),
                ivPctile1y = round(ivPctile1y, 0), rvPctile1y = round(rvPctile1y, 0),
                VRP = round(VRP, 3), VRPz = round(VRPzscore, 2),
                steep30d6m = round(steepness_30d6m, 3), slope = round(slope, 2),
                # earnings proximity is inferred from the last reported date;
                # see the note on cols_to_extract
                daysSinceErn = round(daysSinceErn, 0),
                ernMove = round(absAvgErnMv, 1), impliedIee = round(impliedIee, 2),
                optVolu20d = round(avgOptVolu20d, 0))
    })

    output$table_plot <- renderDT({
        DT::datatable(table_df(), rownames = FALSE, selection = "single",
                      options = list(pageLength = 25))
    })

    observeEvent(input$table_plot_rows_selected, {
        open_in_ticker(table_df()$ticker[input$table_plot_rows_selected])
    })
    
    output$cal_plot <- renderPlot({
        n_tickers <- as.numeric(input$cal_n_tickers)
        cal_front <- as.numeric(input$cal_front)
        cal_back <- as.numeric(input$cal_back)
        date <- input$date
        df <- dash_core()  %>% group_by(ticker)%>% arrange(tradeDate) %>% 
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
        need_rows(df)

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
        df <- dash_core()  %>% group_by(ticker)%>% arrange(tradeDate) %>% 
            mutate(
                ivHvXernRatio = ivHvXernRatio %>% log,
                ivHvXernRatio_zscore = runZscore(roll_safe(ivHvXernRatio), 252),
                volOfIvol_w = ew_sd_roll(c(NA, diff(log(exErnIv30d))) %>% replace_na(0), 20),
                IV_IVVOL = log(exErnIv30d/volOfIvol_w),
                IV_IVVOL_zscore = runZscore(roll_safe(IV_IVVOL), 252)
            ) 
        df <- df %>% filter(tradeDate == date) %>% arrange(desc(avgOptVolu20d)) %>% head(n = n_tickers)
        need_rows(df)
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
            font = list(size = 16), showlegend = FALSE,
            title = plot_title("Variance risk premium - level and term structure")
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
    
    
    output$ticker_plot_8 <- renderPlotly({
        df <- ticker_df() %>%
            mutate(
                slope_pct = runPercentRank(roll_safe(slope), 252)
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
            font = list(size = 16), showlegend = FALSE,
            title = plot_title("SPY IV ratio and correlation | term-structure slope")
        )
        p
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
