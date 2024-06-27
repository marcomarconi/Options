{
    library(derivmkts)
    library(tidyverse)
    library(zoo)
    library(TTR)
    library(data.table)
    library(lubridate)
    library(tsibble)
    source("/home/marco/trading/Systems/Common/RiskManagement.R")
    source("/home/marco/trading/Systems/Common/Common.R")
    theme_set(theme_bw(base_size = 24))
}

# Convenience functions
{
round_to_nearest <- function(x, n=0.05) {
        round(x / n) * n
    }    
    
calloption <- function(s, k, v, r, tt, d, long) {
    bscall(s, k, v, r, tt, d) * long
}

putoption <- function(s, k, v, r, tt, d, long) {
    bsput(s, k, v, r, tt, d) * long
}
    
putoption <- function(s, k, v, r, tt, d, long) {
    bsput(s, k, v, r, tt, d) * long
}

bullspread <- function(s, k1, k2, v, r, tt, d, long) {
    ( bscall(s, k1, v, r, tt, d) - bscall(s, k2, v, r, tt, d) ) * long
}

straddle <- function(s, k, v, r, tt, d, long) {
    ( bscall(s, k, v, r, tt, d) + bsput(s, k, v, r, tt, d) ) * long
}

strangle <- function(s, k1, k2, v, r, tt, d, long) {
    ( bscall(s, k1, v, r, tt, d) + bsput(s, k2, v, r, tt, d) ) * long
}

butterfly <- function(s, k, k1, k2, v, r, tt, d, long) {
    ( - bscall(s, k, v, r, tt, d) - bscall(s, k, v, r, tt, d) + bscall(s, k1, v, r, tt, d) + bscall(s, k2, v, r, tt, d) ) * long
}

ironbutterfly <- function(s, k, k1, k2, v, r, tt, d, long) {
    ( - bscall(s, k, v, r, tt, d) - bsput(s, k, v, r, tt, d) + bsput(s, k1, v, r, tt, d) + bscall(s, k2, v, r, tt, d) ) * long
}

# Use gbm_vec to simulate a GBM
option_sim_profit <- function(gbm, type="call", premium = NULL, X = 100, tt = 1, v=0.3, r = 0, d = 0, hedging = FALSE) {
    if(!type %in% c("call", "put"))
        stop(paste("Option type can be either call or put, not"), type)
    periods <- nrow(gbm)
    tte <- seq(tt, tt/periods, length.out=periods)
    if(type=="call")
        values <- apply(gbm, 2, function(x) bscall(x, X, v, r, tte, d))
    else if(type=="put")
        values <- apply(gbm, 2, function(x) bsput(x, X, v, r, tte, d))
    if(is.null(premium))
        premium <- values[1,]
    hedges <- rep(0, nrow(gbm))
    if(hedging) {
        if(type=="call")
            deltas <- greeks(bscall(values, X, v, r, tt, d), complete=FALSE, long=FALSE, initcaps=FALSE)["delta",]
        else if(type=="put")
            deltas <- greeks(bsput(values, X, v, r, tt, d), complete=FALSE, long=FALSE, initcaps=FALSE)["delta",]
    }
    values <- values + hedges
    price <- as.vector(tail(gbm, 1))
    value <- as.vector(tail(values, 1))
    if(type=="call")
        payoff <- apply(cbind(price-X, 0), 1, max)
    else if(type=="put")
        payoff <- apply(cbind(X-price, 0), 1, max)
    profit <- payoff - premium
    return(data.frame(price=price, value=value, payoff=payoff, profit=profit, hedge=hedges))
}

}

# Load assets prices (Yahoo)
{
    Price_data <- list()
    Returns <- list()
    type <- "Stocks/"# Stocks
    dir <- paste0(main_dir, "Yahoo/", type)
    vol_shift <- 21; period <- 252
    for(f in list.files(dir, pattern = "*.csv")){
        symbol <- sub(".csv", "", f)
        df <- fread(paste0(dir, f)) %>% as.data.frame()
        colnames(df)[6] <- c("AdjClose") 
        df <- mutate(df, 
            Date = as.Date(df$Date),
            Symbol = symbol,
            ReturnPrice = log(AdjClose / lag(AdjClose)), 
            ReturnPrice = replace_na(ReturnPrice, 0),
            HV1 = runSD(ReturnPrice, vol_shift) * sqrt(period), 
            HV2 = calculate_volatility(ReturnPrice, long_span = period, short_span = vol_shift, period = period),
            RV = lead(HV1, vol_shift)
        )
        df <- arrange(df, Date)
        Price_data[[symbol]] <- df
    }
}

# Load single file assets prices (Yahoo) created with for f in `find . -name "*csv"`; do g=`basename $f | sed 's/.csv//'`; echo $g; cat $f | sed 's/^/'$g',/g'  ;done | grep -v Date > full_price_data.csv
{
    vol_shift <- 21; period <- 252
    full_price_data <- read_csv("/home/marco/trading/HistoricalData/Barchart/Options/full_price_data.csv")
    full_price_data <- mutate(full_price_data %>% rename(AdjClose = `Adj Close`), 
                 ReturnPrice = log(AdjClose / lag(AdjClose)), 
                 ReturnPrice = replace_na(ReturnPrice, 0),
                 HV1 = runSD(ReturnPrice, vol_shift) * sqrt(period), 
                 HV2 = calculate_volatility(ReturnPrice, long_span = period, short_span = vol_shift, period = period),
                 RV = lead(HV1, vol_shift)
    )
    full_price_data <- arrange(full_price_data, Date)
}


# Loading barchart historical volatility data (scrape from pages like https://www.barchart.com/stocks/quotes/AAPL/options-history)
{
    # Load barchart options prices
    {
        main_dir <- "/home/marco/trading/HistoricalData/Barchart/Options/"
        type <- "Stocks/" # Stocks
        Options_data <- list()
        dir <- paste0(main_dir, "Data/", type)
        for(f in list.files(dir, pattern = "*.csv")){
            symbol <- sub(".csv", "", f)
            df <- fread(paste0(dir, f)) %>% as.data.frame
            df$Date <- as.Date(df$Date)
            # Columns after the 7th randomly appears
            df <- df[,1:9]
            colnames(df) <- c("Date", "ImpVol", "IVChg", "IVRank", "IVPctl", "PCVol", "OptionsVol", "PCOI","TotalOI")
            for(n in c("ImpVol", "IVChg", "IVRank", "IVPctl"))
                df[,n] <- df[,n] %>% unlist %>% gsub("\\%|,", "", .) %>% as.numeric
            df <- arrange(df, Date)
            Options_data[[symbol]] <- df
        }
    }
    
    # Merge barchart options and yahoo prices
    {
        # Load Infos
        dir <- "/home/marco/trading/HistoricalData/Barchart/Options/"
        ETFs_Info <- read_csv(paste0(dir, "ETF_Info.csv"), show_col_types = FALSE)
        Stocks_Info <- read_csv(paste0(dir, "Stock_Info.csv"), show_col_types = FALSE)
        # Merge options data and price data, calculate volatilities, filter bad data
        load_price_data <- function(options_data, price_data, vol_shift = 21, period = 252) {
            a <- lapply(names(options_data), function(n) {
                        if(nrow(options_data[[n]]) < period) return(NULL)
                        else {
                        full_join(options_data[[n]], price_data[[n]], by="Date") %>% mutate(Symbol=n, .after=Date) %>% arrange(Date) %>% 
                        mutate(
                               ImpVol = case_when(ImpVol == 0 ~ NA, TRUE ~ ImpVol),
                               ImpVol = ImpVol / 100,
                               IVChg = IVChg / 100,
                               Return = log(AdjClose / lag(AdjClose)), 
                               Return = replace_na(Return, 0),
                               HistVol1 = runSD(Return, vol_shift) * sqrt(period), 
                               HistVol2 = sqrt(runMean(Return^2, vol_shift)) * sqrt(period), 
                               HistVol3 = calculate_volatility(Return, long_span = period, short_span = vol_shift, period = period),
                               RealVol = lead(HistVol1, vol_shift),
                               VRP = ImpVol - RealVol,
                               VRPlog = log(ImpVol / RealVol),
                               VRP_return_pct = VRP / ImpVol
                               #VRP_return_log = log(VRP_return_pct/100 + 1),
                               ) 
                        }
            }
                    )
            return(a)
        }
        # Call this for ETFs
        ETFs <- load_price_data(Options_data, Price_data)
        names(ETFs) <- names(Options_data)
        ETFs <- do.call(rbind, ETFs) %>% inner_join(., ETFs_Info, keep=FALSE, by="Symbol")
        # Call this for Stocks
        Stocks <- load_price_data(Options_data, Price_data)
        names(Stocks) <- names(Options_data)
        Stocks <- do.call(rbind, Stocks) %>% inner_join(., Stocks_Info, keep=FALSE, by="Symbol")
        saveRDS(Stocks, "/home/marco/trading/Systems/Options/Stocks.RDS")
    }
}

# Loading barchart historical option data (the ones individually scraped from pages like https://www.barchart.com/stocks/quotes/SPY%7C20200814%7C321.00C/price-history/historical)
{
    dir <- "/home/marco/trading/HistoricalData/Barchart/OTHER/Options/Data/Daily/SPY/" 
    prefix <- "_opt."
    delim <- "%7C"
    files <- list()
    for (f in list.files(dir, pattern = "*csv", full.names = T)) {
        df <- fread(f, sep=",")
        items <- (f %>% basename %>% sub(prefix, "", .) %>% sub(".csv", "", .) %>% strsplit(., delim))[[1]]
        symbol <- items[1]
        expiry <- as.Date(items[2], format="%Y%m%d")
        strike <- as.numeric(substr(items[3], 1, nchar(items[3])-1))
        type <- substr(items[3], nchar(items[3]), nchar(items[3]))
        df <- df %>% mutate(Symbol=symbol, Expiry=expiry, Strike=strike, Type=type, .before = tradeTime)
        files[[f]] <- df
    }
    df <- do.call(rbind, files) %>% rename(Date=tradeTime, Open=openPrice,  High=highPrice, Low=lowPrice, , Close=lastPrice, IV=impliedVolatility) %>% mutate(Date=as.Date(Date, format="%m/%d/%Y"), IV=as.numeric(sub("%", "", IV))) %>% arrange(Date)
    df$ATM <- ifelse(df$Strike == round(df$baseLastPrice, 1), TRUE, FALSE)
    SPY <- df
    # 30-days IV
    IV30 <- SPY %>% filter(ATM==TRUE) %>% group_by(Date) %>% reframe(IVm = mean(IV))
}

# Load Dolthub option data
{
    
    # Generate ATM straddles (you need the lists Price_data and Stocks_Info, see above)
    {
        Straddles <- list()
        dir <- "/home/marco/trading/HistoricalData/Dolthub/Split_by_stock//"
        for(f in list.files(dir, pattern = "*.csv")){
            df <- fread(paste0(dir, f)) %>% as.data.frame() %>% rename(Date=date, Symbol=act_symbol) %>% mutate(Date=as.Date(Date), expiration=as.Date(expiration), dte = expiration-Date)
            symbol <- as.character(head(df$Symbol, 1))
            print(symbol)
            if(symbol %in% names(Straddles)) next
            prices <- Price_data[[symbol]]
            if(is.null(prices) || nrow(Price_data[[symbol]]) < 500 | !symbol %in% Stocks_Info$Symbol) next
            df <- inner_join(df, prices, by=c("Date", "Symbol")) 
            if(nrow(df) == 0) next
            stdls <- df %>% 
                mutate(diff = abs(Close - strike)) %>% # I think Close is more correct than AdjClose (we are before dividends split)
                group_by(Symbol, Date, expiration, call_put) %>% mutate(m=min(diff), ATM=m==diff) %>% 
                group_by(Symbol, Date, expiration, strike) %>% reframe(straddle = sum(ask), spread=sum(ask-bid)/2, IV = sum(vol), delta=sum(delta), gamma=sum(gamma), theta=sum(theta), vega=sum(vega), rho=sum(rho), dte=first(dte), ATM=first(ATM), Close=first(Close)) %>% 
                mutate(ID=paste(Symbol, expiration, strike, sep="_")) %>% arrange(Date) %>% 
                group_by(ID) %>% mutate(Return=log(lead(straddle)/straddle), Cost=spread/straddle+lead(spread)/lead(straddle)) %>% # Here Return is the future return, in case you are entering the position now
                filter(ATM==TRUE)
            stdls <- cbind(stdls, Stocks_Info %>% filter(Symbol == symbol) %>% dplyr::select(-Symbol)) 
            stdls <- inner_join(stdls %>% dplyr::select(-Close), Price_data[[symbol]], by=c("Symbol", "Date")) %>% mutate(VRP = IV - RV, VRPlog = log(IV / RV))  %>% filter(!is.infinite(VRPlog)) %>% arrange(Date) 
            Straddles[[symbol]] <- stdls
        }
    }
    straddles <- do.call(rbind, Straddles)
    # Plots
    straddles %>% group_by(Symbol) %>% reframe(VRP=median(VRPlog, na.rm=T), Class=first(`Sector`)) %>% ggplot(aes(y=VRP, x=Class, group=Class)) + geom_boxplot() + geom_hline(yintercept = 0)+ theme(axis.text.x = element_text(angle = 45))
    straddles %>% filter(!is.infinite(Return)) %>% mutate(dte=round(dte/10)) %>% group_by(Symbol, dte) %>% reframe(Ms=median(VRPlog, na.rm=T)) %>% group_by(dte) %>% reframe(M=mean(Ms, na.rm=T), S=sd(Ms, na.rm=T)/sqrt(n()*2), N=n()) %>% filter(N>100) %>% ggplot(aes(x=dte, ymin=M-S, ymax=M+S)) + geom_errorbar(width=0.5) + geom_hline(yintercept = 0)    
}

# Load Dolthub volatility data
{
    dolthub_vol <- read_csv("/home/marco/trading/HistoricalData/Dolthub/post-no-preference_options_master_volatility_history.csv.gz")
    dolthub_vol <- dolthub_vol %>% rename(Symbol=act_symbol, Date=date) %>% group_by(Symbol) %>% mutate(rv_current = lead(hv_current, 13), VRPlog = log(iv_current / rv_current)) # 13 because we only have mondays, wednesdays and fridays
    dolthub_vol <- full_join(dolthub_vol, Stocks_Info, by="Symbol") %>% ungroup() %>% filter(!is.na(Name) & !is.na(Date))
    dolthub_vol <- full_join(dolthub_vol, full_price_data, by=c("Symbol", "Date")) 
}


# Plot some general observations from barchart data 
{
    # Load saves Stocks and ETFs data
    Stocks <- readRDS("/home/marco/trading/Systems/Options/Stocks.RDS")
    #ETFs <- readRDS("/home/marco/trading/Systems/Options/ETFs.RDS")
    
    # Keep only ETFs with < 20% NAs in ImpVol after 2021 (most data have NAs in the first years)
    reduced_etfs <- ETFs %>% group_by(Symbol) %>% filter(Date > "2021-01-01") %>% filter(sum(is.na(ImpVol))/length(ImpVol) < 0.2) %>% ungroup
    reduced_etfs$Vega <- unlist(greeks(bscall(reduced_etfs$Close, reduced_etfs$Close, reduced_etfs$ImpVol, r=0, tt=30/365, d=0))["Vega",] + 
                                    greeks(bsput(reduced_etfs$Close, reduced_etfs$Close, reduced_etfs$ImpVol, r=0, tt=30/365, d=0))["Vega",])
    reduced_etfs <- reduced_etfs %>% mutate(Profit = Vega * (ImpVol - RealVol))
    # VRP by asset class, AUM and NAV
    reduced_etfs %>% group_by(Symbol) %>% reframe(VRP=median(VRPlog, na.rm=T), Class=first(`Asset Class`)) %>% ggplot(aes(y=VRP, x=Class)) + geom_boxplot() + geom_hline(yintercept = 0)
    reduced_etfs %>% group_by(Symbol) %>% reframe(VRP=median(VRPlog, na.rm=T), Class=first(`AUM`))  %>% mutate(G=ntile(Class, 10))%>% ggplot(aes(y=VRP, x=factor(G))) + geom_boxplot() + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 45))
    reduced_etfs %>% group_by(Symbol) %>% reframe(VRP=median(VRPlog, na.rm=T), Class=first(`NAV`))  %>% mutate(G=ntile(Class, 10))%>% ggplot(aes(y=VRP, x=factor(G))) + geom_boxplot() + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 45)) 
    reduced_etfs %>% group_by(Symbol) %>% reframe(VRP=median(VRPlog, na.rm=T), Class=first(`Mgmnt Fee`))  %>% mutate(G=ntile(Class, 10))%>% ggplot(aes(y=VRP, x=factor(G))) + geom_boxplot() + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 45))
    
    # Keep only Stocks with < 20% NAs in ImpVol betweem 2021 and 2024 (most data have NAs in the first years)
    reduced_stocks <- Stocks %>% group_by(Symbol) %>% filter(between(Date , "2021-01-01", "2024-01-01")) %>% filter(sum(is.na(ImpVol))/length(ImpVol) < 0.1) %>% ungroup %>% filter(VRPlog < Inf)
    stocks <- reduced_stocks # so you can use it generically
    # VRP by class
    classv <- "Sector"
    stocks %>% group_by(Symbol) %>% reframe(VRP=median(VRPlog, na.rm=T), Class=first(!!rlang::sym(classv) )) %>% ggplot(aes(y=VRP, x=Class)) + geom_boxplot() + geom_hline(yintercept = 0)+ theme(axis.text.x = element_text(angle = 45))
    stocks %>% mutate(Decile = ntile((`Market Cap`), 10)) %>% group_by(Symbol, Decile) %>% reframe(VRP=median(VRPlog, na.rm=T)) %>% ggplot(aes(y=VRP, x=Decile, group=Decile)) + geom_boxplot() + geom_hline(yintercept = 0)+ theme(axis.text.x = element_text(angle = 45))
    # VRP by hist vol levels and sector
    stocks %>% group_by(Symbol) %>% mutate(Decile = ntile(lag(HistVol1, 1), 10)) %>% group_by(Decile) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    stocks %>% group_by(Sector) %>%  mutate(Decile = ntile(lag(HistVol1), 20)) %>% group_by(Decile, Sector) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + geom_hline(yintercept = 0) + geom_hline(yintercept = 0.13, color="red", linetype=2) + facet_wrap(~Sector, scales = "free") + ylim(c(-0.1, 0.35))
    # VRP by vol of vol
    stocks %>% mutate(VolVol = runSD(log(HistVol1/lag(HistVol1)) %>% na.locf(na.rm=F), 21),  Decile = ntile(lag(VolVol, 1), 10)) %>% group_by(Decile) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    stocks %>% group_by(Sector) %>% mutate(VolVol = runSD(log(HistVol1/lag(HistVol1)) %>% na.locf(na.rm=F), 21),  Decile = ntile(lag(VolVol), 10)) %>% group_by(Decile, Sector) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Sector, scales = "free")
    # VRP by momentum
    stocks %>% group_by(Symbol) %>% mutate(Momentum = RSI(AdjClose %>% na.locf(na.rm=F), 21, maType=EMA)/50-1,  Decile = ntile(lag(Momentum), 10)) %>% group_by(Decile) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    stocks %>% group_by(Symbol) %>% mutate(Momentum = RSI(AdjClose %>% na.locf(na.rm=F), 21, maType=EMA)/50-1,  Decile = ntile(lag(Momentum), 10)) %>% group_by(Decile, Sector) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()  + facet_wrap(~Sector, scales = "free")
    stocks %>% group_by(Symbol) %>% mutate(Momentum = EMA(Return, 21)/runSD(Return, 21),  Decile = ntile(lag(Momentum), 10)) %>% group_by(Decile) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    stocks %>% group_by(Symbol) %>% mutate(Momentum = EMA(Return, 21)/runSD(Return, 21),  Decile = ntile(lag(Momentum), 10)) %>% group_by(Decile, Sector) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Sector, scales = "free")
    # VRP by week day
    stocks %>% group_by(Decile=wday(Date), Sector) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()+ facet_wrap(~Sector)
    # VRP by higher moments
    stocks %>% group_by(Symbol) %>% mutate(Skew = rollapply(Return, width=21, skew,  fill=NA, align="right"),  Decile = ntile(lag(Skew), 10)) %>% group_by(Decile) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    stocks %>% group_by(Symbol) %>% mutate(Skew = rollapply(Return, width=21, skew,  fill=NA, align="right"),  Decile = ntile(lag(Skew), 10)) %>% group_by(Decile, Sector) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()  + facet_wrap(~Sector, scales = "free") 
    stocks %>% group_by(Symbol) %>% mutate(CR = carver_ratio(Return, n=21),  Decile = ntile(lag(CR), 10)) %>% group_by(Decile) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    stocks %>% group_by(Symbol) %>% mutate(CR = carver_ratio(Return, n=21),  Decile = ntile(lag(CR), 10)) %>% group_by(Decile, Sector) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()  + facet_wrap(~Sector, scales = "free") 
    # Using ad hod clustering (see stocks example for clustering)
    stocks %>% group_by(Symbol) %>% mutate(Momentum = EMA(Return, 21)/runSD(Return, 21)) %>% group_by(Date, Cluster) %>% mutate(Decile=ntile(rank(lag(Momentum)), 10)) %>% group_by(Decile) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    
}



# SPY playing
{
    SPY_opt <- read_csv("/home/marco/trading/HistoricalData/Dolthub/Split_by_stock//sample_SPY.csv", show_col_types = F) %>% mutate(mid = (bid+ask)/2, cost = (ask-bid)/ask, id = paste(act_symbol, expiration, strike, call_put, sep="_"), wday = lubridate::wday(date, label=T), dte = as.integer(expiration-date))
    vol_shift <- 21
    SPY_price <- read_csv("/home/marco/trading/HistoricalData/Barchart/Options/Yahoo//ETFs/SPY.csv", show_col_types = F) %>% 
        rename(date=Date, AdjClose = `Adj Close`) %>% 
        mutate(ReturnPrice = log(AdjClose / lag(AdjClose)),                                                                                                                                                          
               ReturnPrice = replace_na(ReturnPrice, 0),
               Return1week = log(AdjClose / lag(AdjClose, 5)),
               Return6month = log(AdjClose / lag(AdjClose, 120)),
               HV1 = runSD(ReturnPrice, vol_shift) * sqrt(252), 
               HV2 = calculate_volatility(ReturnPrice, long_span = 252, short_span = vol_shift, period = 252),
               RV = lead(HV1, vol_shift))    
    SPY <- merge(SPY_opt, SPY_price, by="date") 
    SPY <- SPY %>% mutate(VRP = vol - RV, VRPlog = log(vol / RV), VRPprofit = vega * (vol - RV), VRPprofit_ = vega/(RV*(365/dte)) * (vol^2 - RV^2))
    # IV vs RV
    SPY %>% na.omit%>% mutate(delta_ = case_when(call_put=="Put" ~ abs(delta), TRUE ~ 1-delta)) %>% group_by(decile1 = date, decile2=round(delta_, 1)) %>% reframe(RV=first(RV), IV=mean(vol)) %>% ggplot(aes(x=decile1)) + geom_line(aes(y=RV), color="red")+ geom_line(aes(y=IV), color="blue") + facet_wrap(~decile2, scales = "free")
    # VRP (vega weighted)
    SPY %>% na.omit %>% mutate(delta_ = case_when(call_put=="Put" ~ abs(delta), TRUE ~ 1-delta)) %>% 
        group_by(decile = round(delta_, 1), call_put) %>% reframe(Mean=median(VRPprofit), SD=sd(VRPprofit)/sqrt(n())) %>% ggplot(aes(x=decile, y=Mean, ymin=Mean-SD*2, ymax=Mean+SD*2, color=call_put)) + geom_line() + geom_errorbar(width=0.025)
    SPY %>% na.omit %>% mutate(delta_ = case_when(call_put=="Put" ~ abs(delta), TRUE ~ 1-delta)) %>% 
        group_by(decile1 = round(delta_, 1), decile2 = round(dte / 10), call_put) %>% reframe(Mean=median(VRPprofit), SD=mad(VRPprofit)/sqrt(n())*2) %>% ggplot(aes(x=decile1, y=Mean, ymin=Mean-SD*2, ymax=Mean+SD*2, color=call_put)) + geom_line() + geom_errorbar(width=0.025)+ facet_wrap(~decile2)  
    # Volatility smile
    SPY %>% mutate(delta_ = case_when(call_put=="Put" ~ abs(delta), TRUE ~ 1-delta)) %>% group_by(decile = round(delta_, 1), call_put) %>% reframe(Mean=median(vol), SD=sd(vol)/sqrt(n())) %>% ggplot(aes(x=decile, y=Mean, ymin=Mean-SD*2, ymax=Mean+SD*2, color=call_put)) + geom_line() + geom_errorbar(width=0.025)    
    SPY %>% mutate(delta_ = case_when(call_put=="Put" ~ abs(delta), TRUE ~ 1-delta)) %>% group_by(decile1 = round(delta_, 1), decile2 = round(dte / 10), call_put) %>% reframe(Mean=median(vol), SD=sd(vol)/sqrt(n())) %>% ggplot(aes(x=decile1, y=Mean, ymin=Mean-SD*2, ymax=Mean+SD*2, color=call_put)) + geom_line() + geom_errorbar(width=0.025) + facet_wrap(~decile2)   
    # Weekend effect?
    SPY_we <- mutate(SPY, yw = yearweek(date, week_start=2))
    SPY_we %>% filter(call_put=="Call") %>% group_by(id, yw)  %>% reframe(Time=-c(NA, diff(dte)), Diff=c(NA, diff(mid))/lag(mid)*100, Day=wday, delta=round(delta, 1)) %>% na.omit %>% filter(Diff!=0) %>% group_by(Time) %>% reframe(Mean=median(Diff), SD=mad(Diff)/sqrt(n()), N=n()) 
}


# Simulate some strategies like E.Sinclair in Positional Options Trading
{
    
    # Good Straddle  
    gbm <- gbm_vec(1)
    option_sim_profit(gbm, type="call") -> a
    option_sim_profit(gbm, type="put") -> b
    profit <- (-a$profit-b$profit)*100
    hist(profit, 50); summary(profit,)
    # Good Strangle
    gbm <- gbm_vec(10000)
    option_sim_profit(gbm, type="put", X = 70) -> a
    option_sim_profit(gbm, type="call", X = 130) -> b
    profit <- (-a$profit-b$profit)*100*1.68
    hist(profit, 50)
    # Bad Straddle Vol  
    gbm <- gbm_vec(10000, sigma = 0.7)
    option_sim_profit(gbm, type="call") -> a
    option_sim_profit(gbm, type="put") -> b
    profit <- (-a$profit-b$profit)*100
    hist(profit, 50)
    # Bad Strangle Vol
    gbm <- gbm_vec(10000, sigma = 0.7)
    option_sim_profit(gbm, type="put", X = 70) -> a
    option_sim_profit(gbm, type="call", X = 130) -> b
    profit <- (-a$profit-b$profit)*100*1.68
    hist(profit, 50)
    # Bad Straddle Drift  
    gbm <- gbm_vec(10000, mu = 0.2)
    option_sim_profit(gbm, type="call") -> a
    option_sim_profit(gbm, type="put") -> b
    profit_straddle <- (-a$profit-b$profit)*100
    # Bad Strangle Drift
    gbm <- gbm_vec(10000, mu = 0.2)
    option_sim_profit(gbm, type="put", X = 70) -> a
    option_sim_profit(gbm, type="call", X = 130) -> b
    profit_strangle <-(-a$profit-b$profit)*100*1.68
    plot(density(profit_straddle), xlim=c(-20000, 3000), lwd=2); lines(density(profit_strangle), col="blue", lwd=2)
    
}

# Simulate some examples from "The second leg down" - Hari Krishnan
{
    # Replicating call portfolio (probably wrong)
    gbm <- gbm_vec(1, sigma = 0.1) %>% as.vector
    tte <- seq(1, 1/365, length.out=365)
    values <-  bscall(gbm, 100, 0.2, 0, tte, 0)
    deltas <- greeks(bscall(gbm, 100, 0.2, 0, tte, 0), complete=FALSE, long=FALSE, initcaps=FALSE)["delta",]
    portfolio <- deltas * gbm - (deltas * 100 - values)
    matplot2(cbind(portfolio, values))
    # Figure 3.23
    SPY %>% filter(call_put=="Put") %>% mutate(delta_ = abs(round(delta, 1))) %>% filter(delta_ %in%  c(0.2, 0.5)) %>% group_by(date, expiration) %>% mutate(N=n()) %>% filter(N==2) %>% arrange(delta_, .by_group = TRUE) %>% group_by(date, expiration) %>% reframe(Vol=(first(vol)-last(vol))*100, Return=first(Return6month)*100) %>% ggplot(aes(Return, Vol)) + geom_point() + geom_smooth(method = "lm") + ylim(c(-15,15))
    # Figure 3.42
    SPY %>% filter(call_put=="Put") %>% filter(between(dte, 20,40)) %>% mutate(delta_ = abs(round(delta, 1))) %>% filter(delta_ %in%  c(0.2, 0.5)) %>% group_by(date, expiration) %>% mutate(N=n()) %>% filter(N==2) %>% arrange(delta_, .by_group = TRUE) %>% group_by(date, expiration) %>% reframe(Vol=(first(vol)-last(vol))*100) %>% ggplot(aes(date, Vol)) + geom_line() 
    # Figure 4.5
    SPY %>% filter(call_put=="Put")  %>% mutate(delta_ = abs(round_to_nearest(delta, 0.05))) %>% group_by(id) %>% mutate(Profit = log(mid / lag(mid))) %>% group_by(delta_)%>% filter(!is.infinite(Profit) & between(dte, 20, 40)) %>% reframe(M=-mean(Profit/HV1, na.rm=T)) %>% ggplot(aes(x=delta_,y=M)) + geom_bar(stat="identity")
}


# Plot greeks (as described in derivmkts)
{
    k <- 100; r <- 0; v <- 0.30; tt <- 1; d <- 0
    S <- seq(50, 150, by=1)
    Call <- greeks(bscall(S, k, v, r, tt, d), long = TRUE)
    Put <- greeks(bsput(S, k, v, r, tt, d), long = TRUE)
    ggplot(rbind(Call, Put), aes(x = s, y = value, color = funcname )) +
        geom_line() + facet_wrap(~ greek, scales = 'free_y') +
        scale_color_discrete(name = 'Option', labels = c('Call','Put' )) +
        scale_x_continuous('Stock', breaks =c(0, 100, 200) ) +
        scale_y_continuous('Value')
}

# Quantra backtesting
{
    df_strike <- DF
    df_strike$R <- round(df_strike$`[UNDERLYING_LAST]`/25)*25
    df_strike <- filter(df_strike, `[STRIKE]`==R)
    df_strike$Return <- log(df_strike$`[UNDERLYING_LAST]` / lag(df_strike$`[UNDERLYING_LAST]`))
    df_strike$RV <- runMean(df_strike$Return^2, 20) ; 
    df_strike$Signal <- -sign(mean(c(df_strike$`[C_IV]`, df_strike$`[P_IV]`)) - df_strike$RV)
    df_0 <- filter(DF, `[DTE]`==0)
    res <- list()
    for(i in 2:30) {
        df <- filter(df_strike, `[DTE]`==i)
        df_merge <- merge(df, df_0, by="[EXPIRE_DATE]") %>% 
            dplyr::select(`[EXPIRE_DATE]`, `[QUOTE_DATE].x`, `[QUOTE_DATE].y`, `[STRIKE].x`, `[STRIKE].y`, `[C_IV].x`, `[C_IV].y`, `[P_IV].x`, `[P_IV].y`,`[C_LAST].x`, `[C_LAST].y`, `[P_LAST].x`, `[P_LAST].y`, RV, Signal) %>% 
            filter(`[STRIKE].x` == `[STRIKE].y`) %>% mutate(PnL = -(`[C_LAST].y` - `[C_LAST].x`) + -(`[P_LAST].y` - `[P_LAST].x`))
        res[[as.character(i)]] <- data.frame(Date=df_merge$`[QUOTE_DATE].y`, PnL=df_merge$PnL) %>% arrange(Date)
    }
    full <- Reduce(function(...) full_join(..., by = "Date"), res) %>% arrange(Date) #%>% dplyr::select(-Date) %>% apply(., 2, function(x) replace(x, is.na(x), 0))
    colnames(full) <- as.character( 2:30)
    full[,-1] <- apply(full[,-1], 2, cumsum)
}

