{
    library(derivmkts)
    library(tidyverse)
    library(zoo)
    library(TTR)
    library(data.table)
    source("/home/marco/trading/Systems/Common/RiskManagement.R")
    source("/home/marco/trading/Systems/Common/Common.R")
}

# Convinience strategies for derivmkts package 
{
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
}

# Loading historical data
{
# Load options prices
{
    dir <- "/home/marco/trading/HistoricalData/Barchart/OTHER/Options/"
    Options <- list()
    for(f in list.files(paste0(dir, "Data/"))){
        symbol <- sub(".csv", "", f)
        df <- fread(paste0(dir, "Data/", f)) %>% as.data.frame
        df$Date <- as.Date(df$Date)
        df$Last <- NULL
        colnames(df) <- c("Date", "ImpVol", "IVChg", "IVRank", "IVPctl", "PCVol", "OptionsVol", "PCOI","TotalOI")
        for(n in c("ImpVol", "IVChg", "IVRank", "IVPctl"))
            df[,n] <- df[,n] %>% unlist %>% gsub("\\%|,", "", .) %>% as.numeric
        df <- arrange(df, Date)
        Options[[symbol]] <- df
    }
}
# Load assets prices
{
    Price <- list()
    for(f in list.files(paste0(dir, "Yahoo/"))){
        symbol <- sub(".csv", "", f)
        df <- fread(paste0(dir, "Yahoo/", f)) %>% as.data.frame()
        df$Date <- as.Date(df$Date)
        colnames(df)[6] <- c("AdjClose") 
        df <- arrange(df, Date)
        Price[[symbol]] <- df
    }
}
# Merge options and stocks/etfs
{
    Stocks_Info <- read_csv(paste0(dir, "Stock_Info.csv"), show_col_types = FALSE)
    ETFs_Info <- read_csv(paste0(dir, "ETF_Info.csv"), show_col_types = FALSE)
    Stocks <- list()
    ETFs <- list()
    hv_span <- 21
    for(f in names(Options)){
            if(nrow(Options[[f]]) < 252) next
            if(!f %in% names(Price)) next
            df <- full_join(Price[[f]] %>% select(Date, AdjClose), Options[[f]], by="Date") %>% 
                           mutate(
                                  Return = na.locf(log(AdjClose/lag(AdjClose)), na.rm=F), 
                                  HV_1 = runSD(Return, hv_span) * sqrt(252), 
                                  HV_2 = sqrt(runMean(Return^2, hv_span)) * sqrt(252), 
                                  HV_3 = calculate_volatility(Return, short_span = hv_span),
                                  ImpVol_lag = lag(ImpVol/100, hv_span),
                                  ) %>% 
                           mutate(Symbol = f, .after=Date) 
            if(f %in% ETFs_Info$Symbol)
                ETFs[[f]] <- df
            else if (f %in% Stocks_Info$Symbol)
                Stocks[[f]] <- df
    }
}
}

# Use gbm_vec to simulate a GBM
option_sim_profit <- function(gbm, type="call", premium = NULL, X = 100, tt = 1, sigma=0.3, r = 0, d = 0) {
    if(!type %in% c("call", "put"))
        stop(paste("Option type can be either call or put, not"), type)
    periods <- nrow(gbm)
    tte <- seq(tt, tt/periods, length.out=periods)
    if(type=="call")
        values <- apply(gbm, 2, function(x) bscall(x, X, sigma, r, tte, d))
    else if(type=="put")
        values <- apply(gbm, 2, function(x) bsput(x, X, sigma, r, tte, d))
    if(is.null(premium))
        premium <- values[1,]
    price <- as.vector(tail(gbm, 1))
    value <- as.vector(tail(values, 1))
    if(type=="call")
        payoff <- apply(cbind(price-X, 0), 1, max)
    else if(type=="put")
        payoff <- apply(cbind(X-price, 0), 1, max)
    profit <- payoff - premium
    return(data.frame(price=price, value=value, payoff=payoff, profit=profit))
}

# Simulate some strategies like E.Sinclair in Positional Options Trading
{

# Godd Straddle  
gbm <- gbm_vec(100000)
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

# Volatility Trading E.Sinclair, AAPL trade example
{
    aapl <- c(122.34, 119.65, 121.99, 120.56, 122.04, 121.26)
    straddle(122.27, 125, 40.85, 0, 1, 0, long = 0)
    bscallimpvol(124.08, 125, 0, 27/365, 0, 5.1) 
}

# Plot implied vol by group
{
    rm(Price); rm(Options)    
    Stocks_long <- do.call(rbind, Stocks)
    ETFs_long <- do.call(rbind, ETFs)
    #stocks_impvol <- Stocks_long %>% group_by(Symbol) %>% reframe(AvgVRP = median(log(lag(ImpVol/100, hv_span)) - log(HV_2), na.rm=T))
    stocks_impvol <- Stocks_long %>% group_by(Symbol) %>% 
        reframe(AvgVRP = median(lag(ImpVol/100, hv_span) - HV_2, na.rm=T), logAvgVRP = median(log(lag(ImpVol/100, hv_span)) - log(HV_2), na.rm=T))
    stocks_impvol <- full_join(stocks_impvol, Stocks_Info, by="Symbol")
    etfs_impvol <- ETFs_long %>% group_by(Symbol) %>% 
        reframe(AvgVRP = median(lag(ImpVol/100, hv_span) - HV_2, na.rm=T), logAvgVRP = median(log(lag(ImpVol/100, hv_span)) - log(HV_2), na.rm=T))
    etfs_impvol <- full_join(etfs_impvol, ETFs_Info, by="Symbol")
    
    etfs_impvol %>% mutate(G=Category)  %>% group_by(G) %>% 
        reframe(M=median(AvgVRP, na.rm=T), S=2*sd(AvgVRP, na.rm=T)/sqrt(n()), N=n()) %>% filter(N>=10) %>% na.omit %>% 
        arrange(M) %>% mutate(G=factor(G, levels=G)) %>% 
        ggplot() + geom_errorbar(aes(x=G, ymin=M-S, ymax=M+S), width=0.5) +  geom_point(aes(x=G, y=M))  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
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
