{
library(tidyverse)
library(tsibble)
library(roll)
library(TTR)
library(Rfast)
library(zoo)
library(data.table)
library(ggthemes)
    library(PerformanceAnalytics)
    source("/home/marco/trading/Systems//Monopoly//Futures.R")
    
    }

    
running_ntile <- function(x, n=252, k=4) {
    rv <- runquantile(x, k = n, probs = seq(0, 1, length.out=k+1), align = "right")
    q <- sapply(1:(ncol(rv)-1), function(j) ifelse(x >= rv[,j] & x < rv[,j+1] , j, 0))
    q[,(ncol(rv)-1)] <- ifelse(x == rv[ ,ncol(rv)], ncol(rv)-1, q[,(ncol(rv)-1)])
    fc <- rowSums(q)
    return(fc)
}

# Random
{
    symbol <- "ZL"
    dir <- "/home/marco/trading/HistoricalData/Barchart/Soyoil///"
    df <- load_future_contracts_long(symbol, dir)
    df_ <- load_future_contracts_wide(symbol, dir)
    spreads <- build_spreads(df, C = list(c(1,2),c(2,3),c(3,4),c(4,5))) %>% filter(between(Date, "2001-01-01", "2023-01-01"))
    backadj <- backadjust_future(df_, N = 2)
    
    plot.ts(cumsum(na.omit((backadj$Return * lag(backadj$Basis)))))
    
    # Plot the spreads
    spreads %>% na.omit %>% #filter(Date > "2020-01-01") %>% 
        group_by(Contracts) %>%
        mutate(slope_zscore = (SpreadLog - roll::roll_mean(SpreadLog, 252))/roll::roll_sd(SpreadLog, 252)) %>%
        mutate(slope_zscore = SpreadLog) %>%
        ggplot(aes(x=Date, y=slope_zscore, color=Contracts)) + geom_line(linewidth=1)  + scale_color_colorblind()

    # Plot performance by spread
    spreads %>%
        group_by(Contracts) %>%
        arrange(Date) %>%
        mutate(
            spreadvol = roll::roll_sd(SpreadReturn, 32) * sqrt(252),
            scaledreturns = 0.2 / lag(spreadvol) * SpreadReturn
        ) %>%
        na.omit() %>%
        group_by(Contracts) %>%
        arrange(Date) %>%
        mutate(cumreturns = cumsum(-scaledreturns)) %>%
        ggplot(aes(x=Date, y=cumreturns, color=Contracts)) + geom_line(linewidth=2) + ggtitle('VX spreads volatility adjusted - cumulative returns') + scale_color_colorblind()
    
    # Returns performance by slope strenght
    spreads %>% na.omit %>% 
        group_by(Contracts) %>%
        mutate(
            slope_zscore = (SpreadLog - roll::roll_mean(SpreadLog, 252))/roll::roll_sd(SpreadLog, 252),
        ) %>%
        mutate(lagbasis = ntile(lag(slope_zscore), 5)) %>%
        group_by(Contracts, lagbasis) %>%
        summarize(meanreturn = mean(SpreadReturn), sdreturn = sd(SpreadReturn)/sqrt(n())) %>%
        ggplot(aes(x=lagbasis, y=meanreturn)) + geom_bar(stat='identity') +
        geom_errorbar(aes(ymin=meanreturn-sdreturn,ymax=meanreturn+sdreturn), width=0.25)+ facet_wrap(~Contracts)
    
    # Returns performance by slope strenght
    spreads %>% na.omit %>% 
        group_by(Contracts) %>%
        mutate(
            slope_zscore = (SpreadLog - roll::roll_mean(SpreadLog, 252))/roll::roll_sd(SpreadLog, 252),
            spreadvol = roll::roll_sd(SpreadReturn, 32) * sqrt(252),
            scaledreturns = 0.2 / lag(spreadvol) * SpreadReturn
        ) %>%na.omit %>% 
        mutate(lagbasis = ntile(lag(slope_zscore), 10), returns = scaledreturns*(lag(slope_zscore))/10) %>% na.omit %>% 
        #group_by(Contracts) %>% reframe(SR=mean(returns)/sd(returns)*16)
        mutate(cumreturns = cumsum(returns)) %>%
        ggplot(aes(x=Date, y=cumreturns, color=Contracts)) + geom_line(linewidth=2) + ggtitle('VX spreads volatility adjusted - cumulative returns') + scale_color_colorblind()
    
    spreads  %>% na.omit %>% 
        filter(Contracts == 'c_12') %>%
        group_by(x=month(Date)) %>%
        summarize(M =  mean(SpreadReturn), S = sd(SpreadReturn) / sqrt(n())) %>%
        ggplot(aes(x=x, y=M)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=M-S, ymax=M+S), width=0.25)

}


# Here I replicate "VIX_Calendars_for_Fun_and_Profit.ipynb"
{
    vx <- load_future_contracts_long("VI", "/home/marco/trading/HistoricalData/Barchart/VIX/")
    
    spreads <- build_spreads(vx, C = list(c(1,2),c(2,3),c(3,4),c(4,5),c(5,6)))
    
    # Plot performance by spread
    spreads %>%
        group_by(Contracts) %>%
        arrange(Date) %>%
        mutate(
            spreadvol = roll::roll_sd(SpreadReturn, 32) * sqrt(252),
            scaledreturns = 0.2 / lag(spreadvol) * SpreadReturn
        ) %>%
        na.omit() %>%
        group_by(Contracts) %>%
        arrange(Date) %>%
        mutate(cumreturns = cumsum(scaledreturns)) %>%
        ggplot(aes(x=Date, y=cumreturns, color=Contracts)) + geom_line(linewidth=2) + ggtitle('VX spreads volatility adjusted - cumulative returns') + scale_color_colorblind()
    
    # All spreads performance 
    spreads %>%
        group_by(Contracts) %>%
        arrange(Date) %>%
        mutate(
            spreadvol = roll::roll_sd(SpreadReturn, 60) * sqrt(252),
            scaledreturns = 0.2 / lag(spreadvol) * SpreadReturn
        ) %>%
        na.omit() %>%
        group_by(Date) %>%
        mutate(returns = mean(scaledreturns)) %>%
        ungroup() %>%
        mutate(cumreturns = cumsum(scaledreturns)) %>%
        ggplot(aes(x=Date, y=cumreturns)) + geom_line() + ggtitle('VX spreads volatility adjusted traded together - cumulative returns')
    
    # Returns performance by slope strenght
    spreads %>% na.omit %>% 
        group_by(Contracts) %>%
        mutate(
            slope_zscore = (SpreadLog - roll::roll_mean(SpreadLog, 252))/roll::roll_sd(SpreadLog, 252),
        ) %>%
        mutate(lagbasis = ntile(lag(slope_zscore), 10)) %>%
        group_by(Contracts, lagbasis) %>%
        summarize(meanreturn = mean(SpreadReturn), sdreturn = sd(SpreadReturn)/sqrt(n())) %>%
        ggplot(aes(x=lagbasis, y=meanreturn)) + geom_bar(stat='identity') +
        geom_errorbar(aes(ymin=meanreturn-sdreturn,ymax=meanreturn+sdreturn), width=0.25)+ facet_wrap(~Contracts)
    
    # Split the previous by year for a sigle spread
    spreads %>%
        filter(Contracts == 'c_34') %>%
        mutate(year=year(Date)) %>%
        mutate(
            slope_zscore = (SpreadLog - roll::roll_mean(SpreadLog, 252))/roll::roll_sd(SpreadLog, 252),
        ) %>%
        mutate(lagbasis = ntile(lag(slope_zscore), 10)) %>%
        group_by(year, lagbasis) %>%
        summarize(meanreturn = mean(SpreadReturn)) %>%
        ggplot(aes(x=lagbasis, y=meanreturn)) + geom_bar(stat='identity') + facet_wrap(~year)
    
}



# Here I replicate RW "VIX_Futures_vs_SPX_Options_Basis_.ipynb" analysis using my own data to calculate the constant maturity contract
{
setwd("/home/marco/trading/Systems/Options/")
VI_future <- read_rds("/home/marco/trading/HistoricalData/Barchart/Futures.RDS")$VI
VI <- backadjust_future(VI_future) %>% select(Date, Constant, Constant_return, Return)
SPVIXSTR <- read_csv("Data/SPVIXSTR.csv", show_col_types = F) %>% mutate(Date=as.Date(Date, format="%m/%d/%Y")) %>% arrange(Date) %>% select(Date, Price) %>% rename(SPVIXSTR=Price)
VVIX <- read_csv("Data/VVIX.csv", show_col_types = F) %>% select(Date, `Adj Close`) %>% rename(VVIX=`Adj Close`)
VIX <- read_csv("Data/VIX.csv", show_col_types = F)  %>% select(Date, `Adj Close`) %>% rename(VIX=`Adj Close`)
VIX3M <- read_csv("Data/VIX3M.csv", show_col_types = F)  %>% select(Date, `Adj Close`) %>% rename(VIX3M=`Adj Close`)
SPX <- read_csv("Data/SPX.csv", show_col_types = F)  %>% select(Date, `Adj Close`) %>% rename(SPX=`Adj Close`)
SPY <- read_csv("Data/SPY.csv", show_col_types = F)  %>% select(Date, `Adj Close`) %>% rename(SPY=`Adj Close`)


full_df <- Reduce(function(...) full_join(..., by = "Date"), list(VI, VIX, VIX3M, VVIX, SPX, SPY)) %>% arrange(Date)
full_df <- full_df %>% mutate(
    premium = log(Constant / VIX3M),
    vx30_tr = Constant_return # or Return for realistic front-only contract trading
) %>% na.omit

premium <- full_df %>% 
    mutate(
        premium_mean = roll::roll_mean(premium, 252, complete_obs = T),
        premium_sd = roll::roll_sd(premium, 252, complete_obs = T),
        premium_zscore = (premium - premium_mean)/premium_sd
    ) 

premium %>%
    ggplot(aes(x=Date, y=premium_zscore)) + geom_line() +
    ggtitle('252 day zscore of Log(VX30/VIX3M)')

premium %>%
    mutate(
        lagpremium = lag(premium_zscore,1),
        decile = ntile(lagpremium, 10)
    ) %>%
    na.omit() %>%
    group_by(decile) %>%
    summarize(
        meanreturn = mean(vx30_tr) * 100,
        count = n(),
        minpremium = min(lagpremium),
        maxpremium = max(lagpremium)
    )  %>%
    ggplot(aes(x=decile, y = meanreturn)) + geom_bar(stat='identity') +
    ggtitle('VX30 next day total returns by decile of richness of VX30 vs VIX3M')

premium %>%
    mutate(lag_premium_zscore = lag(premium_zscore, 1)) %>%
    ggplot(aes(x=lag_premium_zscore, y=vx30_tr)) + geom_point() + geom_smooth()

final <- premium %>% 
    mutate(lag_premium_zscore = lag(premium_zscore, 1)) %>%
    mutate(tr30 = vx30_tr) %>% 
    mutate(Volatility = calculate_volatility(vx30_tr), Position = (0.2 / Volatility) %>% lag(1) %>% replace(.,is.na(.), 0) ) %>% 
    arrange(Date) %>% na.omit %>% 
    mutate(
        cutoff_1_0 = case_when(lag_premium_zscore <= -1  ~ tr30 , lag_premium_zscore > -1 ~ -tr30, TRUE ~ 0),
        cutoff_1_25 = case_when(lag_premium_zscore <= -1.25  ~ tr30, lag_premium_zscore > -1.25 ~ -tr30, TRUE ~ 0),
        cutoff_1_5 = case_when(lag_premium_zscore <= -1.5  ~ tr30, lag_premium_zscore > -1.5 ~ -tr30, TRUE ~ 0),
        cutoff_1_75 = case_when(lag_premium_zscore <= -1.75  ~ tr30 , lag_premium_zscore > -1.75 ~ -tr30, TRUE ~ 0),
        cutoff_2 = case_when(lag_premium_zscore <= -2  ~ tr30 , lag_premium_zscore > -2 ~ -tr30, TRUE ~ 0),
        cutoff_no = -tr30,
        cutoff_1_5_test = case_when(lag_premium_zscore <= -1.5  ~ 0, lag_premium_zscore > -1.5 ~ -tr30, TRUE ~ 0),
    ) %>%
    select(c(Date, starts_with('cutoff'))) %>%
    pivot_longer(-Date, names_to = 'zscore_cutoff', values_to = 'returns') %>% group_by(zscore_cutoff) %>% reframe(Date=Date, returns=returns, cumreturns=cumsum(returns)) 

final %>%  group_by(zscore_cutoff) %>% reframe(SR=mean(returns)/sd(returns)*16)

final %>%     ggplot(aes(x=Date, y=cumreturns, color = zscore_cutoff)) + geom_line(linewidth=2) + ggtitle('Long/Short - Be short when not long. 1d gap between signal and trade.') + scale_color_viridis_d()
}

# Here I replicate RW "2022 Review and Alternative Implementations.ipynb"
{
    df <- read_rds("/home/marco/trading/HistoricalData/Barchart/Futures.RDS")$VI
    ba <- backadjust_future(df)
    VI <- ba %>% select(Date, Constant, Constant_return)
    SPVIXSTR <- read_csv("Data/SPVIXSTR.csv", show_col_types = F) %>% mutate(Date=as.Date(Date, format="%m/%d/%Y")) %>% arrange(Date) %>% select(Date, Price) %>% rename(SPVIXSTR=Price)
    VVIX <- read_csv("Data/VVIX.csv", show_col_types = F) %>% select(Date, `Adj Close`) %>% rename(VVIX=`Adj Close`)
    VIX <- read_csv("Data/VIX.csv", show_col_types = F)  %>% select(Date, `Adj Close`) %>% rename(VIX=`Adj Close`)
    VIX3M <- read_csv("Data/VIX3M.csv", show_col_types = F)  %>% select(Date, `Adj Close`) %>% rename(VIX3M=`Adj Close`)
    SPX <- read_csv("Data/SPX.csv", show_col_types = F)  %>% select(Date, `Adj Close`) %>% rename(SPX=`Adj Close`)
    SPY <- read_csv("Data/SPY.csv", show_col_types = F)  %>% select(Date, `Adj Close`) %>% rename(SPY=`Adj Close`)
    
    full_df <- Reduce(function(...) full_join(..., by = "Date"), list(SPVIXSTR, VI, VIX, VIX3M, VVIX, SPX, SPY)) %>% arrange(Date)
    
    features <- full_df %>%
        mutate(
            basis_ratio = VIX3M / VIX,
            basis_ratio_bucket = ntile(basis_ratio, 10),
            VIX_bucket = ntile(VIX, 10),
            VVIX_bucket = ntile(VVIX, 10),
            SPVIXSTR_1df = log(lead(SPVIXSTR, 1) / SPVIXSTR),
            SPVIXSTR_2df = log(lead(SPVIXSTR,2) / lead(SPVIXSTR,1)),
            SPVIXSTR_10df = log(lead(SPVIXSTR,10) / lead(SPVIXSTR,1)),
            Constant_1df = lead(Constant_return, 1)
        )
    

    features %>%
        mutate(feature = basis_ratio_bucket) %>%
        select(
            Date,
            feature,
            SPVIXSTR_1df,
            SPVIXSTR_2df,
            SPVIXSTR_10df
        ) %>% 
        na.omit() %>%
        group_by(feature) %>%
        summarise(
            mean_return_1df = mean(SPVIXSTR_1df) / sd(SPVIXSTR_1df),
            mean_return_2df = mean(SPVIXSTR_2df) / sd(SPVIXSTR_2df),
            mean_return_10df = mean(SPVIXSTR_10df) / sd(SPVIXSTR_10df)
        )  %>%
        pivot_longer(-feature, names_to = 'period') %>%
        ggplot(aes(x=feature, y=value, color = period)) + 
        geom_line(linewidth=5) + 
        ggtitle('Mean SPVIXSTR log returns next day (red) and one day later (green) by basis decile')
    
    features <- features %>%
        mutate(
            vrp_score = (0.5 * basis_ratio_bucket) + (0.25 * VIX_bucket) + (0.25 * VVIX_bucket),
            vrp_score_floor = floor(vrp_score)
        ) 
    
    features %>%
        select(Date, vrp_score) %>%
        na.omit() %>%
        ggplot(aes(x=Date, y=vrp_score)) +
        geom_line() +
        ggtitle('Time Series of VRP score (high = short vol, low = long vol)')
    
    features %>%
        mutate(feature = vrp_score_floor) %>%
        select(
            Date,
            feature,
            SPVIXSTR_1df,
            SPVIXSTR_2df,
            Constant_1df
        ) %>% 
        na.omit() %>%
        group_by(feature) %>%
        summarise(
            mean_return_1df = mean(SPVIXSTR_1df),
            mean_return_2df = mean(SPVIXSTR_2df),
            mean_return__1df = mean(Constant_1df)
        ) %>%
        pivot_longer(-feature, names_to = 'period') %>%
        ggplot(aes(x=feature, y=value, color = period)) + 
        geom_line() + 
        ggtitle('Mean SPVIXSTR log returns next day (red) and 1d later (green) by vrp score')
    
    features %>%
        mutate(
            position_size = (vrp_score - 2) / 10,
            strat2df = position_size * -lead(Constant_1df, 1),
            short_only = -lead(Constant_1df, 1),
        ) %>%
        select(Date, vrp_score, position_size, strat2df, short_only) %>%
        na.omit() %>%
        mutate(
            cumret2d = cumsum(strat2df),
            cumret_short_only = cumsum(short_only),
        ) %>%
        # filter(date >= '2010-01-01') %>%
        # filter(date >= '2022-01-01') %>%
        ggplot(aes(x=Date, y=cumret2d, color=position_size)) + geom_point() + geom_line(aes(Date, cumret_short_only), color="red") 
        ggtitle('Simulation (cost-free) taking SPVIXSTR position in proportion to signal')
}


