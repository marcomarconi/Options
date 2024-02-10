library(tidyverse)
library(tsibble)
library(roll)
library(TTR)

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
    arrange(Date) %>%
    mutate(
        cutoff_1_0 = case_when(lag_premium_zscore <= -1  ~ tr30 , lag_premium_zscore > -1 ~ -tr30, TRUE ~ 0),
        cutoff_1_25 = case_when(lag_premium_zscore <= -1.25  ~ tr30, lag_premium_zscore > -1.25 ~ -tr30, TRUE ~ 0),
        cutoff_1_5 = case_when(lag_premium_zscore <= -1.5  ~ tr30, lag_premium_zscore > -1.5 ~ -tr30, TRUE ~ 0),
        cutoff_1_75 = case_when(lag_premium_zscore <= -1.75  ~ tr30 , lag_premium_zscore > -1.75 ~ -tr30, TRUE ~ 0),
        cutoff_2 = case_when(lag_premium_zscore <= -2  ~ tr30 , lag_premium_zscore > -2 ~ -tr30, TRUE ~ 0),
        cutoff_no = -tr30,
        cutoff_1_5_position = Position * case_when(lag_premium_zscore <= -1.5  ~ tr30, lag_premium_zscore > -1.5 ~ -tr30, TRUE ~ 0),
        test = Position * case_when(lag_premium_zscore <= -1.5  ~ 1, lag_premium_zscore > -1.5 ~ -1, TRUE ~ 0),
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


