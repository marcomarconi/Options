## WARNING: strike data downloaded from ORATS have some problems:
# - I missed 2021 from January to April
# - sometimes the columns "cOpra"and "pOpra" are missing. This is a problem if you want to stitch all the files together. 

{
    library(derivmkts)
    library(tidyverse)
    library(zoo)
    library(TTR)
    library(data.table)
    library(lubridate)
    library(Rfast)
    library(tsibble)
    library(ggthemes)
    library(cmdstanr)
    library(posterior)
    library(bayesplot)
    library(patchwork)
    library(MASS)
    library(moments)
    library(arrow)
    library(GGally)
    source("/home/marco/trading/Systems/Common/RiskManagement.R")
    source("/home/marco/trading/Systems/Common/Common.R")
    theme_set(theme_bw(base_size = 24))
}

# Some Kelly functions
{
    backtest_kelly <- function(returns, kelly_fraction, initial_capital) {
        capital <- initial_capital
        capital_progress <- numeric(length(returns))
        if(length(kelly_fraction)==1)
            kelly_fraction <- rep(kelly_fraction, length(returns))
        for (i in 1:length(returns)) {
            bet_size <- capital * kelly_fraction[i]
            capital <- capital + bet_size * returns[i]
            capital_progress[i] <- capital
        }
        
        return(capital_progress)
    }

    kelly_fraction <- function(mu, sigma, mom3=0) {
        l <- max(length(mu), length(sigma), length(mom3))
        if(length(mu)==1)
            mu <- rep(mu, l)
        if(length(sigma)==1)
            sigma <- rep(sigma, l)
        if(length(mom3)==1)
            mom3 <- rep(mom3, l)
        if(all(mom3 == 0)) {
            k_f <- mu / sigma^2
        } else {
            sign_f <- sign(mu)
            k_f <- (mu^2 + sigma^2 + sign_f*(sqrt((mu^2 + sigma^2)^2 - 4 * mom3 * mu))) / (2 * mom3)
        }
        return(k_f)
    }
    

}


## Creating the data set
{
Barchart_vols_ETFs  <- read_csv("/home/marco/trading/HistoricalData/Barchart/Options/Barchart_vols_ETFs.csv")
# This is for files in the format found on the FTP server
svmdata <- read_parquet("/home/marco/trading/HistoricalData/ORATS/Weekend.pq")
svmdata <- svmdata %>% rename(spotPrice = stkPx, tradeDate = trade_date) %>% mutate(tradeDate = as.Date(tradeDate, format="%m/%d/%Y"), expirDate = as.Date(expirDate, format="%m/%d/%Y"), dte = as.integer(expirDate - tradeDate + 1)) %>% filter(dte <= 8)
svmdata <- svmdata %>% rename(callAskPrice=cAskPx,callBidPrice=cBidPx,putAskPrice=pAskPx,putBidPrice=pBidPx)
final <- svmdata
# Plot spot prices
final %>% group_by(ticker, tradeDate) %>% reframe(spotPrice = first(spotPrice)) %>% ggplot(aes(tradeDate, log(spotPrice), color=ticker)) + geom_line(linewidth=2)
# Unique straddle id
ff_id <- mutate(final, id=paste(ticker, strike, expirDate, sep="_"), .after = ticker) %>% arrange(tradeDate)
# Get rounded delta, weekday name, and value of a straddle
Sys.setlocale("LC_TIME", "en_US.UTF-8")
ff_rd <- ff_id %>% mutate(delta_ = round_to_nearest(delta, 0.1), wd = lubridate::wday(tradeDate, label = TRUE), value=(callAskPrice+callBidPrice)/2+(putAskPrice+putBidPrice)/2, cost=(callAskPrice-callBidPrice)+(putAskPrice-putBidPrice), .after = ticker)
# 1 day profit of a straddle
ff_profit <- ff_rd %>% group_by(id) %>% mutate(profit = c(diff(value), 0), profit_pct = profit / spotPrice, .after = ticker)
# ATM straddles, merge duplicate straddles (sometime filtering for 0.5 we can very close straddles)
straddles <- ff_profit %>% filter(delta_ == 0.5) %>% filter(wd == "Fri") %>% group_by(ticker, tradeDate, expirDate) %>% reframe(across(everything(), first)) %>% ungroup
# Backtest, enter on friday exit one day on next week 
straddles %>% filter(ticker %in% c("SPY", "QQQ", "IWM")) %>% group_by(ticker, dte) %>% mutate(PnL = cumsum(-profit)) %>% ggplot(aes(tradeDate, PnL, color=ticker)) + geom_line(linewidth=2) + geom_point(color="black", size=0.2) + facet_wrap(~dte) #+ scale_color_colorblind()
straddles %>% filter(ticker %in% c("SPY", "QQQ", "IWM")) %>% group_by(ticker, dte) %>% mutate(PnL = cumsum(-profit_pct)) %>% ggplot(aes(tradeDate, PnL, color=ticker)) + geom_line(linewidth=2) + geom_point(color="black", size=0.2) + facet_wrap(~dte) #+ scale_color_colorblind()
straddles %>% filter(ticker %in% c("SPY", "QQQ", "IWM")) %>% ggplot(aes(x=-profit*100, fill=ticker)) + geom_boxplot(color="black", alpha=0.5) + theme(legend.position = "right") + xlab("Short Straddle Profit")
straddles %>% group_by(ticker, dte) %>% reframe(SR=-mean(profit_pct)/sd(profit_pct)*sqrt(52))
# Skewness
straddles %>% filter(ticker %in% c("SPY", "QQQ", "IWM") & dte == 4) %>% pull(profit) %>% skewness()
# Keep only 4-dte
straddles <- straddles %>% filter(dte == 4)
# Putting together the data
VVIX <- read_csv("Data/temp/VVIX.csv", show_col_types = F) %>% mutate(Time=as.Date(Time, format="%m/%d/%Y")) %>%  rename(tradeDate=Time, VVIX=Last) %>% arrange(tradeDate) %>% dplyr::select(tradeDate, VVIX)%>%  mutate(VVIXz = runZscore(log(VVIX), 252))
VIX <- read_csv("Data/temp/VIX.csv", show_col_types = F) %>% mutate(Time=as.Date(Time, format="%m/%d/%Y")) %>%  rename(tradeDate=Time, VIX=Last)%>% arrange(tradeDate) %>%  dplyr::select(tradeDate, VIX) %>%  mutate(VIXz = runZscore(log(VIX), 252))
Barchart_vols_selected <- Barchart_vols_ETFs %>% filter(Symbol %in% straddles$ticker) %>% rename(ticker = Symbol, tradeDate = Date) %>% 
                             group_by(ticker) %>% mutate(Momentum_5 = RSI2(AdjClose, 5, maType=EMA), Momentum_20 = RSI2(AdjClose, 20, maType=EMA ), Momentum_60 = RSI2(AdjClose, 60, maType=EMA ))
z <- inner_join(straddles, Barchart_vols_selected, by=c("ticker", "tradeDate")) %>% inner_join(., VVIX, by="tradeDate")  %>% inner_join(., VIX, by="tradeDate")
columns <- c("tradeDate", "ticker", "profit_pct", "Momentum_60", "IVRank", "PCVol", "VVIX")
weekend_data <- dplyr::select(z, all_of(c(columns))) %>% 
    mutate(profit_pct=profit_pct, Momentum_60=Momentum_60, VVIX=log(VVIX)-4.5, PCVol=log(PCVol), IVRank=(IVRank-50)/50)
# Expected Kelly fraction
r <- weekend_data  %>% pull(profit_pct)  %>% `*`(sqrt(52)) # Annualized weekly trades
kelly_1 <- mean(r)/sd(r)^2 # Typical Kelly
kelly_2 <- (mean(r)^2 + sd(r)^2 - sqrt((mean(r)^2 + sd(r)^2)^2 - 4 * moment(r, 3) * mean(r))) / (2 * moment(r, 3)) # Sinclair's Kelly
# Plot backtest, in log sum, and dollars with Kelly fraction. Give also sharpes
weekend_data %>% group_by(ticker) %>% mutate(PnL = cumsum(profit_pct*sqrt(52))) %>% ggplot(aes(tradeDate, PnL, color=ticker)) + geom_line(linewidth=2)
kelly_optimal <- weekend_data %>% group_by(ticker) %>% mutate(Dollars = backtest_kelly(profit_pct, kelly_1, 1000)) %>% ggplot(aes(tradeDate, log(Dollars), color=ticker)) + geom_line(linewidth=2) + ggtitle(paste("Kelly Optimal:", round(kelly_1,2)))
kelly_skew <- weekend_data %>% group_by(ticker) %>% mutate(Dollars = backtest_kelly(profit_pct, kelly_2, 1000)) %>% ggplot(aes(tradeDate, log(Dollars), color=ticker)) + geom_line(linewidth=2) + ggtitle(paste("Kelly Optimal:", round(kelly_2,2)))
kelly_ruin <- weekend_data %>% group_by(ticker) %>% mutate(Dollars = backtest_kelly(profit_pct, kelly_1*10, 1000)) %>% ggplot(aes(tradeDate, log(Dollars), color=ticker)) + geom_line(linewidth=2) + ggtitle(paste("Kelly Ruin:", round(kelly_1*2,2)))
kelly_optimal/kelly_skew/kelly_ruin;
data.frame(kelly_optimal=backtest_kelly(weekend_data$profit_pct, kelly_1, 1000) %>% log %>% diff %>% ts %>% SharpeRatio(),
           kelly_skew=backtest_kelly(weekend_data$profit_pct, kelly_2, 1000) %>% log %>% diff %>% ts %>% SharpeRatio()
           )
# General plottings weekend data
weekend_data %>% ungroup %>% ggplot(aes(x=tradeDate, y=cumsum(profit_pct))) + line(aes(color=ticker), size=2)
weekend_data %>% pivot_wider(id_cols = tradeDate, names_from = ticker, values_from = profit_pct) %>% dplyr::select(-1) %>% na.omit%>% ggpairs
weekend_data %>% group_by(ticker) %>% ggplot(aes(x=profit_pct)) + geom_density(aes(color=ticker), linewidth=2)
weekend_data %>% dplyr::select(-c(tradeDate,ticker)) %>% mutate(profit_pct_abs=abs(profit_pct)) %>% cor %>% corrplot::corrplot()
}

## Some for calendar spreads?
{
    weekend <- read_parquet("/home/marco/trading/HistoricalData/ORATS/Strikes/Weekend.pq") 
    weekend_0 <- weekend# %>% filter(ticker == "IWM")
    weekend_1 <- weekend_0 %>% mutate( id=paste(ticker, strike, expirDate, sep="_"), delta_ = round_to_nearest(delta, 0.1), wd = lubridate::wday(tradeDate), .after = ticker)  %>% arrange(tradeDate)
    weekend_2 <- weekend_1 %>% group_by(id) %>% mutate(cProfit = c(diff(cValue), 0), pProfit = c(diff(pValue), 0), cProfit_pct = cProfit / spotPrice, , pProfit_pct = pProfit / spotPrice, .after = ticker)
    weekend_3 <-  weekend_2 %>% filter(delta_ == 0.5 & wd == 6 & dte < 60)
    weekend_3 %>% group_by(dte, ticker) %>%  reframe(M=mean(pProfit_pct+cProfit_pct, na.rm=T), S=sd(pProfit_pct+cProfit_pct, na.rm=T)/sqrt(n()), N=n()) %>% filter(N>30) %>%  ggplot(aes(dte, ymin=M-S*2, ymax=M+S*2)) + geom_errorbar() + facet_wrap(~ticker)
}

### Model univariate
{
Weekend_uni_mod <- cmdstan_model("Weekend.stan")
fit_weekend <- Weekend_uni_mod$sample(data = list(y=weekend_data$profit_pct*100, N=nrow(weekend_data), M=3, J=3, k=rep(0,nrow(weekend_data)), X=dplyr::select(weekend_data, Momentum_60, IVz, VVIXz) ,family=1), chains = 4, parallel_chains = 4)
fit_1_s <- brm(bf(profit_pct ~ 1, sigma ~ 1 +  Momentum_60 + IVRank + VVIX), data=mutate(weekend_data, profit_pct=profit_pct*100), cores = 4, family = "skew_normal", control = list(adapt_delta=0.95), backend = "cmdstanr")
fit_1_s_nop <- brm(bf(profit_pct ~ 1, sigma ~ 1 + (1 | ticker) + Momentum_60 + IVRank + VVIX ), data=mutate(weekend_data, profit_pct=profit_pct*100), cores = 4, family = "skew_normal", control = list(adapt_delta=0.99), backend = "cmdstanr")
fit_weekend_brms <- brm(bf(profit_pct ~ 1, sigma ~ 1 + (1 + Momentum_60 + IVRank + VVIX + PCVol || ticker) ), data=mutate(weekend_data, profit_pct=profit_pct*100), cores = 4, family = "skew_normal", control = list(adapt_delta=0.95), backend = "cmdstanr")

# Plots and backtest
{
    fit <- fit_weekend
    y_hat <- fit$draws(variables = "y_hat") %>% merge_chains () %>% as.data.frame()
    y <- weekend_data$profit_pct*100
    pp_check(y, as.matrix(y_hat[1:50,]), ppc_dens_overlay)
    g <- weekend_data$ticker
    pp_check(y, as.matrix(y_hat[1:50,]), fun = "stat_grouped", group = g, stat = "mean")
    par(mfrow = c(2,2))
    plot(density(weekend_data %>% filter(ticker=="IWM") %>% pull(profit_pct) %>% `*`(100))); lines(density(unlist(y_hat[,1:116])), col="red");
    plot(density(weekend_data %>% filter(ticker=="QQQ") %>% pull(profit_pct) %>% `*`(100))); lines(density(unlist(y_hat[,117:232])), col="red");
    plot(density(weekend_data %>% filter(ticker=="SPY") %>% pull(profit_pct) %>% `*`(100))); lines(density(unlist(y_hat[,233:348])), col="red");
    y_adj <- weekend_data$profit_pct * sqrt(52)
    y_hat_adj <- y_hat / 100 * sqrt(52)
    pred_mean <- y_hat_adj %>% apply(., 2, mean) 
    pred_sd <- y_hat_adj %>% apply(., 2, sd) 
    pred_mom3 <- y_hat_adj %>% as.vector %>% moment(., 3) 
    kelly_0 <- -1 # Full Kelly
    kelly_1 <- kelly_fraction(mean(y_adj), sd(y_adj), moment(y_adj, 3)) # Typical Kelly
    kelly_2 <- kelly_fraction(pred_mean, pred_sd,  pred_mom3) # Model Kelly
    df <- data.frame(ticker=weekend_data$ticker, tradeDate=weekend_data$tradeDate, profit_pct=weekend_data$profit_pct, Kelly_0=kelly_0, Kelly_1=kelly_1, Kelly_2=kelly_2)
    kelly_optimal_0 <- df %>% filter(tradeDate > "2022-01-01") %>% group_by(ticker) %>% mutate(Dollars = backtest_kelly(profit_pct, Kelly_0, 1000)) %>% ggplot(aes(tradeDate, log(Dollars), color=ticker)) + geom_line(linewidth=2) + theme(axis.title = element_blank())+ scale_color_colorblind()
    kelly_optimal_1 <- df %>% filter(tradeDate > "2022-01-01") %>% group_by(ticker) %>% mutate(Dollars = backtest_kelly(profit_pct, Kelly_1, 1000)) %>% ggplot(aes(tradeDate, log(Dollars), color=ticker)) + geom_line(linewidth=2) + theme(axis.title = element_blank()) + scale_color_colorblind()
    kelly_optimal_2 <- df %>% filter(tradeDate > "2022-01-01") %>% group_by(ticker) %>% mutate(Dollars = backtest_kelly(profit_pct, Kelly_2, 1000)) %>% ggplot(aes(tradeDate, log(Dollars), color=ticker)) + geom_line(linewidth=2) + theme(axis.title = element_blank()) + scale_color_colorblind()
    kelly_optimal_0/kelly_optimal_1/kelly_optimal_2
    data.frame(kelly_0=backtest_kelly(weekend_data$profit_pct, kelly_0, 1000) %>% log %>% diff %>% ts %>% SharpeRatio(),
               kelly_1=backtest_kelly(weekend_data$profit_pct, kelly_1, 1000) %>% log %>% diff %>% ts %>% SharpeRatio(),
               kelly_2=backtest_kelly(weekend_data$profit_pct, kelly_2, 1000) %>% log %>% diff %>% ts %>% SharpeRatio())
    )
}
}
### Model multivariate
{
    Weekend_multi_2_mod <- cmdstan_model("Weekend_multi_2.stan")
    # Data and model fitting
    dates <- weekend_data %>% dplyr::select(tradeDate, ticker, profit_pct) %>% pivot_wider(id_cols = tradeDate, names_from = ticker, values_from = profit_pct) %>% na.omit %>% arrange(tradeDate) %>% pull(tradeDate) 
    Y <-  weekend_data %>% dplyr::select(tradeDate, ticker, profit_pct) %>% pivot_wider(id_cols = tradeDate, names_from = ticker, values_from = profit_pct) %>% arrange(tradeDate) %>% na.omit
    X1 <-   weekend_data %>% dplyr::select(tradeDate, ticker, Momentum_60) %>% pivot_wider(id_cols = tradeDate, names_from = ticker, values_from = Momentum_60) %>% arrange(tradeDate) %>% na.omit
    X2 <-   weekend_data %>% dplyr::select(tradeDate, ticker, IVz) %>% pivot_wider(id_cols = tradeDate, names_from = ticker, values_from = IVz) %>% arrange(tradeDate) %>% na.omit
    X3 <-   weekend_data %>% dplyr::select(tradeDate, ticker, VVIXz) %>% pivot_wider(id_cols = tradeDate, names_from = ticker, values_from = VVIXz) %>% arrange(tradeDate) %>% na.omit
    fit_weekend_multi_2 <- Weekend_multi_2_mod$sample(data = list(Y=Y[,-1]*100, X1=X1[,-1], X2=X2[,-1], X3=X3[,-1], N=nrow(Y), K=3, M=3), chains = 4, parallel_chains = 4, init=0)
    fit <- fit_weekend_multi_2
    # Plots
    mcmc_pairs(fit$draws(variables = c("xi_I", "omega_I", "xi_B", "omega_B"), format = "df") %>% merge_chains)
    # pp-check
    bayesplot_theme_set(theme_default(base_size = 8, base_family = "sans"))
    Y_hat <- fit$draws(variables = "Y_hat")  %>% merge_chains%>% as.data.frame() 
    par(mfrow = c(2,2))
    plot(density(unlist(Y[,2]*100))); lines(density(unlist(Y_hat[,1:65])), col="red");
    plot(density(unlist(Y[,3]*100))); lines(density(unlist(Y_hat[,66:130])), col="red");
    plot(density(unlist(Y[,4]*100))); lines(density(unlist(Y_hat[,131:195])), col="red");
    pairs(Y)
    # Backtest
    {
        weekend_data_reduced <- Y %>% pivot_longer(-tradeDate, names_to = "ticker", values_to = "profit_pct")  %>%  arrange(ticker)
        r <- weekend_data_reduced  %>% pull(profit_pct) %>% `*`(-1)  %>% `*`(sqrt(52))
        kelly_static <- mean(r)/sd(r)^2 
        weekend_data_reduced$KellyStatic <- kelly_static / 1
        weekend_data_reduced$Mean <- apply(-Y_hat / 100 * sqrt(52), 2, mean)
        weekend_data_reduced$SD <- apply(-Y_hat / 100 * sqrt(52), 2, sd)
        weekend_data_reduced$Skew <- apply(-Y_hat / 100 * sqrt(52), 2, skew)
        weekend_data_reduced$Kurt <- apply(-Y_hat / 100 * sqrt(52), 2, kurt)
        weekend_data_reduced <- mutate(weekend_data_reduced, KellyDynamic=(Mean/SD^2))
        with(weekend_data_reduced, matplot2(cbind(KellyStatic, KellyDynamic)))
        ks <- weekend_data_reduced %>% group_by(ticker) %>% mutate(Dollars = backtest_kelly(-profit_pct, KellyStatic, 1000)) %>% ggplot(aes(tradeDate, log(Dollars), color=ticker)) + geom_line(linewidth=2) + ggtitle("Kelly Static") + scale_color_colorblind()
        kd <- weekend_data_reduced %>% group_by(ticker) %>% mutate(Dollars = backtest_kelly(-profit_pct, KellyDynamic, 1000)) %>% ggplot(aes(tradeDate, log(Dollars), color=ticker)) + geom_line(linewidth=2) + ggtitle("Kelly Dynamic")+  scale_color_colorblind()
        ks/kd 
    }
}



# Straddle profit by DTE (in weeks) UNRELATED WITH WEEKEND
{
SPY <- read_csv("/home/marco/ORATS/strikes/SPY/_.csv")
# same as before
final <- SPY
ff <- mutate(final, id=paste(ticker, strike, expirDate, sep="_"), .after = ticker)
ff <- ff %>% mutate(delta_ = round_to_nearest(delta, 0.1), wd = lubridate::wday(tradeDate, label=TRUE), value=(callAskPrice+callBidPrice)/2+(putAskPrice+putBidPrice)/2, cost=(callAskPrice-callBidPrice)+(putAskPrice-putBidPrice), .after = ticker)
ff <- ff %>% group_by(id) %>% mutate(profit = c(diff(value), 0), profit_pct = profit / spotPrice, .after = ticker) %>% ungroup
# Calculate profit from entering a straddle (sum profit from a line till the end of a straddle id)
ff_ <- ff %>% group_by(id) %>% arrange(tradeDate) %>%    
    mutate(cumsum_from_date = rev(cumsum(rev(profit))), mean_from_date=rev(mean(rev(profit))) / seq_along(profit), sd_from_date = map_dbl(row_number(), ~ sd(value[.x:length(profit)])), .after = value) %>% ungroup
# Typical short-term straddle profit distribution
ff_ %>% filter(delta_==0.5 & between(dte, 2, 90)) %>% pull(cumsum_from_date) %>% hist(100) ; abline(v=0)
# Profit distribution by DTE (in weeks)
ff_ %>% filter(delta_==0.5 & between(dte, 2, 90)) %>% mutate(dte=round(dte/7))  %>%  ggplot(aes(x=cumsum_from_date)) + geom_density() + facet_wrap(~dte)  + geom_vline(xintercept = 0) 
# Sharpe by DTE (in weeks)
ff_ %>% filter(delta_==0.5 & between(dte, 2, 252)) %>% mutate(SR=-mean_from_date/sd_from_date*16) %>% group_by(dte=round(dte/7)) %>% reframe(M=median(SR, na.rm=T), S=mad(SR, na.rm=T)/sqrt(n()), n=n()) %>%  ggplot(aes(dte, ymin=M-S*2, ymax=M+S*2)) + geom_errorbar()
}
