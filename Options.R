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
    library(arrow)
    library(PerformanceAnalytics)
    library(bizdays)
    library(patchwork)
    source("/home/marco/trading/Systems/Common/RiskManagement.R")
    source("/home/marco/trading/Systems/Common/Common.R")
    source("/home/marco/trading/Systems/Common/Indicators.R")
    theme_set(theme_bw(base_size = 20))
}

# TODO:

# FILES:
# Price_data_stocks: yahoo price data with additional return and price volatilities, created with for f in `find . -name "*csv"`; do g=`basename $f | sed 's/.csv//'`;  cat $f | sed 's/^/'$g',/g'  ;done | grep -v Date > stock_price_data.csv
# ORATS_core: ORATS core, containing single day volatility and straddle data


# Convenience functions
{
get_decimal_places <- function(x) {
    # Convert the number to a string
    x_str <- as.character(x)
    
    # Split the string at the decimal point
    parts <- strsplit(x_str, "\\.")[[1]]
    
    # If there's no decimal point, there are no decimal places
    if (length(parts) == 1) {
        return(0)
    }
    
    # Otherwise, count the number of digits after the decimal point
    return(nchar(parts[2]))
}    
    
round_to_nearest <- function(x, n=0.05) {
        round(floor(x / n) * n, get_decimal_places(n))
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

# Get price for a given delta delta
get_delta_put <- function(S, tt, sigma, r, delta_target) {
    get_delta <- function(K) {
        d1 <- (log(S / K) + (r + 0.5 * sigma^2) * tt) / (sigma * sqrt(tt))
        return(pnorm(d1) - 1)
    }
    root <- optim(par = S, fn = function(K) (get_delta(K) - (-delta_target))^2, method="L-BFGS-B",lower = 0, upper = S)$par
    return(root)
}

# Black-Scholes delta for a put option
get_delta_call <- function(S, tt, sigma, r, delta_target) {
    get_delta <- function(K) {
        d1 <- (log(S / K) + (r + 0.5 * sigma^2) * tt) / (sigma * sqrt(tt))
        return(pnorm(d1))
    }
    root <- optim(par = S, fn = function(K) (get_delta(K) - (delta_target))^2, method="L-BFGS-B", lower = S, upper = S*2)$par
    return(root)
}

### Simulate options priceing and payoffs
option_sim_profit <- function(gbm, type="call", premium = NULL, X = 100, v=0.3, r = 0, d = 0, tt_start = 1, tt_end = NULL) {
    if(!is.matrix(gbm))
        stop(paste("gbm must be a matrix not "), class(gbm))
    if(!type %in% c("call", "put"))
        stop(paste("Option type can be either call or put, not"), type)
    periods <- nrow(gbm)
    if(is.null(tt_end)) {
        tt_end <- tt_start/periods
    } else if(tt_end >= tt_start) {
        stop(paste("tt_start must be bigger than tt_end"))
    }
    tte <- seq(tt_start, tt_end, length.out=periods)
    if(type=="call")
        values <- apply(gbm, 2, function(x) bscall(x, X, v, r, tte, d))
    else if(type=="put")
        values <- apply(gbm, 2, function(x) bsput(x, X, v, r, tte, d))
    if(is.null(premium))
        premium <- values[1,]
    price <- as.vector(tail(gbm, 1))
    value <- as.vector(tail(values, 1))
    if(type=="call")
        payoff <- apply(cbind(price-X, 0), 1, max)
    else if(type=="put")
        payoff <- apply(cbind(X-price, 0), 1, max)
    profit <- payoff - premium
    return(data.frame(price=price, value=value, payoff=payoff, profit=profit, premium=premium))
}


# Use gbm_vec to simulate a GBM
option_sim_values <- function(gbm, type="call", X = 100, tt = 1, v=0.3, r = 0, d = 0, hedging = FALSE) {
    if(!type %in% c("call", "put"))
        stop(paste("Option type can be either call or put, not"), type)
    periods <- nrow(gbm)
    tte <- seq(tt, tt/periods, length.out=periods)
    if(type=="call")
        values <- apply(gbm, 2, function(x) bscall(x, X, v, r, tte, d))
    else if(type=="put")
        values <- apply(gbm, 2, function(x) bsput(x, X, v, r, tte, d))
    return(values)
}

}


# ORATS core data loading, with straddle expected returns
{
    # function for loading an ORATS core data file and returning a subset of columns
    quiet_read_csv <- purrr::quietly((.f = read_csv))
    quiet_fread <- purrr::quietly((.f = fread))
    
    load_orats_day <- function(filename, cols_to_extract) {
        print(filename)
        # quiet_read_csv(glue::glue("/media/marco/Elements/ORATS/cores/{filename}")) #%>%
        quiet_fread(glue::glue("/media/marco/Elements/ORATS/cores/{filename}")) %>%
            purrr::pluck("result")  %>%   select(all_of(cols_to_extract)) %>% mutate(across(4:ncol(.), as.numeric)) # choose the right starting numeric column
    }
    
    cols_to_extract <- c('ticker', 'tradeDate', 'pxAtmIv', 'hiStrikeM1', 'hiStrikeM2',
                         "mktCap", "beta1y", "correlSpy1y", "correlEtf1y",
                         "straPxM1", "straPxM2", #"atmIvM1",	"atmIvM2",
                         "avgOptVolu20d", #"cVolu",  "cOi" , "pVolu",  "pOi", 
                         "dtExM1","dtExM2", 
                         "iv10d", "iv30d", "iv90d", "iv6m", "iv1yr", "volOfIvol", 
                         "orHv10d", "orHv20d", "clsHv20d", "clsHv60d", "clsHv120d", "clsHv252d",
                         "exErnIv30d", "orHvXern20d", # ex-Ern VRP
                         "ivHvXernRatio", "ivEtfRatio", "etfIvHvXernRatio", "ivPctile1y", 
                         "fbfexErn30_20", "fbfexErn60_30", "fbfexErn90_30",
                         "slope", "etfSlopeRatio", "contango", "deriv", "confidence", "borrow30"
                         )
    # Loads all ORATS core files, selecting interesting columns
    dir <- "/media/marco/Elements/ORATS/cores/"
    files <- c(list.files(dir, pattern = "orats_core_201[3-9].*gz"), list.files(dir, "orats_core_202[0-9].*gz"))
    ORATS_core <- files %>% purrr::map_df(.f = load_orats_day, cols_to_extract)
    # Reduce the size by removing short and low-liquidity tickers
    ORATS_core <- ORATS_core  %>% group_by(ticker) %>% filter(n() > 1000 & mean(avgOptVolu20d, na.rm=T) > 5) %>% ungroup()
    ### The following lines will calculate estimated straddle returns as abs(price - strike) - straddle
    # Calculate price returns, set negative prices to zero
    ORATS_core <- ORATS_core  %>% mutate(tradeDate=as.Date(tradeDate)) %>% mutate(pxAtmIv = case_when(pxAtmIv < 0 ~ 0, TRUE ~ pxAtmIv)) %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(retAtmIv = c(0, diff(log(pxAtmIv))), .after = pxAtmIv) %>% ungroup()
    # Remove returns > 0.1 as they are usually from stock splits (maybe set them to zero?). Also removes returns == 0? CHECK THIS
    ORATS_core <- ORATS_core  %>% mutate(retAtmIv = case_when(abs(retAtmIv) > 0.1 ~ 0, TRUE ~ retAtmIv)) # maybe retAtmIv == 0 ~ NA ?
    # Get next expiry date (ignore dtExM1 == 0 as they represent the day after expiration) and trading days to expiration (dte1 and dte2)
    nyse <- timeDate::holidayNYSE(2000:year(Sys.Date()) +1)
    create.calendar(name='NYSE', holidays=nyse, weekdays=c('saturday', 'sunday'))
    bizdays.options$set(default.calendar='NYSE')
    ORATS_core <- ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(
                                                             dte1 = case_when(dtExM1 > 0 ~ bizdays(tradeDate, tradeDate+dtExM1, "NYSE"), TRUE ~ 0), 
                                                             dte2 = case_when(dtExM2 > 0 ~ bizdays(tradeDate, tradeDate+dtExM2, "NYSE"), TRUE ~ 0), 
                                                             expiryDate1 = case_when(dtExM1 > 0 ~ tradeDate + dtExM1 - 1, TRUE ~ NA),
                                                             expiryDate2 = case_when(dtExM2 > 0 ~ tradeDate + dtExM2 - 1, TRUE ~ NA),
                                                             .after = tradeDate)
    # Get second straddle's estimated price at the expiration of the first straddle
    # ORATS_core <- ORATS_core %>% group_by(ticker, expiryDate1) %>% mutate(straPxM2_1 = straPxM2 * dplyr::last(iv30d) / iv30d * sqrt(dplyr::last(dtExM2) / dtExM2), .after = straPxM2) %>% ungroup()
    # Calculate cumulative price returns from current date to expiry 
    # ORATS_core <- ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(cumRetAtmIvM1 = map_dbl(1:n(), ~ sum(retAtmIv[(.x+1):(.x+dte1[.x])])), .after = retAtmIv)  %>% ungroup()
    # ORATS_core <- ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(cumRetAtmIvM2 = map_dbl(1:n(), ~ sum(retAtmIv[(.x+1):(.x+dte2[.x])])), .after = cumRetAtmIvM1)  %>% ungroup()
    # ORATS_core <- ORATS_core %>% mutate(cumRetAtmIvM1 = case_when(dte1 > 0 ~ cumRetAtmIvM1, TRUE ~ NA)) 
    # ORATS_core <- ORATS_core %>% mutate(cumRetAtmIvM2 = case_when(dte2 > 0 ~ cumRetAtmIvM2, TRUE ~ NA))
    # Obtain price on expiry. If expiry data is missing (usually because it lands on saturday), try the day before
    ORATS_core <- ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(pxAtmIvM1 = pxAtmIv[match(expiryDate1, tradeDate)], pxAtmIvM2 = pxAtmIv[match(expiryDate2, tradeDate)] ,.after = pxAtmIv) %>% ungroup
    ORATS_core <- ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(pxAtmIvM1 = case_when(is.na(pxAtmIvM1) ~ pxAtmIv[match(expiryDate1-1, tradeDate)], TRUE ~ pxAtmIvM1), pxAtmIvM2 = case_when(is.na(pxAtmIvM2) ~ pxAtmIv[match(expiryDate2-1, tradeDate)], TRUE ~ pxAtmIvM2),.after = pxAtmIv) %>% ungroup
    # ORATS_core <- ORATS_core %>% mutate(retAtmIvM1 = 1.0 - pxAtmIv/pxAtmIvM1, retAtmIvM2 = 1.0 - pxAtmIv/pxAtmIvM2, .after = pxAtmIvM2)    
    # Theoretical straddle prices
    # ORATS_core <- ORATS_core  %>% group_by(ticker) %>% mutate(
    #                                     straTheoM1 = 0.8 * pxAtmIv * mad(retAtmIv)*sqrt(252) * sqrt(dtExM1/365), 
    #                                     straTheoM2 = 0.8 * pxAtmIv * mad(retAtmIv)*sqrt(252) * sqrt(dtExM1/365),
    #                                     .after = retAtmIvM2) %>% ungroup
    # Calculate straddle returns, exclude straddle return when estimated daily returns during that period are > 10% (usually are stock splits) or straddle price > 10*stock price. Also, ignore 0DTEs. 
    ORATS_core <- ORATS_core %>% mutate(
                                        straProM1 = abs(pxAtmIvM1 - hiStrikeM1) - straPxM1, 
                                        straProM2 = abs(pxAtmIvM2 - hiStrikeM2) - straPxM2,
                                        straRetM1 = straProM1 / pxAtmIv, 
                                        straRetM2 = straProM2 / pxAtmIv, 
                                        # straLogM1 = log(straPxM1 / abs(pxAtmIvM1 - pxAtmIv + 1e-03)), 
                                        # straLogM2 = log(straPxM2 / abs(pxAtmIvM2 - pxAtmIv + 1e-03)),
                                        .after = straPxM2) %>% 
                                  mutate(
                                         straProM1 = case_when(abs(pxAtmIvM1 - hiStrikeM1)/pxAtmIvM1/sqrt(dte1+1) > 0.1 | straPxM1 > pxAtmIv*10 | straPxM1 == 0 | dtExM1 == 1 ~ NA, TRUE ~ straProM1),
                                         straProM2 = case_when(abs(pxAtmIvM2 - hiStrikeM2)/pxAtmIvM2/sqrt(dte2+1) > 0.1 | straPxM2 > pxAtmIv*10 | straPxM2 == 0 | dtExM2 == 1 ~ NA, TRUE ~ straProM2),
                                         straRetM1 = case_when(abs(pxAtmIvM1 - hiStrikeM1)/pxAtmIvM1/sqrt(dte1+1) > 0.1 | straPxM1 > pxAtmIv*10 | straPxM1 == 0 | dtExM1 == 1 ~ NA, TRUE ~ straRetM1),
                                         straRetM2 = case_when(abs(pxAtmIvM2 - hiStrikeM2)/pxAtmIvM2/sqrt(dte2+1) > 0.1 | straPxM2 > pxAtmIv*10 | straPxM2 == 0 | dtExM2 == 1 ~ NA, TRUE ~ straRetM2)#,
                                         #straLogM1 = case_when(abs(pxAtmIvM1 - hiStrikeM1)/pxAtmIvM1/sqrt(dte1+1) > 0.1 | straPxM1 > pxAtmIv*10 | straPxM1 == 0 ~ NA, TRUE ~ straLogM1),
                                         #straLogM2 = case_when(abs(pxAtmIvM2 - hiStrikeM2)/pxAtmIvM2/sqrt(dte2+1) > 0.1 | straPxM2 > pxAtmIv*10 | straPxM2 == 0 ~ NA, TRUE ~ straLogM2)
                                        )

    # Calculate expected straddle returns (ignore straddle whose prices are too high)
    # ORATS_core <- ORATS_core %>% mutate(straRetM1 = abs(cumRetAtmIvM1) - straPxM1 / pxAtmIv, 
    #                                     straRetM2 = abs(cumRetAtmIvM2) - straPxM2 / pxAtmIv, .after = cumRetAtmIvM2) %>% 
    #                                     mutate(straRetM1 = case_when(straPxM1 > pxAtmIv*10 ~ NA, TRUE ~ straRetM1), 
    #                                            straRetM2 = case_when(straPxM2 > pxAtmIv*10 ~ NA, TRUE ~ straRetM2))

    # Calculate VRP, replace infinites with NA
    ORATS_core <- ORATS_core %>% group_by(ticker) %>% 
        mutate(VRP = (log(iv30d / lead(orHv20d, 20))) %>% replace(is.infinite(.), NA),  
               VRPXern = (log(exErnIv30d / lead(orHvXern20d, 20))) %>% replace(is.infinite(.), NA),  
               .before = straProM1) %>% ungroup 
    # Misc stuff, IV percentile
    #ORATS_core <- ORATS_core %>% group_by(ticker) %>% mutate(IVpct = if(n() > 500) runPercentRank(iv30d, 252) else NA) %>% ungroup
    write_parquet(ORATS_core, "/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq")
}






# ORATS core data general observations 
{
    ORATS_core <- read_parquet("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq")
    # Simple backtest
    ORATS_core %>% filter(ticker=="AAPL") %>% group_by(expiryDate1) %>% reframe(M=mean(straRetM1, na.rm=T)) %>% mutate(PnL=cumsum(replace_na(M, 0))) %>% ggplot(aes(expiryDate1, PnL)) + geom_line() + geom_point()
    ## VRPs
    ORATS_core %>% mutate(VRP_30 = log(iv30d / lead(clsHv20d, 20)), VRP_90 = log(iv90d / lead(clsHv60d, 60)), VRP_180 = log(iv6m / lead(clsHv120d, 120)), VRP_365 = log(iv1yr / lead(clsHv252d, 252))) %>% 
        select(tradeDate, ticker, VRP_30, VRP_90, VRP_180, VRP_365) %>% pivot_longer(cols = c(VRP_30,  VRP_90,  VRP_180, VRP_365)) %>%  
        mutate(value = replace(value, is.infinite(value) | is.nan(value), NA)) %>% group_by(name, ticker) %>% reframe(M=mean(value, na.rm=T)) %>%  group_by(name) %>%  reframe(Mean=mean(M, na.rm=T), SD=sd(M, na.rm=T)/sqrt(n()), N=n())  %>% mutate(name = factor(name, levels=c("VRP_30", "VRP_90", "VRP_180", "VRP_365"))) %>% ggplot(aes(name, ymin=Mean-SD, ymax=Mean+SD)) + geom_errorbar(width = 0.5)
    ## Straddles return over tradeDate - all moments
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(mktCap, 8)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=mean(straRetM1 , na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(mktCap, 8)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=sd(straRetM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(mktCap, 8)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=skewness(straRetM1 %>% replace(.,is.infinite(.), NA), na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(mktCap, 8)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=kurtosis(straRetM1 %>% replace(.,is.infinite(.), NA), na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ## Straddles return over all data (or by ticker) summarized with density plots
    # market cap 
    ORATS_core %>% mutate(Decile = factor(ntile(mktCap, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # dte
    ORATS_core %>% filter(dtExM1<=30) %>% group_by(ticker, dte) %>% reframe(Value=mean(straRetM1/dtExM1 , na.rm=T))  %>% group_by(dte) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=dte, ymin=M-S, ymax=M+S)) + geom_errorbar() 
    # price
    ORATS_core %>% mutate(Decile = factor(ntile(pxAtmIv, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # normalized straddle price
    ORATS_core %>% mutate(Decile = factor(ntile(straNormM1, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # beta
    ORATS_core %>% mutate(Decile = factor(ntile(beta1y, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # iv30 - inter ticker and intra ticker 
    ORATS_core %>% mutate(Decile = factor(ntile(iv30d, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    ORATS_core %>% group_by(ticker) %>% mutate(Decile = factor(ntile(iv30d, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # clsHv20d - inter ticker and intra ticker (almost identical to above iv30)
    ORATS_core %>% group_by(ticker) %>% mutate(Decile = factor(ntile(clsHv20d, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # IVpct - intra ticker, it seems that VRP is better predicted than straddle return
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% mutate(Decile = factor(ntile(IVpct, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # volofIvol - inter ticker  only
    ORATS_core %>% mutate(Decile = factor(ntile(volOfIvol, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # iv30d correlation with SPX's
    ORATS_core %>% arrange(ticker, tradeDate) %>% mutate(rho=Rho_dynamic$rho) %>% group_by(tradeDate) %>% mutate(Decile = ntile(rho, 5)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=median(straRetM1 , na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    # contango - inter ticker and intra ticker 
    ORATS_core %>%  mutate(Decile = factor(ntile(contango, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    ORATS_core %>% group_by(ticker) %>% mutate(Decile = factor(ntile(contango, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # slope - inter ticker only
    ORATS_core %>% mutate(Decile = factor(ntile(slope, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # deriv - inter ticker only
    ORATS_core %>% mutate(Decile = factor(ntile(deriv, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # confidence - inter ticker  only
    ORATS_core %>% mutate(Decile = factor(ntile(confidence, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # Straddles return PACF autocorrelation by dte (probably it's just VRP making straddles correlated)
    ORATS_core %>% arrange( ticker, dte1, tradeDate) %>% group_by(dte1) %>% filter(n()>10000) %>% reframe(Acf = pacf(na.omit(straRetM1), plot = F, lag.max = 1)$acf[[1]], n()) %>% ggplot(aes(dtExM1, Acf)) + geom_point()
    # month day
    ORATS_core %>% mutate(Decile = mday(tradeDate)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(value=median(straRetM1 , na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(value, na.rm=T), S=sd(value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    # stock return (maybe in absolute terms? but it is basically vol) - inter ticker and intra ticker 
    ORATS_core %>%  mutate(Decile = factor(ntile(retAtmIv %>% replace_na(0), 5))) %>% na.omit %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    ORATS_core %>% group_by(ticker) %>%  mutate(Decile = factor(ntile(retAtmIv %>% replace_na(0)  , 5))) %>% na.omit %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    # stock price momentum - inter ticker and intra ticker ARRANGE IS IMPORTANT!!!
    ORATS_core %>% arrange(ticker, tradeDate) %>% group_by(ticker) %>% filter(n()>252)%>% mutate(rsi=pxAtmIv %>% na.locf(na.rm = F) %>% RSI2(., 60, maType=EMA)) %>% ungroup() %>% mutate(Decile=factor(ntile(rsi,5))) %>% na.omit %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0)+ xlim(c(-10,10))   + scale_color_colorblind() 
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% arrange(ticker, tradeDate) %>%  mutate(Decile = factor(ntile(pxAtmIv %>% na.locf(na.rm = F) %>% RSI2(., 60, maType=EMA), 5))) %>% na.omit %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1))
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% arrange(tradeDate) %>%  mutate(pred = retAtmIv %>% replace_na(0) %>%  {roll_meanr(.,252)/roll_sdr(.,252)} ) %>% ungroup %>% mutate(Decile = factor(ntile(pred, 5))) %>% ggplot(aes(x=straRetM1, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-1, 1)) # Slow!
    ORATS_core %>% arrange(ticker, tradeDate) %>% group_by(ticker) %>% filter(n()>252) %>% mutate(rsi=pxAtmIv %>% na.locf(na.rm = F) %>% RSI2(., 252, maType=EMA)) %>% mutate(Decile=factor(ntile(rsi,8))) %>% group_by(ticker, Decile) %>% reframe(Value=mean(straRetM1 , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # OI ratio - inter ticker
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(cOi/pOi, 5)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=mean(straRetM1 , na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ## Straddle returns by ticker
    # hurst index - pure value, no date ntile
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% reframe(H = HurstIndex(iv30d), Value = mean(straRetM1, na.rm=T)) %>% group_by(Decile = ntile(H, 8)) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Momentum 
    ORATS_core %>% arrange(ticker, tradeDate) %>% group_by(ticker) %>% filter(n()>252) %>% mutate(rsi=pxAtmIv %>% na.locf(na.rm = F) %>% RSI2(., 252, maType=EMA)) %>% mutate(Decile=factor(ntile(rsi,8))) %>% group_by(ticker, Decile) %>% reframe(Value=mean(straRetM1 , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Percentile IV, HV or IV-HV
    ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% filter(n()>252) %>% mutate(pred=runPercentRank(clsHv20d, 252)) %>% mutate(Decile=round(pred, 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(straRetM1 , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Confidence
    ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% filter(n()>252) %>% mutate(pred=confidence/100) %>% mutate(Decile=round(lag(pred), 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(VRP , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Contango, (iv6m-iv30d is very similar)
    ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% filter(n()>252) %>% mutate(pred=runPercentRank(contango, 252)) %>% mutate(Decile=round(lag(pred), 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(VRP , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Vol of Vol
    ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% filter(n()>252) %>% mutate(pred=runPercentRank(volOfIvol, 252)) %>% mutate(Decile=round(lag(pred), 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(VRP , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Avg Volume
    ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% filter(n()>500) %>% mutate(pred=runPercentRank(avgOptVolu20d, 252)) %>% mutate(Decile=round(lag(pred), 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(straRetM1 , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
}

# Plotting several observations
{
    # Cross sectional predictors, useful to select which stocks to trade
    dir <- "/home/marco/trading/Systems/Options/Plots/byDate/"
    setwd(dir)
    for(predictor in c("mktCap", "beta1y", "pxAtmIv", "confidence", "deriv", "slope", "contango", "avgOptVolu20d", "iv30d", "clsHv20d", "volOfIvol")) {
        print(predictor)
        df <- ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(!!sym(predictor), 8)) %>% group_by(ticker, Decile, Year=year(tradeDate))
        a <- df %>% reframe(Value=mean(straRetM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=mean(straLogM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straLogM1") + ggtitle("")
        d <- df %>% reframe(Value=mean(VRP, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("VRP") + ggtitle("")
        p <- a+b+d
        ggsave(paste0(predictor, ".png"), p, width = 12, height = 6)
    }
    # Single ticker predictors, useful to see how to trade one stock
    dir <- "/home/marco/trading/Systems/Options/Plots/byTicker/"
    setwd(dir)
    df <- ORATS_core %>% group_by(ticker) %>% arrange(tradeDate)  %>% collect %>% filter(n()>500) %>% ungroup
    for(predictor in c("pxAtmIv", "confidence", "deriv", "slope", "contango", "avgOptVolu20d", "iv30d", "clsHv20d", "log(iv30d/clsHv20d)", "volOfIvol")) {
        print(predictor)
        #df <- df %>% group_by(ticker) %>% mutate(pred=runPercentRank(na.locf(eval(parse_expr(predictor)), na.rm=F), 252)) %>% mutate(Decile=ntile(lag(EMA(pred)), 8))  %>% group_by(ticker, Decile, Year=year(tradeDate))
        df <- df %>% group_by(ticker) %>% mutate(pred=eval(parse_expr(predictor))) %>% mutate(Decile=ntile(lag(pred), 8))  %>% group_by(ticker, Decile, Year=year(tradeDate))
        a <- df %>% reframe(Value=mean(straRetM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=mean(straLogM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straLogM1") + ggtitle("")
        d <- df %>% reframe(Value=mean(VRP, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("VRP") + ggtitle("")
        p <- a+b+d
        ggsave(paste0(predictor, ".png"), p, width = 12, height = 6)
    }
    # Two-dimensional plotting
    ETFs <- ORATS_core %>% filter(ticker %in% etfs) %>% collect
    ETFs <- ETFs %>% group_by(ticker) %>% filter(n()>252) %>% arrange(tradeDate) %>% mutate(IVpct = EMA(runPercentRank(iv30d, 252), 10), HVpct = EMA(runPercentRank(clsHv20d, 252), 10), CTpct = EMA(runPercentRank(contango, 252), 10), VRPhist = lag(VRP, 21))
    ETFs %>% filter(tradeDate == "2025-03-03") %>% ggplot(aes(HVpct, CTpct)) + geom_label(aes(label = ticker))
}

# ORATS strikes bid-ask spreads (Not good, better use a list of tradable stock from somewhere else)
{
    dir <- "/media/marco/Elements/ORATS/smvstrikes/2024/"
    files <- list.files(dir) %>% tail(30)
    res <- list()
    for(f in files){
        res[[f]] <- read_csv(paste0(dir, f), show_col_types = F) %>% mutate(delta_ = round_to_nearest(delta)) %>% 
            filter(delta_ == 0.5) %>% group_by(ticker,trade_date) %>% filter(yte == min(yte)) %>% ungroup()
    }
    ORATS_bidask_spread <- res %>% do.call(rbind,.) %>% mutate(spread_price= ((cAskPx - cBidPx) + (pAskPx - pBidPx)) / 2 / stkPx * 100, spread_opt = ((cAskPx - cBidPx)/cAskPx + (pAskPx - pBidPx)/pAskPx) / 2 * 100) %>% 
                                   group_by(ticker) %>% reframe(spread_price_mean = mean(spread_price), spread_price_stderr = sd(spread_price)/sqrt(n()), 
                                                                spread_opt_mean = mean(spread_opt), spread_opt_stderr = sd(spread_opt)/sqrt(n()),N=n()) #%>% select(ticker, spread_mean, spread_stderr,N) 
    write_csv(ORATS_bidask_spread, "/home/marco/trading/HistoricalData/ORATS/ORATS_bidask_spreads.csv")
    # Get ticker that have resonable spread cost and that still exists
    ORATS_bidask_spread <- read_csv("/home/marco/trading/HistoricalData/ORATS/ORATS_bidask_spreads.csv")
    #last_month <- ORATS_core %>% mutate(YM=yearmonth(tradeDate)) %>% group_by(ticker) %>% arrange(tradeDate) %>% reframe(YM=last(YM))
    #ORATS_tradable <- ORATS_bidask_spread %>% inner_join(last_month, by="ticker") %>%  filter(YM == yearmonth("2024 Aug"), spread_price_mean+spread_price_stderr < 10 & spread_opt_mean+spread_opt_stderr < 50) %>% pull(ticker)
    #ORATS_tradable <- ORATS_bidask_spread %>% filter(spread_price_mean+spread_price_stderr < 10 & spread_opt_mean+spread_opt_stderr < 10) %>% pull(ticker)
    stock_screens <- read_csv("/home/marco/trading/Systems/Options/stocks-screener-03-18-2025.csv", show_col_types = F)
    etf_screens <- read_csv("/home/marco/trading/Systems/Options/etf-screener-04-02-2025.csv", show_col_types = F) 
    ORATS_tradable <- c(stock_screens %>% filter(`Options Vol` > 1000 & `Total OI`> 10000) %>% pull(Symbol), 
                        etf_screens %>% filter(`Options Vol` > 1000 & `Total OI`> 10000) %>% pull(Symbol))
} 

# Fundamentals
{
    # Load ORATS core data
    #ORATS_core <- read_csv("/home/marco/trading/HistoricalData/ORATS/ORATS_core.csv.gz")
    ORATS_core_ds <- open_dataset("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq") 
    # Load dolthub fundamentals
    dolthub_dir <- "/home/marco/trading/HistoricalData/Dolthub/"
    balance_sheet_assets <- read_csv(paste0(dolthub_dir, "post-no-preference_earnings_master_balance_sheet_assets.csv.gz"), show_col_types = F)
    balance_sheet_equity <- read_csv(paste0(dolthub_dir, "post-no-preference_earnings_master_balance_sheet_equity.csv.gz"), show_col_types = F)
    balance_sheet_liabilities <- read_csv(paste0(dolthub_dir, "post-no-preference_earnings_master_balance_sheet_liabilities.csv.gz"), show_col_types = F)
    income_statement <- read_csv(paste0(dolthub_dir, "post-no-preference_earnings_master_income_statement.csv.gz"), show_col_types = F)
    cash_flow_statement <- read_csv(paste0(dolthub_dir, "post-no-preference_earnings_master_cash_flow_statement.csv.gz"), show_col_types = F)
    eps_history <- read_csv(paste0(dolthub_dir, "post-no-preference_earnings_master_eps_history.csv.gz"), show_col_types = F) %>% mutate(period = "Quarter", EPS = reported) %>% rename(date = period_end_date)
    ohlcv <- read_csv(paste0(dolthub_dir, "post-no-preference_stocks_master_ohlcv.csv.gz"), show_col_types = F) %>% select(date, act_symbol, close )
    temp <- Reduce(function(...) full_join(..., by = c("act_symbol", "date", "period")), list(balance_sheet_assets, balance_sheet_equity, balance_sheet_liabilities, income_statement, cash_flow_statement, eps_history)) %>% arrange(date) %>% filter(period=="Quarter") %>% mutate(net_income = net_income.x) %>% select(-net_income.x, net_income.y)
    temp <- inner_join(temp, ohlcv, by = c("act_symbol", "date"))
    # Calculate some intesting derivative data not present in the dolthub
    temp <- temp %>% mutate(
                            ROA = net_income / total_assets, 
                            ROE = net_income / total_equity, 
                            DE = total_liabilities / total_equity,
                            )
    cols_to_select <- c( "beta1y", "mktCap", "ROA", "ROE", "DE", "EPS")
    fundamentals <- temp  %>% rename(tradeDate = date, ticker = act_symbol) %>% dplyr::select(ticker, tradeDate, period, any_of(cols_to_select))
    write_csv(fundamentals, "/home/marco/trading/HistoricalData/ORATS/Fundamentals.csv")
    # In order to merge with ORATS core WE LOSE SOME MATCH (fundamentals are only every 3 months, and sometimes the fundamentals are on weekends, so they don't match with ORATS trading days)
    # so you can fill the missing dates like this (done with chatgpt)
    fundamentals_filled <-  fundamentals %>% ungroup %>% mutate(tradeDate = as.Date(tradeDate)) %>% arrange(tradeDate) %>% group_by(ticker) %>% mutate(NextDate = lead(tradeDate)) %>%  rowwise() %>%
        mutate(FilledDates = list(seq(tradeDate, if_else(is.na(NextDate), tradeDate, NextDate - 1), by = "day"))) %>% # Generate sequence of dates
        unnest(FilledDates) %>% select(FilledDates, ticker, any_of(cols_to_select)) %>% rename(tradeDate = FilledDates) %>%  ungroup()
    # and finally only get the last friday of every month (It should match with some trading day in ORATS)
    fundamentals <- fundamentals_filled %>% filter(!lubridate::wday(tradeDate) %in% c(6, 7))  %>% group_by(M = yearmonth(tradeDate)) %>% filter(tradeDate == last(tradeDate))
    # Merge with ORATS
    ORATS_core_fundamentals <- inner_join(ORATS_core_ds, fundamentals, by=c("ticker", "tradeDate")) %>% arrange(ticker, tradeDate) %>% ungroup %>% collect
    # Get straddle returns, and remove infinite values
    ORATS_core_fundamentals <- ORATS_core_fundamentals %>% select(ticker, tradeDate, VRP, straRetM1, straRetM2, all_of(cols_to_select)) %>% mutate(across(c(VRP, all_of(cols_to_select)), ~ ifelse(is.infinite(.), NA, .))) 
    # Create binnings of the fundamentals data
    df_orats_fundamentals <- ORATS_core_fundamentals %>%  group_by(tradeDate) %>% mutate(across(all_of(cols_to_select), ~ntile(.,10))) %>%  ungroup
    # See which ones are interesting (market cap is usually the winner, I ignored current_ratio to simplify)
    df_orats_fundamentals %>% select(all_of(cols_to_select)) %>% cor(use = "pairwise.complete.obs") %>% corrplot::corrplot()
    write_csv(df_orats_fundamentals, "/home/marco/trading/HistoricalData/ORATS/Fundamentals_ORATS.csv")
    # Decile prediction
    df_orats_fundamentals %>% select(tradeDate, straRetM1, mktCap:EPS) %>% rename(Value=straRetM1) %>% pivot_longer(-c(tradeDate, Value)) %>% group_by(name, value) %>% reframe(M=mean(Value*100, na.rm=T), S=sd(Value*100, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=value, ymin=M-S, ymax=M+S)) + geom_errorbar()    + facet_wrap(~name)
    # Quick and dirty backtest by decile
    df_orats_fundamentals %>% mutate(pred=mktCap, profit = straRetM1)  %>% group_by(tradeDate, pred) %>% reframe(M=mean(profit, na.rm=T)) %>% na.omit %>% group_by(pred) %>% mutate(PnL=cumsum(M))  %>% ggplot(aes(tradeDate, PnL, color=factor(pred))) + geom_line(linewidth=2)
    # Backtesting strategy: short bottom decile, long top decile
    df_orats_fundamentals %>% mutate(pred = mktCap, signal = case_when(pred == 1 ~ -1, pred == 10 ~ 1, TRUE ~ 0)) %>% mutate(profit = straLogM1*(signal))  %>% group_by(tradeDate) %>% reframe(M=mean(profit, na.rm=T)) %>% pull(M) %>% ts %>% SharpeRatio()
}

# Ticker general performance  and FINAL list
{
    start_year <- 2021; 
    ORATS_core <- open_dataset("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq") %>% arrange(ticker, tradeDate)
    barchart_dir <- "/home/marco/trading/HistoricalData/Barchart/Options/"
    ETFs_Info <- read_csv(paste0(barchart_dir, "ETF_Info.csv"), show_col_types = FALSE) %>% select(Symbol, Name, Sector, Industry)
    Stocks_Info <- read_csv(paste0(barchart_dir, "Stock_Info.csv"), show_col_types = FALSE)%>% select(Symbol, Name, Sector, Industry)
    Info <- rbind(ETFs_Info, Stocks_Info) %>% rename(ticker=Symbol)
    df_orats_fundamentals <- read_csv("/home/marco/trading/HistoricalData/ORATS/Fundamentals_ORATS.csv")
    last_day_fundamentals <- df_orats_fundamentals %>%  group_by(ticker) %>% arrange(tradeDate) %>% slice_tail(n = 1) %>% 
        select(ticker, tradeDate, mktCap,   ROA ,  ROE  ,  DE  , EPS) %>% ungroup
    # We calculate average performance from 2021
    ORATS_core_reduced <- ORATS_core %>% filter(year(tradeDate) >= start_year)
    # Calculate mean straddle and VRP by month, and later average again
    tickers_performance <- ORATS_core_reduced  %>% filter(dte1 != 0 & dte2 != 0  & !is.na(VRP) & !is.na(straLogM1) & !is.na(straLogM2)) %>% collect %>% 
        group_by(ticker, ym=yearmonth(tradeDate)) %>% 
        reframe(s1=mean(straLogM1/dtExM1*100, na.rm=T), s2=mean(straLogM2/dtExM2*100, na.rm=T), vrp=mean(VRP, na.rm=T), pxAtmIv=last(pxAtmIv), beta1y=last(beta1y), tradeDate=last(tradeDate), n()) %>% 
        group_by(ticker) %>% reframe(Mstra1 = mean(s1), Sstra1 = sd(s1)/sqrt(n()), Mstra2 = mean(s2), Sstra2 = sd(s2)/sqrt(n()), Mvrp = mean(vrp), Svrp = sd(vrp)/sqrt(n()), pxAtmIv=last(pxAtmIv), beta1y=last(beta1y), tradeDate=last(tradeDate), N=n())    # Merge with last day fundamentals 
    # Merge with fundamentals
    tickers_selection <- full_join(tickers_performance %>% full_join(Info, by="ticker"), last_day_fundamentals %>% select(-tradeDate), by="ticker") 
    # Final criteria
    tickers_selection %>% filter(Mstra1 > 10 & Mvrp > 0.2 & tradeDate == "2025-01-16"  & ticker %in% ORATS_tradable & beta1y < 1) %>% View
}


# ORATS *api data* single stock playing with (SPY as example)
{
    strikes_f <- read_csv("/home/marco/ORATS/strikes/IWM/IWM_.csv")
    df_file <- strikes_f %>% mutate(delta_ = abs(round_to_nearest(delta, 0.1)), dist = 1/(abs(dte - 30)+1))
    df_w <-  df_file %>% group_by(tradeDate) %>%  filter(delta_==0.5) %>% group_by(tradeDate, expirDate) %>% reframe(M=mean(smvVol), dist=first(dist), vega=first(vega), value=mean(callValue+putValue))  %>%     group_by(tradeDate) %>% mutate(W = (dist / sum(dist))) 
    df_iv <- group_by(df_w, tradeDate) %>% reframe(IV = sum(M*W, na.rm=T), Slope = log(last(M)/first(M)), vega=first(vega), value=first(value))
    # Volatility smile
    strikes_f %>% mutate(IV=putMidIv, delta_ = round_to_nearest(1-delta, 0.1), dte_ = round(dte / 30)) %>% group_by(delta_, dte_) %>% reframe(M=median(IV, na.rm=T), S=mad(IV, na.rm=T)/sqrt(n()), N=n()) %>% ggplot(aes(delta_, y=M, ymin=M-S*2, ymax=M+S*2)) + geom_line(color="blue") + geom_errorbar(width=0.05) + facet_wrap(~dte_)
    # Straddles over weekend analysis (by IV)
    strikes_f <- read_csv("/home/marco/ORATS/strikes/IWM/IWM_.csv") 
    cores_f <- core_df %>% filter(ticker == "IWM")
    hv <- strikes_f %>% dplyr::select(tradeDate,spotPrice ) %>% group_by(tradeDate) %>% reframe(spotPrice=first(spotPrice)) %>% 
        mutate(ReturnPrice =log(spotPrice/lag(spotPrice)), HV = runSD(ReturnPrice, 7) * sqrt(252), RV = lead(HV, 7)) %>% dplyr::select(-spotPrice)    
    ff <- full_join(strikes_f, hv, by="tradeDate") %>% mutate(IV = (callMidIv+putMidIv)/2) %>% 
        mutate(VRP = IV - RV, VRPlog = log(IV / RV), wd = lubridate::wday(tradeDate, label = TRUE), delta_ = round_to_nearest(delta, 0.1))
    ff %>% filter(delta_==0.5 & dte == 1) %>% group_by(wd) %>% reframe(M=mean(VRPlog, na.rm=T), S=2*sd(VRPlog, na.rm=T)/sqrt(n()), N=n())
    # Straddles over weekend analysis (by price)
    strikes_f <- read_csv("/home/marco/ORATS/strikes/SLV/_.csv") 
    ff <- mutate(strikes_f, id=paste(strike, expirDate, sep="_"), .after = ticker)
    ff <- mutate(ff, delta_ = round_to_nearest(delta, 0.1), wd = lubridate::wday(tradeDate, label=TRUE), value=(callAskPrice+callBidPrice)/2+(putAskPrice+putBidPrice)/2, cost=(callAskPrice-callBidPrice)+(putAskPrice-putBidPrice), .after = ticker)
    ff <- ff %>% group_by(id) %>% mutate(profit = c(diff(value), 0), profit_pct = profit / spotPrice * 100, .after = ticker)
    ff %>% filter(delta_ == 0.5 & dte <= 7) %>% group_by(wd, dte) %>% reframe(M=mean(profit, na.rm=T), S=sd(profit, na.rm=T)/sqrt(n()), N=n()) %>% ggplot(aes(x=wd, ymin=M-S*2, ymax=M+S*2)) + geom_errorbar(width=0.5) + facet_wrap(~dte) + geom_hline(yintercept = 0)
    ff %>% filter(delta_ == 0.5 & dte == 5) %>% group_by(wd) %>% mutate(PnL = cumsum(-profit)) %>% ggplot(aes(tradeDate, PnL, color=wd)) + geom_line(linewidth=2) + geom_point(color="black")
    # Straddles over weekend single backtest
    ff <- filter(strikes_f, tradeDate >= "2020-01-01") %>% 
        select(ticker, tradeDate, strike, expirDate, callAskPrice, callBidPrice, putAskPrice,putBidPrice ,  delta, dte)
    ff <- mutate(ff, id=paste(strike, expirDate, sep="_"), .after = ticker)
    ff <- mutate(ff, value=(callAskPrice+callBidPrice)/2+(putAskPrice+putBidPrice)/2, cost=(callAskPrice-callBidPrice)+(putAskPrice-putBidPrice), .after = ticker)
    ff <- ff %>% group_by(id) %>% mutate(profit=c(0, diff(value)), .after = ticker)
    ff <- ff %>%  mutate(delta_ = round_to_nearest(delta, 0.1), wd = lubridate::wday(tradeDate, label=TRUE),  .after = ticker)
    straddles_we <- ff %>% mutate(profit_ = -1 * lead(profit) - cost, profit_0 = -1 * lead(profit), profit_p = -1 * log(lead(value) / value)) %>% 
        filter(dte <= 7 & delta_ == 0.50 & wd == "Fri") %>% group_by(tradeDate) %>% reframe(profit_=mean(profit_, na.rm=T), profit_0=mean(profit_0, na.rm=T), profit_p=mean(profit_p, na.rm=T))  %>% ungroup %>% na.omit %>% mutate(PnL = cumsum(profit_), PnL0 = cumsum(profit_0))         
    straddles_we %>% ggplot(aes(tradeDate, PnL)) + geom_line() + geom_point() + geom_line(aes(y=PnL0), color="purple") + geom_point(aes(y=PnL0), color="purple")
}

# ORATS *strikes data* straddles returns 
{
    # You might want to extract ticker data from historical zip files as 
    # for f in `find /media/marco/Elements/ORATS/smvstrikes/  -iname  "*zip"`; do echo $f; unzip -c $f | grep "SPY,\|^QQQ,\|^IWM,"  >> TEST.csv  ; done
    smvstrikes <- rbind(read_csv("/home/marco/trading/HistoricalData/ORATS/SPY_strikes//2020.csv"),
                        read_csv("/home/marco/trading/HistoricalData/ORATS/SPY_strikes/2021.csv"),
                        read_csv("/home/marco/trading/HistoricalData/ORATS/SPY_strikes/2022.csv"),
                        read_csv("/home/marco/trading/HistoricalData/ORATS/SPY_strikes/2023.csv"),
                        read_csv("/home/marco/trading/HistoricalData/ORATS/SPY_strikes/2024.csv"))
    # Load some ORATS strikes data
    smvstrikes <- read_parquet("/home/marco/trading/HistoricalData/ORATS/Strikes/AAPL.pq")
    smvstrikes <- read_parquet("/home/marco/trading/HistoricalData/ORATS/Strikes/TPB_IBM_SPY.pq")
    smvstrikes <- smvstrikes %>% rename(tradeDate = trade_date) %>% mutate(tradeDate = as.Date(tradeDate, format="%m/%d/%Y"), expirDate = as.Date(expirDate, format="%m/%d/%Y"), dte = as.integer(expirDate - tradeDate + 1))  %>% arrange(tradeDate)
    # Remove distant expiries
    smvstrikes <- smvstrikes %>% filter(dte <= 90)
    # Create and id for each put/call combo (same delta) and round the deltas
    smvstrikes <- mutate(smvstrikes, id=paste(ticker, strike, expirDate, sep="_"), delta_ = abs(round_to_nearest(delta, 0.1)), .after = ticker)
    # Calculate put/call combo price
    smvstrikes <- mutate(smvstrikes, value=(cAskPx+cBidPx)/2+(pAskPx+pBidPx)/2, cost=(cAskPx-cBidPx)+(pAskPx-pBidPx), .after = id)
    # Calculate put/call combo returns
    smvstrikes <- smvstrikes %>% group_by(id) %>% arrange(tradeDate) %>% 
        mutate(profit = c(diff(value), 0), profit_pct =  c(diff(value/stkPx), 0), profit_log = c(diff(log(value)), 0), 
               profit_cum = rev(cumsum(rev(profit))), profit_pct_cum = rev(cumsum(rev(profit_pct))), profit_log_cum = rev(cumsum(rev(profit_log))), .after = ticker)
    # Retrieve straddles (put/call combo with delta==0.5)
    smvstrikes_straddles <- smvstrikes %>% filter(delta_ == 0.5) %>% select(-c(profit, profit_pct, profit_log))
    smvstrikes_straddles %>% mutate(decile=round(dte/7))  %>%  ggplot(aes(x=profit_pct_cum)) + geom_density() + facet_wrap(~decile)  + geom_vline(xintercept = 0) + xlim(c(-1,1))
    smvstrikes_straddles %>% mutate(Decile=round(dte/7)) %>% group_by(Decile) %>%  reframe(M=mean(profit_pct_cum, na.rm=T), S=sd(profit_pct_cum, na.rm=T)/sqrt(n())) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() 
    # Draft attempt to calculate smile slope
    # smvstrikes_w <-  smvstrikes %>% mutate(dist = abs(dte - 30)+1) %>% group_by(tradeDate) %>%  filter(delta_==0.5 & dte <= 30) %>% arrange(tradeDate, expirDate) %>% group_by(tradeDate, expirDate) %>% reframe(M=mean(smoothSmvVol), dist=first(dist), dte=first(dte)) %>% group_by(tradeDate) %>% mutate(W = (dist / sum(dist))) 
    # smvstrikes_iv <- group_by(smvstrikes_w, tradeDate) %>% reframe(IV = sum(M*W, na.rm=T), Slope = log(last(M)/first(M))) %>% mutate(IVRank=ntile(IV, 10))
    # smvstrikes_straddles_final <- full_join(smvstrikes_straddles, smvstrikes_iv, by="tradeDate")
    # Single stock straddle analysis by dte
    smvstrikes_straddles %>% filter(ticker=="SPY") %>% group_by(dte=round(dte/7), tradeDate) %>% reframe(P=median(profit_cum/dte)) %>% group_by(dte) %>% mutate(PnL=cumsum(P)) %>% ggplot(aes(tradeDate, PnL)) + geom_line() + facet_wrap(~dte, scales = "free")
    # Positive Control: straddle over weekend
    smvstrikes_straddles %>% filter(ticker=="SPY") %>% filter(dte == 4 & wday(tradeDate)==6) %>% pull(profit_cum) %>% {.*-1} %>% cumsum %>% plot.ts
    # Backtest straddles on symbol and dte
    ids <- smvstrikes_straddles %>% filter(ticker=="AAPL" & dte == 25) %>% select(id, tradeDate, expirDate) %>% group_by(tradeDate, expirDate) %>% reframe(id=first(id)) %>% pull(id) 
    smvstrikes %>% filter(ticker=="AAPL") %>% filter(id %in% ids) %>% pull(profit_pct) %>% cumsum %>% plot.ts
    # Quick and dirty comparision with ORATS core
    {
        ticker_test <- "AAPL"; dte_test <- 30
        ids <- smvstrikes_straddles %>% filter(ticker==ticker_test & dte == dte_test) %>% select(id, tradeDate, expirDate) %>% group_by(tradeDate, expirDate) %>% reframe(id=first(id)) %>% pull(id)
        q <- smvstrikes_straddles %>% filter(ticker==ticker_test) %>% filter(id %in% ids & dte==dte_test)
        qq <- ORATS_core %>% filter(ticker==ticker_test & dtExM1 == dte_test)
        z <- merge(qq %>% select(tradeDate, cumRetAtmIv, straPxM1, straRetM1), q %>% select(tradeDate, value, profit_pct_cum), by="tradeDate") %>% na.omit
        matplot2(cbind(z$straRetM1 %>% cumsum, z$profit_pct_cum %>% cumsum))
    }
    
}


# Testing ticker wise volatility prediction
{
    ORATS_core_ds <- open_dataset("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq")
    ORATS_core <- ORATS_core_ds  %>%  collect() 
    ORATS_core <- ORATS_core  %>% mutate(vRatio30 = iv30d/clsHv20d, vRatio30 = case_when(vRatio30 <= 0 | is.infinite(vRatio30) ~ NA, TRUE ~ log(vRatio30))) %>% group_by(ticker)  %>%  mutate(vRatio30_signal = EMA(na.locf(vRatio30, na.rm = F)), vRatio30_signal = cap_forecast(vRatio30_signal, 2)) %>% ungroup
    ORATS_core <- ORATS_core %>% group_by(ticker)  %>% mutate(rsi = RSI2(pxAtmIv, 25, maType = EMA))  %>% ungroup
    ORATS_core %>% mutate(X=vRatio30_signal) %>% group_by(ticker, Decile=ntile(X, 8), Year=year(tradeDate)) %>% reframe(Value = mean(lag(X, 1) * straRetM1 / dtExM1, na.rm=T) * 100) %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)    
    
    etf_screens <- read_csv("/home/marco/trading/Systems/Options/etf-screener-04-02-2025.csv", show_col_types = F) 
    ETFs_weekly <- etf_screens %>% filter(`Wkly Opts` == "Yes") %>% pull(Symbol) 
    ORATS_core_ETFs <- ORATS_core_ds %>% filter(ticker %in% ETFs_weekly) %>% collect() %>% group_by(ticker) %>% filter(n()>500) %>% ungroup
    ORATS_core_ETFs <- ORATS_core_ETFs %>% group_by(ticker) %>% 
        mutate(
            vRatio30 = iv30d/clsHv20d, vRatio30 = case_when(vRatio30 <= 0 | is.infinite(vRatio30) ~ NA, TRUE ~ log(vRatio30)), vRatio30_signal = EMA(na.locf(vRatio30, na.rm = F)), vRatio30_signal = cap_forecast(vRatio30_signal, 2),
            ivPCT_signal = EMA(runPercentRank(iv30d, 252) * 2 - 1, 10),
            rsi25_signal = RSI2(pxAtmIv, 25, maType = EMA), rsi252_signal = RSI2(pxAtmIv, 252, maType = EMA)
            )
    ORATS_core %>% group_by(ticker) %>% mutate(X=log(clsHv20d)) %>% 
        mutate(Decile=ntile(X, 8)) %>% filter(dtExM1==8) %>% group_by(ticker, Decile) %>% reframe(Value = mean(straRetM1, na.rm=T) * 100) %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() 
    ORATS_core_ETFs %>% mutate(X=log(iv30d/clsHv20d)) %>% filter(dtExM1==8) %>% group_by(ticker, Decile=ntile(X, 8)) %>% reframe(Value = mean(straRetM1, na.rm=T) * 100) %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() 
    
    a <- ORATS_core_ETFs %>% group_by(ticker) %>% mutate(X = -(ivHvXernRatio-1), PnL = straRetM1 * X / runSD(straRetM1 %>% replace_na(0), 32)) %>% filter(dtExM1 == 8) %>% group_by(ticker) %>% mutate(cumPnL = cumsum(replace_na(PnL, 0)))
    ggplot(a, aes(tradeDate, cumPnL, color=ticker)) + geom_line() + geom_hline(yintercept = 0)
    group_by(a, tradeDate) %>% reframe(M=mean(PnL, na.rm=T)) %>% pull(M) %>% na.omit -> b
    mean(b)/sd(b)
    
    vrp_pred <- ORATS_core_ds %>% collect %>% arrange(tradeDate) %>% group_by(ticker) %>% mutate(
                            Volatility = calculate_volatility(retAtmIv),
                            VolatilityZ = scale(Volatility),
                            ivHvXernRatioZ = scale(ivHvXernRatio),
                            # ivPctile1yZ = scale(ivPctile1y),
                            # ivEtfRatioZ = scale(ivEtfRatio),
                            etfIvHvXernRatioZ = scale(etfIvHvXernRatio),
                            # mycontangoZ = scale(iv30d/iv90d),
                            contangoZ = scale(contango),
                            slopeZ = scale(slope),
                            derivZ = scale(deriv),
                            # borrowZ = scale(borrow30),
                            # confidenceZ = scale(confidence),
                            
                            # VolatilityZ = runZscore(Volatility, 252),
                            # ivHvXernRatioZ = runZscore(ivHvXernRatio, 252),
                            # #ivPctile1yZ = runZscore(ivPctile1y, 252),
                            # #ivEtfRatioZ = runZscore(ivEtfRatio, 252),
                            # etfIvHvXernRatioZ = runZscore(etfIvHvXernRatio, 252),
                            # #mycontangoZ = runZscore(iv30d/iv90d, 252),
                            # contangoZ = runZscore(contango, 252),
                            # slopeZ = runZscore(slope, 252),
                            # derivZ = runZscore(deriv, 252),
                            # #borrowZ = runZscore(borrow30, 252),
                            # #confidenceZ = runZscore(confidence, 252)
                            
                            ) %>% ungroup
                    
    vrp_pred %>% filter(mday(tradeDate) == 1) %>% dplyr::select(VRPXern, contains("Z")) %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot::corrplot()
    vrp_pred %>% filter(mday(tradeDate) == 1) %>% dplyr::select(VRPXern, contains("Z")) %>% lm(VRPXern ~ ., data=.) %>% summary

    
}


# Simulating option prices similar to ORATS
{
    # Black-Scholes delta function for a call
    call_delta <- function(S, K, r, tt, sigma) {
        d1 <- (log(S / K) + (r + 0.5 * sigma^2) * tt) / (sigma * sqrt(tt))
        return(pnorm(d1))  # Cumulative standard normal distribution
    }
    
    # Function to solve for strike price K given delta
    find_strike <- function(S, r, tt, sigma, target_delta) {
        uniroot(
            function(K) call_delta(S, K, r, tt, sigma) - target_delta,  # Solve for delta = target_delta
            lower = 1e-6,  # Avoid log(0) issues
            upper = S * 2  # Arbitrary upper limit
        )$root
    }
    
    {
    year_length <- 252
    years_num <- 2
    N <- year_length*years_num
    garch_vol <- FALSE
    target_vol <- 0.3
    if(!garch_vol) {
        price <- gbm_vec(1, mu=0, t = N, sigma = target_vol, dt = 1/year_length) %>% as.vector
        sigma <- rep(target_vol, N)
    } else {
        gbm_gv <- gbm_garch_vec(1, mu=0, t = N, target_vol = target_vol, alpha = 0.2, beta = 0.75, dt = 1/year_length)
        price <- gbm_gv$gbm %>% as.vector
        sigma <- gbm_gv$vol %>% as.vector
    }
    ticker <- "XYZ"
    rates <- 0
    vrp <- 0.1
    expiry_dates <- seq(21, N, 21)
    svm_sim <- matrix(NA, ncol=19, nrow=N*length(expiry_dates)*1)
    colnames(svm_sim) <- c("stkPx", "sigma", "expirDate", "dte", "yte", "strike", "cValue", "pValue",  "cIV", "pIV", "delta", "gamma", "vega", "rho", "theta", "tradeDate", "topStk", "bottomStk", "atmStk")
    count <- 1
    for(i in 1:N){
        print(i)
        tradeDate <- i
        stkPx <- price[i]
        bottomStk <- 1#round(find_strike(stkPx, r, 1, sigma, 0.95))
        topStk <- 100#round(find_strike(stkPx, r, 1, sigma, 0.05))
        atmStk <- round(stkPx)
        for(e in expiry_dates){
            if(e < i | (e - i) > year_length) next
            expirDate <- e
            dte <- (e - i)
            yte <- dte / year_length
            # for strikes....
            {
                strike <- atmStk
                res <- bsopt(stkPx, strike, sigma[i], rates, ifelse(yte == 0, 0.001, yte), 0)
                cValue <- exp(log(res$Call["Premium",]) + vrp * log(strike) + 0*log(sigma[i]) )
                pValue <- exp(log(res$Put["Premium",])  + vrp * log(strike) + 0*log(sigma[i]) )
                cIV <- bscallimpvol(stkPx, strike, rates, ifelse(yte == 0, 0.001, yte), 0, cValue)
                pIV <- bsputimpvol(stkPx, strike, rates, ifelse(yte == 0, 0.001, yte), 0, pValue)
                svm_sim[count,] <- c(stkPx, sigma[i], expirDate, dte, yte, strike, cValue, pValue, cIV, pIV, res$Call[2:6], tradeDate, topStk, bottomStk, atmStk)
                count <- count + 1
            }
        }
    }
    {
    # Replicate the data in ORATS core (see the section "ORATS core data loading")
    svm_sim_core <- svm_sim %>% as.data.frame() %>% mutate(ticker = ticker, Value = cValue+pValue, .before=cValue) %>% group_by(tradeDate) %>% mutate(closest = min(dte)) %>% filter(dte == closest)
    svm_sim_core <- svm_sim_core  %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(return = c(0, diff(log(stkPx))), .after = stkPx)
    svm_sim_core <- svm_sim_core %>% group_by(ticker, expirDate) %>% arrange(tradeDate) %>% mutate(cumRet = map_dbl(1:n(), ~ sum(return[(.x+1):n()])), .after = return) %>% ungroup 
    #svm_sim_core <- svm_sim_core  %>% group_by(ticker) %>% mutate(cumRet = map_dbl(1:n(), ~ sum(return[(.x+1):min(.x + dte[.x], n())])), cumRet = case_when(dte == 0 ~ NA, TRUE ~ cumRet), .after = return)
    svm_sim_core <- svm_sim_core %>% mutate(straRet = abs(cumRet) - Value / stkPx ) 
    }
    # {
    # # Check no bias in straddle price
    # svm_sim_core$straRet  %>% plot.ts
    # # No edge here! Known predictors show no predictive power
    # svm_sim_core %>% mutate(Decile = factor(ntile(stkPx, 5))) %>% ggplot(aes(x=straRet/dte, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-0.025,0.025))
    # svm_sim_core %>% mutate(clsHv20d = runSD(return, 20)) %>% mutate(Decile = factor(ntile(clsHv20d, 5))) %>% ggplot(aes(x=straRet/(dte+1), color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-0.02,0.02))
    # }
    {
    svm_sim_core$straRet %>% na.omit %>% density %>% plot
    (aapl$straRetM1) %>% na.omit %>% density %>% lines(col="red")
    (tpb$straRetM1) %>% na.omit %>% density %>% lines(col="blue")
    }
}
}



# Simulating options outcomes
{
    # Put spread
    {
        S0 <- 100
        d1 <- 0.3
        d2 <- 0.1
        days <- 30
        iv1 <- 0.40
        iv2 <- 0.45
        rv <- 0.35
        r <- 0.045
        X1 <- get_delta_put(S0, days/365, sigma = iv1, r = r, delta_target = d1)
        X2 <- get_delta_put(S0, days/365, sigma = iv2, r = r, delta_target = d2)
        gbm <- gbm_vec(10000, t = days, S0 = S0, dt = 1/252, sigma = rv)
        put1 <- option_sim_profit(gbm, type = "put", X = X1, tt_start = days/365, tt_end = 1/365, v = iv1); 
        put2 <- option_sim_profit(gbm, type = "put", X = X2, tt_start = days/365, tt_end = 1/365, v = iv2); 
        profit <- (-put1$profit -put1$profit   + -put2$profit -put2$profit ) * 100
        profit %>% hist
        profit %>% summary
    }
    # Calendars
    {
        S0 <- 35.51
        X <- 35
        front_days <- 24
        back_days <- 53
        front_IV <- 0.415
        back_IV <- 0.30
        sigma <- 0.2
        gbm <- gbm_vec(10000, front_days, S0 = S0, dt = 1/252, sigma = sigma)
        front_put <- option_sim_profit(gbm, type = "put", X = X, tt_start = front_days/365, tt_end = 1/365, v = front_IV); 
        front_call <- option_sim_profit(gbm, type = "call", X = X,tt_start = front_days/365, tt_end = 1/365, v = front_IV); 
        back_put <- option_sim_profit(gbm, type = "put", X = X, tt_start = back_days/365, tt_end = (back_days-front_days)/365, v = back_IV); 
        back_call <- option_sim_profit(gbm, type = "call", X = X, tt_start = back_days/365, tt_end = (back_days-front_days)/365, v = back_IV); 
        profit <- (-front_put$profit -front_call$profit   + back_put$value - back_put$premium + back_call$value - back_call$premium) * 100
        profit %>% hist
    }
    # Predicting alpha strategy: https://predictingalpha.com/profitable-option-selling-strategy/ 
    {
        S0 <- 100
        X <- 100
        r <- 0
        iv_f <- 0.30
        iv_b <- 0.22
        rv <- 0.2
        front_days <- 7 # Our strangle
        back_days <- 90 # The hedge
        K1_f <- get_delta_put(S0, front_days/365, sigma = iv_f, r = r, delta_target = 0.2)
        K2_f <- get_delta_call(S0, front_days/365, sigma = iv_f, r = r, delta_target = 0.2)
        K1_b <- get_delta_put(S0, back_days/365, sigma = iv_b, r = r, delta_target = 0.2)
        K2_b <- get_delta_call(S0, back_days/365, sigma = iv_b, r = r, delta_target = 0.2)
        print(c(K1_f, K2_f, K1_b, K2_b))
        gbm <- gbm_vec(10000, front_days, S0 = S0, dt = 1/252, sigma = rv)
        front_put <- option_sim_profit(gbm, type = "put", X = K1_f, tt_start = front_days/365, tt_end = 1/365, v = iv_f); 
        front_call <- option_sim_profit(gbm, type = "call", X = K2_f, tt_start = front_days/365, tt_end = 1/365, v = iv_f);
        back_put <- option_sim_profit(gbm, type = "put", X = K1_b, tt_start = back_days/365, tt_end = (back_days-front_days*4)/365, v = iv_b); 
        back_call <- option_sim_profit(gbm, type = "call", X = K2_b, tt_start = back_days/365, tt_end = (back_days-front_days*4)/365, v = iv_b); 
        front_profit <- 4*(-front_put$profit -front_call$profit) * 100
        back_profit <- (back_put$value - back_put$premium + back_call$value - back_call$premium) * 100
        profit <- front_profit+back_profit
        profit %>% summary
    }
}


# Simulate some strategies like E.Sinclair in Positional Options Trading
{
    
    ## Good Straddle  
    gbm <- gbm_vec(100, sigma = 0.2)
    option_sim_profit(gbm, type="call") -> a
    option_sim_profit(gbm, type="put") -> b
    profit <- (-a$profit-b$profit)*100
    hist(profit, 50); summary(profit)
    ## Good Strangle
    gbm <- gbm_vec(10000)
    option_sim_profit(gbm, type="put", X = 70) -> a
    option_sim_profit(gbm, type="call", X = 130) -> b
    profit <- (-a$profit-b$profit)*100*1.68
    hist(profit, 50)
    ## Bad Straddle Vol  
    gbm <- gbm_vec(10000, sigma = 0.7)
    option_sim_profit(gbm, type="call") -> a
    option_sim_profit(gbm, type="put") -> b
    profit <- (-a$profit-b$profit)*100
    hist(profit, 50)
    ## Bad Strangle Vol
    gbm <- gbm_vec(10000, sigma = 0.7)
    option_sim_profit(gbm, type="put", X = 70) -> a
    option_sim_profit(gbm, type="call", X = 130) -> b
    profit <- (-a$profit-b$profit)*100*1.68
    hist(profit, 50)
    ## Bad Straddle Drift  
    gbm <- gbm_vec(10000, mu = 0.2)
    option_sim_profit(gbm, type="call") -> a
    option_sim_profit(gbm, type="put") -> b
    profit_straddle <- (-a$profit-b$profit)*100
    ## Bad Strangle Drift
    gbm <- gbm_vec(10000, mu = 0.2)
    option_sim_profit(gbm, type="put", X = 70) -> a
    option_sim_profit(gbm, type="call", X = 130) -> b
    profit_strangle <-(-a$profit-b$profit)*100*1.68
    plot(density(profit_straddle), xlim=c(-20000, 3000), lwd=2); lines(density(profit_strangle), col="blue", lwd=2)
    ## Covered Call
    gbm <- gbm_vec(10000, mu = 0.1, sigma = 0.15, t = 252, dt=1./252)
    call <- option_sim_profit(gbm, type="call", v=0.2)
    stock_profit <- as.vector(tail(gbm, 1))
    call_profit <- call$profit
    covered_call_profit <- stock_profit - call_profit
    hist(covered_call_profit) # terminal profit
    summary(covered_call_profit/100-1)
    ## Calendat Straddle
    gbm <- gbm_vec(10000, 30, dt = 1/252, sigma = 0.3)
    front_iv <- 0.3
    back_iv <- 0.3
    front_call <- option_sim_profit(gbm, type = "call", tt_start = 30/365, tt_end = 1/365, v = front_iv); 
    front_put <- option_sim_profit(gbm, type = "put", tt_start = 30/365, tt_end = 1/365, v = front_iv); 
    back_call <- option_sim_profit(gbm, type = "call", tt_start = 60/365, tt_end = 31/365, v = back_iv); 
    back_put <- option_sim_profit(gbm, type = "put", tt_start = 60/365, tt_end = 31/365, v = back_iv); 
    front_straddle <- (front_call$payoff + front_put$payoff) 
    back_straddle <- (back_call$value + back_put$value)
    # Figure 6.14
    plot(front_call$price, -front_straddle + back_straddle)
    # Profit at the expiration of the front option (Table 6.14)
    front_profit <- -(front_call$profit + front_put$profit)
    back_profit <- (back_call$value + back_put$value) - (back_call$premium + back_put$premium)
    profit <- (front_profit  + back_profit) * 100
    hist(profit, 50); summary(profit)
    # calendar spread path (not just final profit as before), it is the same...
    gbm <- gbm_vec(1, 30, dt = 1/252, sigma = 0.3) %>% as.vector
    call_path_short <- bscall(gbm, 100, 0.3, 0, seq(30/365, 1/365, length.out=30), 0)
    call_path_long <- bscall(gbm, 100, 0.3, 0, seq(60/365, 31/365, length.out=30), 0)
    call_spread <- call_path_long - call_path_short 
    put_path_short <- bsput(gbm, 100, 0.3, 0, seq(30/365, 1/365, length.out=30), 0)
    put_path_long <- bsput(gbm, 100, 0.3, 0, seq(60/365, 31/365, length.out=30), 0)
    put_spread <- put_path_long - put_path_short365
    straddle_path <- -call_path_short-put_path_short + call_path_long+put_path_long
    matplot2(cbind(call_spread, put_spread, straddle_path/2))
}

# Clustering
{
    # Returns, the hard way
    library(dbscan)
    library(Rtsne)
    PC <- 10 # The number of PCA factor to use, keep it low to avoid overfitting
    EPS <- 1 # Play with this to get the number of pairs you want  
    good_symbols -> ... # all non NA containing symbols
    Returns <- stocks %>% filter(Symbol %in% good_symbols & between(Date, "2023-01-01", "2024-01-01")) %>% select(Date, Symbol, ReturnPrice) %>% pivot_wider(names_from = Symbol, values_from = ReturnPrice) %>% arrange(Date) 
    pca <- prcomp( Returns[,-1] %>%  t, center = TRUE, scale. = TRUE) # run PCA
    X <- pca$x[,1:PC]; # Extract PCA factors
    rtsne <- Rtsne(X, check_duplicates = TRUE)$Y # Run t-SNE, some people say it is bad and you should only use PCA factors, I don't know
    optic_res <- optics(rtsne, minPts = 2) # Run optics, this is the pre-clustering
    Clusters <- extractDBSCAN(optic_res, eps_cl = 1) # Run DBscan clustering
    hullplot(rtsne, Clusters) # visualize clusters
    clusters <- data.frame(Symbol = good_symbols[-1], Cluster = Clusters$cluster)
    # Returns,the easy way
    a <- lapply(Price_data_stocks, function(df) select(df, Date, ReturnPrice))
    names(a) <- names(Price_data_stocks)
    b <- Reduce(function(...) full_join(..., by = "Date"), a) %>% arrange(Date)
    z <- cor(b[,-1], use="pairwise.complete.obs")
    rownames(z) <- names(Price_data_stocks)
    colnames(z) <- names(Price_data_stocks)
    z[is.na(z)] <- 0 # careful
    d <- as.dist(1 - z)
    tree <- hclust(d, method="complete")
    dend <- as.dendrogram(tree)
    clusters <- cutree(dend, k=50)
    # IV returns from ORATS
    df <- ORATS_core %>% select(ticker, tradeDate, iv30d) %>% arrange(ticker, tradeDate)  %>% 
        filter(year(tradeDate)>=2020) %>% na.omit %>% group_by(ticker) %>% 
        arrange(tradeDate) %>%  mutate(ivRet = c(0, diff(log(iv30d)))) %>% ungroup %>% select(-iv30d)
    df_wide <- df %>% pivot_wider(id_cols = tradeDate, names_from = ticker, values_from = ivRet)
    df_matrix <- df_wide[,-1] %>% as.matrix; df_matrix[is.infinite(df_matrix)] <- 0
    ORATS_IV_cor_matrix <- cor(df_matrix, use="pairwise.complete.obs")
    # ...
}

# MISC
{
    sigma_pred <- function(s, h, omega, alpha, beta){
        if(is.na(h)) return(NA)
        z <- rep(NA, (h+1))
        z[1] <- s
        for(i in 2:(h+1))
            z[i] <- sqrt(omega  + (alpha + beta) * z[i-1]^2)
        return(z)
    }
    
    # Calculate h-step-ahead volatility in one shot
    h_step_ahead_volatility <- function(sigma, h, omega, alpha, beta) {
        phi <- alpha + beta
        return(sqrt(omega * (1 - phi^h) / (1 - phi)) + (phi^h * sigma^2))
    }
    
    h_step_ahead_volatility2 <- function(sigma, h, omega, alpha, beta) {
        long_run_variance <- omega / (1 - alpha - beta)
        phi <- alpha + beta
        return(sqrt(long_run_variance + phi^(h) * (sigma^2 - long_run_variance)))
    }
    
    h_step_ahead_volatility3 <- function(sigma, h, omega, alpha, beta) {
        long_run_variance <- omega / (1 - alpha - beta)
        phi <- alpha + beta
        return(sqrt(long_run_variance * h + (sigma^2 - long_run_variance) * phi * (1 - phi^h) / (1 - phi)))
    }
    
    ret_pred <- function(df) {
        df <- ungroup(df)
        df <- df %>% mutate(sigma = calculate_volatility(retAtmIv), .after = ticker)
        df <- df %>% mutate(sigma_0 = runSD(retAtmIv, 252) * sqrt(252), .after = ticker)
        df <- df %>% mutate(retPred_0 = sigma_0 / sqrt(252) * sqrt(dte), .after = ticker)
        df <- df %>% mutate(retPred_1 = sigma / sqrt(252) * sqrt(dte), .after = ticker)
        df <- df %>% mutate(retPred_2 = h_step_ahead_volatility_closed(sigma, dte, sigma_0^2 * (1-0.05-0.9), 0.025, 0.9) / sqrt(252) * sqrt(dte), .after = ticker)
        df <- df %>% mutate(retPred_3 = sqrt(abs(retAtmIv)^2 * sqrt(dte)), .after = ticker) %>% ungroup
        return(df)
    }
    
    ibm <-     ret_pred(ibm)
    ibm %>% mutate(W = 1, P=straRetM1/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
    ibm %>% mutate(W = (retPred_0 - straPxM1 / pxAtmIv) , W=sign(W),P=straRetM1/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
    ibm %>% mutate(W = (retPred_1 - straPxM1 / pxAtmIv) , W=sign(W),P=straRetM1/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
    ibm %>% mutate(W = (retPred_2 - straPxM1 / pxAtmIv) , W=sign(W),P=straRetM1/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
    ibm %>% mutate(W = (retPred_3 - straPxM1 / pxAtmIv) , W=sign(W),P=straRetM1/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
    with(df, mean(abs(retPred_3 - abs(cumRetAtmIv)), na.rm=T))
    with(df, mean(abs(retPred_2 - abs(cumRetAtmIv)), na.rm=T))
    with(df, mean(abs(retPred_1 - abs(cumRetAtmIv)), na.rm=T))
    with(df, mean(abs(retPred_0 - abs(cumRetAtmIv)), na.rm=T))
    
    # some GARCH test
    {
        N <- 100000
        max_dte <- 252
        omega <- 0.000001; alpha = 0.1; beta = 0.89
        library(rugarch)
        spec <- ugarchspec(    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),    distribution.model = "norm")
        setfixed(spec) <- list(omega = omega, alpha1 = alpha, beta1 = beta)
        sim <- ugarchpath(spec, n.sim = N)
        returns <- fitted(sim)
        sigma <- sigma(sim) * sqrt(252)
        #sigma <- NA
        #returns <- rnorm(N, 0, sigma)
        df <- data.frame(returns = returns, sigma = sigma, group = rep(1:ceiling(N / max_dte), each = max_dte)[1:N],  dte =rep(max_dte:1, length.out=N))
        df <- df %>% group_by(group) %>% mutate(cum_returns = map_dbl(1:n(), ~ sum(returns[(.x+1):n()])), cum_returns = replace_na(cum_returns, 0)) 
        df <- df %>% ungroup %>% mutate(volatility = calculate_volatility(returns))
        df <- df %>% mutate(dte_ = dte - 1,
                            ret_pred_0 = sigma / sqrt(252) * sqrt(dte_) *  sqrt(2 / pi),
                            ret_pred_1 = runSD(returns, 252) * sqrt(dte_) *  sqrt(2 / pi),
                            ret_pred_2 = volatility / sqrt(252) * sqrt(dte_) *  sqrt(2 / pi),
                            ret_pred_3 = h_step_ahead_volatility3(volatility / sqrt(252), dte_, omega, alpha, beta) *  sqrt(2 / pi)
        )
        with(df, matplot2(tail(cbind(ret_pred_0, ret_pred_1, ret_pred_2, ret_pred_3, abs(cum_returns)), 500)))
        df %>% mutate(i = 1:nrow(df), cum_returns=abs(cum_returns)) %>% select(cum_returns, ret_pred_0:ret_pred_3) %>% reshape2::melt("cum_returns") %>% na.omit %>% mutate(err = cum_returns-value) %>% group_by(variable) %>% reframe(M=mean(err^2))
    }   
    {
        gbm_vec(10000, mu = 0.0001026322*252, sigma = 0.2262341) -> gbm
        option_sim_profit(gbm, type="call", v = 0.2537848) -> a
        option_sim_profit(gbm, type="put", v = 0.2537848) -> b
        profit_straddle <- (-a$profit-b$profit)*100
        profit_straddle %>% summary
    }
    # Various correlations with SPY (SPY is a data.frame with barchart vol data and historical volatilities)
    {
        a <- select(Barchart_vols_stocks, Symbol, Date, HV1) %>% pivot_wider(names_from = Symbol, values_from = HV1)
        b <- merge(a, SPY %>% select(Date, HV1) %>% rename(SPY=HV1), by="Date")
        hv1 <- cor(b[,-1], use="pairwise.complete.obs")[,"SPY"]
        a <- select(Barchart_vols_stocks, Symbol, Date, IV) %>% pivot_wider(names_from = Symbol, values_from = IV)
        b <- merge(a, SPY %>% select(Date, IV) %>% rename(SPY=IV), by="Date")
        iv <- cor(b[,-1], use="pairwise.complete.obs")[,"SPY"] 
        a <- select(Barchart_vols_stocks, Symbol, Date, ReturnPrice) %>% pivot_wider(names_from = Symbol, values_from = ReturnPrice)
        b <- merge(a, SPY %>% select(Date, ReturnPrice) %>% rename(SPY=ReturnPrice), by="Date")
        ret <- cor(b[,-1], use="pairwise.complete.obs")[,"SPY"] 
        SPY_correlations <- data.frame(Symbol=names(ret), corHV1=hv1, corIV=iv, corReturn=ret)
        write_csv(SPY_correlations, "/home/marco/trading/HistoricalData/Barchart/Options/SPY_correlations.csv")
        # ORATS VIX corr
        a <- ORATS_core %>% select(tradeDate, ticker, iv30d)
        b <- ORATS_core %>% select(tradeDate, ticker, iv30d) %>% filter(ticker == "SPX") %>% rename(VIX=iv30d) %>% select(-ticker)
        df <- full_join(a, b, by="tradeDate")
        Rho_static <- df %>% arrange(ticker, tradeDate) %>% group_by(ticker) %>% reframe(rho = cor(iv30d, VIX, use = "pairwise.complete.obs"), N=n())
        Rho_dynamic <- df %>% arrange(ticker, tradeDate)  %>% group_by(ticker) %>% mutate(rho = runCor(iv30d %>% na.locf(na.rm = F), VIX%>% na.locf(na.rm = F), n = 252, use = "pairwise.complete.obs"), N=n())
    }
}


