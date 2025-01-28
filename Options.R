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
    library(rugarch)
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
        round(round(x / n) * n, get_decimal_places(n))
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
    hedges <- rep(0, ncol(gbm))
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
    
    cols_to_extract <- c('ticker', 'tradeDate', 'pxAtmIv', 
                         "mktCap",  "beta1m", "beta1y",
                         "straPxM1", "straPxM2", "smoothStraPxM1",
                          "cVolu",  "cOi" , "pVolu",  "pOi", "avgOptVolu20d", 
                         "atmIvM1", "dtExM1",  "atmIvM2", "dtExM2", "iv30d", "iv6m", "volOfIvol", "clsHv20d", "clsHv252d",
                         "contango", "slope", "deriv", "confidence", "borrow30"
                         )
    # Loads all ORATS core files, selecting interesting columns
    dir <- "/media/marco/Elements/ORATS/cores/"
    files <- list.files(dir, pattern = "orats_core_20*.*gz")
    ORATS_core <- files %>% purrr::map_df(.f = load_orats_day, cols_to_extract)
    ### The following lines will calculate estimated straddle returns as abs(returns) - (straddle / price)
    # Calculate price returns, set negative prices to zero
    ORATS_core <- ORATS_core  %>% mutate(tradeDate=as.Date(tradeDate)) %>% mutate(pxAtmIv = case_when(pxAtmIv < 0 ~ 0, TRUE ~ pxAtmIv)) %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(retAtmIv = c(0, diff(log(pxAtmIv))), .after = pxAtmIv)
    # Remove returns > 0.1 as they are usually from stock splits (maybe set them to zero?). Also removes returns == 0? CHECK THIS
    ORATS_core <- ORATS_core  %>% mutate(retAtmIv = case_when(abs(retAtmIv) > 0.1 ~ 0, TRUE ~ retAtmIv)) # maybe retAtmIv == 0 ~ NA ?
    # Get next expiry date (ignore dtExM1 == 0 as they represent the day after expiration)
    ORATS_core <- ORATS_core %>% group_by(ticker) %>% mutate(dte = case_when(dtExM1 > 0 ~ dtExM1, TRUE ~ NA), expiryDate = tradeDate + dte - 1, .after = tradeDate)
    # Calculate cumulative price returns from current date to expiry 
    ORATS_core <- ORATS_core %>% group_by(ticker, expiryDate) %>% arrange(tradeDate) %>% mutate(cumRetAtmIv = map_dbl(1:n(), ~ sum(retAtmIv[(.x+1):n()])), .after = retAtmIv) 
    # Calculate expected straddle returns (ignore straddle whose prices are too high)
    ORATS_core <- ORATS_core %>% mutate(straRetM1 = abs(cumRetAtmIv) - straPxM1 / pxAtmIv, .after = cumRetAtmIv) %>% mutate(straRetM1 = case_when(straPxM1 > pxAtmIv*10 ~ NA, TRUE ~ straRetM1)) 
    # Calculate VRP, replace infinites with NA
    ORATS_core <- ORATS_core %>% group_by(ticker) %>% mutate(VRP = (log(iv30d / lead(clsHv20d, 21))) %>% replace(is.infinite(.), NA),  .after = straRetM1) %>% ungroup
    # Calculate Normalized Percentage Straddle Price 
    ORATS_core <- ORATS_core %>% mutate(straNormM1 = straPxM1 / dte  / pxAtmIv * 100, 
                                        straRetNorm = straRetM1 / dte  * 100, 
                                        straRetNorm2 = straRetM1 / sqrt(dte) * 100, .after = straPxM1) %>% ungroup
    # Misc stuff, IV percentile
    ORATS_core <- ORATS_core %>% group_by(ticker) %>% mutate(IVpct = if(n() > 500) runPercentRank(iv30d, 252) else NA) %>% ungroup
    #write_parquet(ORATS_core, "/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq")
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



# ORATS core data general observations 
{
    ORATS_core <- read_parquet("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq")
    # Simple backtest
    ORATS_core %>% filter(ticker=="AAPL") %>% group_by(expiryDate) %>% reframe(M=mean(straRetNorm, na.rm=T)) %>% mutate(PnL=cumsum(replace_na(M, 0))) %>% ggplot(aes(expiryDate, PnL)) + geom_line() + geom_point()
    ## Straddles return over tradeDate summarized with bars- moments
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(mktCap, 5)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=mean(straRetNorm2 , na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(mktCap, 5)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=sd(straRetNorm2, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(mktCap, 5)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=skewness(straRetNorm2 %>% replace(.,is.infinite(.), NA), na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(mktCap, 5)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=kurtosis(straRetNorm2 %>% replace(.,is.infinite(.), NA), na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ## Straddles return over all data (or by ticker) summarized with density plots
    # market cap 
    ORATS_core %>% mutate(Decile = factor(ntile(mktCap, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # dte
    ORATS_core %>% filter(dtExM1<=30) %>% group_by(ticker, dtExM1) %>% reframe(Value=mean(straRetNorm2 , na.rm=T))  %>% group_by(dtExM1) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=dtExM1, ymin=M-S, ymax=M+S)) + geom_errorbar() 
    # price
    ORATS_core %>% mutate(Decile = factor(ntile(pxAtmIv, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # normalized straddle price
    ORATS_core %>% mutate(Decile = factor(ntile(straNormM1, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # beta
    ORATS_core %>% mutate(Decile = factor(ntile(beta1y, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # iv30 - inter ticker and intra ticker 
    ORATS_core %>% mutate(Decile = factor(ntile(iv30d, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    ORATS_core %>% group_by(ticker) %>% mutate(Decile = factor(ntile(iv30d, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # clsHv20d - inter ticker and intra ticker (almost identical to above iv30)
    ORATS_core %>% group_by(ticker) %>% mutate(Decile = factor(ntile(log(iv30d/clsHv20d), 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # IVpct - intra ticker, it seems that VRP is better predicted than straddle return
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% mutate(Decile = factor(ntile(IVpct, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # volofIvol - inter ticker  only
    ORATS_core %>% mutate(Decile = factor(ntile(volOfIvol, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # iv30d correlation with SPX's
    ORATS_core %>% arrange(ticker, tradeDate) %>% mutate(rho=Rho_dynamic$rho) %>% group_by(tradeDate) %>% mutate(Decile = ntile(rho, 5)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=median(straRetNorm2 , na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    # contango - inter ticker and intra ticker 
    ORATS_core %>%  mutate(Decile = factor(ntile(contango, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    ORATS_core %>% group_by(ticker) %>% mutate(Decile = factor(ntile(contango, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # slope - inter ticker only
    ORATS_core %>% mutate(Decile = factor(ntile(slope, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # deriv - inter ticker only
    ORATS_core %>% mutate(Decile = factor(ntile(deriv, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # confidence - inter ticker  only
    ORATS_core %>% mutate(Decile = factor(ntile(confidence, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # Straddles return PACF autocorrelation by dte (probably it's just VRP making straddles correlated)
    ORATS_core %>% arrange( ticker, dtExM1, tradeDate) %>% group_by(dtExM1) %>% filter(n()>10000) %>% reframe(Acf = pacf(na.omit(straRetNorm2), plot = F, lag.max = 1)$acf[[1]], n()) %>% ggplot(aes(dtExM1, Acf)) + geom_point()
    # month day
    ORATS_core %>% mutate(Decile = mday(tradeDate)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(value=median(straRetNorm2 , na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(value, na.rm=T), S=sd(value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    # stock return (maybe in absolute terms? but it is basically vol) - inter ticker and intra ticker 
    ORATS_core %>%  mutate(Decile = factor(ntile(retAtmIv %>% replace_na(0), 5))) %>% na.omit %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    ORATS_core %>% group_by(ticker) %>%  mutate(Decile = factor(ntile(retAtmIv %>% replace_na(0)  , 5))) %>% na.omit %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    # stock price momentum - inter ticker and intra ticker ARRANGE IS IMPORTANT!!!
    ORATS_core %>% arrange(ticker, tradeDate) %>% group_by(ticker) %>% filter(n()>252)%>% mutate(rsi=pxAtmIv %>% na.locf(na.rm = F) %>% RSI2(., 60, maType=EMA)) %>% ungroup() %>% mutate(Decile=factor(ntile(rsi,5))) %>% na.omit %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0)+ xlim(c(-10,10))   + scale_color_colorblind() 
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% arrange(ticker, tradeDate) %>%  mutate(Decile = factor(ntile(pxAtmIv %>% na.locf(na.rm = F) %>% RSI2(., 60, maType=EMA), 5))) %>% na.omit %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10))
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% arrange(tradeDate) %>%  mutate(pred = retAtmIv %>% replace_na(0) %>%  {roll_meanr(.,252)/roll_sdr(.,252)} ) %>% ungroup %>% mutate(Decile = factor(ntile(pred, 5))) %>% ggplot(aes(x=straRetNorm2, color=Decile, group=Decile)) + geom_density(linewidth=1) + geom_vline(xintercept = 0) + scale_color_colorblind() + xlim(c(-10,10)) # Slow!
    ORATS_core %>% arrange(ticker, tradeDate) %>% group_by(ticker) %>% filter(n()>252) %>% mutate(rsi=pxAtmIv %>% na.locf(na.rm = F) %>% RSI2(., 252, maType=EMA)) %>% mutate(Decile=factor(ntile(rsi,8))) %>% group_by(ticker, Decile) %>% reframe(Value=mean(straRetNorm2 , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Straddle return by OI ratio - inter ticker
    ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(cOi/pOi, 5)) %>% group_by(ticker, Decile, Year=year(tradeDate)) %>% reframe(Value=mean(straRetNorm2 , na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    ## Straddle return (or VRP, check it!) by ticker summarized as bars
    # Momentum
    ORATS_core %>% arrange(ticker, tradeDate) %>% group_by(ticker) %>% filter(n()>252) %>% mutate(rsi=pxAtmIv %>% na.locf(na.rm = F) %>% RSI2(., 252, maType=EMA)) %>% mutate(Decile=factor(ntile(rsi,8))) %>% group_by(ticker, Decile) %>% reframe(Value=mean(straRetNorm2 , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar(
    # Percentile IV, HV or IV-HV
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% mutate(pred=runPercentRank(clsHv20d, 252)) %>% mutate(Decile=round(pred, 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(straRetNorm2 , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Confidence
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% mutate(pred=confidence/100) %>% mutate(Decile=round(pred, 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(VRP , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Contango, curiosly, V-shaped for straddle, linear for VRP (iv6m-iv30d is very similar)
    ORATS_core%>% group_by(ticker) %>% filter(n()>252) %>% mutate(pred=runPercentRank(contango, 252)) %>% mutate(Decile=round(pred, 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(VRP , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Vol of Vol
    ORATS_core %>% group_by(ticker) %>% filter(n()>252) %>% mutate(pred=runPercentRank(volOfIvol, 252)) %>% mutate(Decile=round(pred, 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(VRP , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Price
    ORATS_core %>% group_by(ticker) %>% filter(n()>500) %>% mutate(pred=runPercentRank(pxAtmIv, 252)) %>% mutate(Decile=round(pred, 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(straRetNorm2 , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
}


# ORATS api data single stock playing with (SPY as example)
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

# ORATS strikes data straddles returns 
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

# ORATS strikes bid-ask spreads
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
    last_month <- ORATS_core %>% mutate(YM=yearmonth(tradeDate)) %>% group_by(ticker) %>% arrange(tradeDate) %>% reframe(YM=last(YM))
    ORATS_tradable <- ORATS_bidask_spread %>% inner_join(last_month, by="ticker") %>%  filter(YM == yearmonth("2024 Aug"), spread_price_mean+spread_price_stderr < 10 & spread_opt_mean+spread_opt_stderr < 50) %>% pull(ticker)
} 

# Fundamentals
{
    # Load ORATS core data
    #ORATS_core <- read_csv("/home/marco/trading/HistoricalData/ORATS/ORATS_core.csv.gz")
    ORATS_core_ds <- open_dataset("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq") %>% arrange(ticker, tradeDate)
    # Load dolthub fundamentals
    balance_sheet_assets <- read_csv("/home/marco/trading/HistoricalData/Dolthub/post-no-preference_earnings_master_balance_sheet_assets.csv.gz", show_col_types = F)
    balance_sheet_equity <- read_csv("/home/marco/trading/HistoricalData/Dolthub/post-no-preference_earnings_master_balance_sheet_equity.csv.gz", show_col_types = F)
    balance_sheet_liabilities <- read_csv("/home/marco/trading/HistoricalData/Dolthub/post-no-preference_earnings_master_balance_sheet_liabilities.csv.gz", show_col_types = F)
    income_statement <- read_csv("/home/marco/trading/HistoricalData/Dolthub/post-no-preference_earnings_master_income_statement.csv.gz", show_col_types = F)
    cash_flow_statement <- read_csv("/home/marco/trading/HistoricalData/Dolthub/post-no-preference_earnings_master_cash_flow_statement.csv.gz", show_col_types = F)
    eps_history <- read_csv("/home/marco/trading/HistoricalData/Dolthub/post-no-preference_earnings_master_eps_history.csv.gz", show_col_types = F) %>% mutate(period = "Quarter", EPS = reported) %>% rename(date = period_end_date)
    temp <- Reduce(function(...) full_join(..., by = c("act_symbol", "date", "period")), list(balance_sheet_assets, balance_sheet_equity, balance_sheet_liabilities, income_statement, cash_flow_statement, eps_history)) %>% arrange(date) %>% filter(period=="Quarter") %>% mutate(net_income = net_income.x)
    # Calculate some intesting derivative data not present in the dolthub
    temp <- temp %>% mutate(roe = net_income / total_equity, 
                            current_ratio = total_current_assets / total_current_liabilities,
                            debt_to_assets = total_liabilities / total_assets, 
                            asset_turnover_ratio = sales / total_assets)
    cols_to_select <- c("mktCap", "beta1y", "roe", "current_ratio", "debt_to_assets", "asset_turnover_ratio", "book_value_per_share", "EPS")
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
    # Get normalized straddle returns, and remove infinite values
    ORATS_core_fundamentals <- ORATS_core_fundamentals %>% select(ticker, tradeDate, pxAtmIv, VRP, straRetNorm, all_of(cols_to_select)) %>% mutate(across(c(VRP, all_of(cols_to_select)), ~ ifelse(is.infinite(.), NA, .))) 
    # Create binnings of the fundamentals data
    df_orats_fundamentals <- ORATS_core_fundamentals %>%  group_by(tradeDate) %>% mutate(across(all_of(cols_to_select), ~ntile(.,10))) %>%  ungroup
    # See which ones are interesting (market cap is usually the winner, I ignored current_ratio to simplify)
    df_orats_fundamentals %>% select(all_of(cols_to_select)) %>% cor(use = "pairwise.complete.obs") %>% corrplot::corrplot()
    write_csv(df_orats_fundamentals, "/home/marco/trading/HistoricalData/ORATS/Fundamentals_ORATS.csv")
    fit_gam_straddle <- gam(Y ~ s(mktCap) + s(beta1y)  + s(roe) + s(debt_to_assets) + s(book_value_per_share) + s(asset_turnover_ratio) , data=df_orats_fundamentals %>% mutate(Y=straRetNorm) %>% select(-ticker, -tradeDate, -VRP,-straRetNorm), , method = "REML")
    fit_gam_vrp <- gam(Y ~ s(mktCap) + s(beta1y)  + s(roe)  + s(debt_to_assets) + s(book_value_per_share) + s(asset_turnover_ratio), data=df_orats_fundamentals %>% mutate(Y=VRP) %>% select(-ticker, -tradeDate, -VRP,-straRetNorm), , method = "REML")
    # Some ticker time series predictions
    df_orats_fundamentals %>% filter(ticker %in% c("AAPL", "TRIP", "GLTO")) %>% group_by("ticker") %>% mutate(pred=predict(fit_gam_straddle, newdata=.)) %>% ggplot(aes(tradeDate, pred, color=ticker)) +geom_path() + geom_point(size=3)
    # Decile prediction
    df_orats_fundamentals %>% select(-ticker, -VRP) %>% pivot_longer(-c(tradeDate, straRetNorm)) %>% group_by(name, value) %>% reframe(M=mean(straRetNorm, na.rm=T), S=sd(straRetNorm, na.rm=T)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=value, ymin=M-S, ymax=M+S)) + geom_errorbar()    + facet_wrap(~name)
    # Quick and dirty backtest by decile
    df_orats_fundamentals %>% mutate(pred=mktCap, profit = straRetNorm)  %>% group_by(tradeDate, pred) %>% reframe(M=mean(profit, na.rm=T)) %>% na.omit %>% group_by(pred) %>% mutate(PnL=cumsum(M))  %>% ggplot(aes(tradeDate, PnL, color=factor(pred))) + geom_line(linewidth=2)
    # Backtesting strategy: short bottom decile, long top decile
    df_orats_fundamentals %>% mutate(pred = mktCap, signal = case_when(pred == 1 ~ -1, pred == 10 ~ 1, TRUE ~ 0)) %>% mutate(profit = straRet*(signal))  %>% group_by(tradeDate) %>% reframe(M=mean(profit, na.rm=T)) %>% pull(M) %>% ts %>% SharpeRatio()
}

# Final tickers list
{
    start_year <- 2013
    last_day_fundamentals <- df_orats_fundamentals %>% filter(ticker %in% ORATS_tradable) %>% 
        group_by(ticker) %>% arrange(tradeDate) %>% slice_tail(n = 1) %>% 
        select(ticker, tradeDate, mktCap, roe,  debt_to_assets , book_value_per_share,asset_turnover_ratio, EPS) %>% ungroup
    tickers_performance <- ORATS_core %>% filter(ticker %in% ORATS_tradable & year(tradeDate) >= start_year)  %>% 
        group_by(ticker, dtExM1) %>% filter(n() > 30) %>%  reframe(Ms=mean(straRetNorm2, na.rm=T), Mv=mean(VRP, na.rm=T), pxAtmIv=last(pxAtmIv), beta1y=last(beta1y)) %>% na.omit %>% 
        group_by(ticker) %>% reframe(SRstraddle = mean(Ms)/sd(Ms), SRvrp = mean(Mv)/sd(Mv), pxAtmIv=last(pxAtmIv), beta1y=last(beta1y), N=n())
    tickers_selection <- full_join(tickers_performance, last_day_fundamentals, by="ticker")
    tickers_selection %>% filter(SRstraddle < -2 & SRvrp > 6.5 & pxAtmIv < 50) %>% View

    NNOX
    SAVA
    AMC
    GME
    UNG
    PRGO
    EXEL?
        SCO
    GO?
        RCAT?
        RINF?
        VTIP
    SRLN
    IMVT
    IMAX
    TVTX
    ANAB
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
ibm %>% mutate(W = 1, P=straRetNorm2/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
ibm %>% mutate(W = (retPred_0 - straPxM1 / pxAtmIv) , W=sign(W),P=straRetNorm2/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
ibm %>% mutate(W = (retPred_1 - straPxM1 / pxAtmIv) , W=sign(W),P=straRetNorm2/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
ibm %>% mutate(W = (retPred_2 - straPxM1 / pxAtmIv) , W=sign(W),P=straRetNorm2/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
ibm %>% mutate(W = (retPred_3 - straPxM1 / pxAtmIv) , W=sign(W),P=straRetNorm2/100*W) %>% pull(P) %>% na.omit  %>% strategy_performance(period = 1) %>% unlist
with(df, mean(abs(retPred_3 - abs(cumRetAtmIv)), na.rm=T))
with(df, mean(abs(retPred_2 - abs(cumRetAtmIv)), na.rm=T))
with(df, mean(abs(retPred_1 - abs(cumRetAtmIv)), na.rm=T))
with(df, mean(abs(retPred_0 - abs(cumRetAtmIv)), na.rm=T))

# some GARCH test
{
N <- 100000
max_dte <- 30
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
}

# Testing ticker wise volatility prediction
{
    alpha <- 0.1; beta <- 0.89
    k0 <- sqrt(2 / pi)
    df <- ORATS_core %>% select(tradeDate, ticker, dte, retAtmIv, cumRetAtmIv) %>% group_by(ticker) %>% filter(n()>1000) %>% arrange(tradeDate) %>%  mutate(returns = replace_na(retAtmIv, 0), dte_ = dte - 1, volatility = calculate_volatility(returns), volatility_long = runSD(returns, 500) * sqrt(252), omega = (volatility_long/sqrt(252))^2 * (1-alpha-beta))
    df <- df %>% group_by(ticker) %>% mutate(
                        ret_pred_0 = abs(returns) * sqrt(dte_) * k0,
                        ret_pred_1 = volatility_long / sqrt(252) * sqrt(dte_) * k0,
                        ret_pred_2 = volatility / sqrt(252) * sqrt(dte_) * k0,
                        ret_pred_3 = h_step_ahead_volatility3(volatility / sqrt(252), dte_, omega, alpha, beta) * sqrt(dte_) * k0)
    df <- df %>% mutate(err0 = ((abs(cumRetAtmIv)-ret_pred_0)/dte_) * 252, 
                        err1 = ((abs(cumRetAtmIv)-ret_pred_1)/dte_) * 252, 
                        err2 = ((abs(cumRetAtmIv)-ret_pred_2)/dte_) * 252, 
                        err3 = ((abs(cumRetAtmIv)-ret_pred_3)/dte_) * 252)
    z <- df %>% group_by(ticker, Year=year(tradeDate)) %>% reframe(m0=mean(err0^2, na.rm=T),m1=mean(err1^2, na.rm=T), m2=mean(err2^2, na.rm=T), m3=mean(err3^2, na.rm=T))     
    
}

# Simulating option prices
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

# Simulate some strategies like E.Sinclair in Positional Options Trading
{
    
    # Good Straddle  
    gbm <- gbm_vec(10000, sigma = 0.2)
    option_sim_profit(gbm, type="call") -> a
    option_sim_profit(gbm, type="put") -> b
    profit <- (-a$profit-b$profit)*100
    hist(profit, 50); summary(profit)
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
