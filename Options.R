

source("/home/marco/trading/Systems/Options//OptionsCommon.R")



# ORATS core data loading, with straddle expected returns
{
    # function for loading an ORATS core data file and returning a subset of columns
    quiet_read_csv <- purrr::quietly((.f = read_csv))
    quiet_fread <- purrr::quietly((.f = fread))
    
    load_orats_day <- function(filename, cols_to_extract) {
        print(filename)
        # quiet_read_csv(glue::glue("/media/marco/Elements/ORATS/cores/{filename}")) #%>%
        quiet_fread(glue::glue("/home/marco/trading/HistoricalData/ORATS/core/{filename}")) %>%
            purrr::pluck("result")  %>%  
            #dplyr::filter(assetType < 4)  %>%  
            dplyr::select(all_of(cols_to_extract)) %>%   
            mutate(across(3:ncol(.), ~ as.single(.))) # choose the right starting numeric column
    }
    
    cols_to_extract <- c('ticker', 'tradeDate', 'pxAtmIv', 'hiStrikeM1', 'hiStrikeM2',
                         "stkPxChng1wk", "stkPxChng1m", "stkPxChng6m",
                         "mktCap", "beta1y", "correlSpy1y", 
                         "straPxM1", "straPxM2", "atmIvM1",	"atmIvM2",
                         "avgOptVolu20d", #"cVolu",  "cOi" , "pVolu",  "pOi", 
                         "dtExM1","dtExM2", 
                         "iv10d", "iv30d", "iv90d", "iv6m", "iv1yr", "volOfIvol", 
                         "orHv5d", "orHv10d", "orHv20d", "clsHv20d", "clsHv60d", "clsHv120d", "clsHv252d",
                         "exErnIv30d", "orHvXern20d", # ex-Ern VRP
                         "ivHvXernRatio", "ivEtfRatio", "etfIvHvXernRatio",  
                         "fexErn60_30", "ffexErn60_30",
                         "slope", "contango", "deriv"
                         )
    
    # Loads all ORATS core files, selecting interesting columns
    dir <- "/home/marco/trading/HistoricalData/ORATS/core/"
    files <- c(list.files(dir, pattern = "orats_core_201[5-9].*gz"), list.files(dir, "orats_core_202[0-9].*gz"))
    ORATS_core <- files %>% purrr::map_df(.f = load_orats_day, cols_to_extract)
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
    # Calculate straddle returns, exclude straddle return when estimated daily returns during that period are > 10% (usually are stock splits) or straddle price > 10*stock price. Also, ignore 0DTEs. 
    ORATS_core <- ORATS_core %>% mutate(
                                        straProM1 = abs(pxAtmIvM1 - hiStrikeM1) - straPxM1, 
                                        straProM2 = abs(pxAtmIvM2 - hiStrikeM2) - straPxM2,
                                        straRetM1 = straProM1 / pxAtmIv, 
                                        straRetM2 = straProM2 / pxAtmIv, 
                                        .after = straPxM2) %>% 
                                  mutate(
                                         straProM1 = case_when(abs(pxAtmIvM1 - hiStrikeM1)/pxAtmIvM1/sqrt(dte1+1) > 0.1 | straPxM1 > pxAtmIv*10 | straPxM1 == 0 | dtExM1 == 1 ~ NA, TRUE ~ straProM1),
                                         straProM2 = case_when(abs(pxAtmIvM2 - hiStrikeM2)/pxAtmIvM2/sqrt(dte2+1) > 0.1 | straPxM2 > pxAtmIv*10 | straPxM2 == 0 | dtExM2 == 1 ~ NA, TRUE ~ straProM2),
                                         straRetM1 = case_when(abs(pxAtmIvM1 - hiStrikeM1)/pxAtmIvM1/sqrt(dte1+1) > 0.1 | straPxM1 > pxAtmIv*10 | straPxM1 == 0 | dtExM1 == 1 ~ NA, TRUE ~ straRetM1),
                                         straRetM2 = case_when(abs(pxAtmIvM2 - hiStrikeM2)/pxAtmIvM2/sqrt(dte2+1) > 0.1 | straPxM2 > pxAtmIv*10 | straPxM2 == 0 | dtExM2 == 1 ~ NA, TRUE ~ straRetM2)#,
                                        )

    # Calculate expected straddle returns (ignore straddle whose prices are too high)
    # ORATS_core <- ORATS_core %>% mutate(straRetM1 = abs(cumRetAtmIvM1) - straPxM1 / pxAtmIv, 
    #                                     straRetM2 = abs(cumRetAtmIvM2) - straPxM2 / pxAtmIv, .after = cumRetAtmIvM2) %>% 
    #                                     mutate(straRetM1 = case_when(straPxM1 > pxAtmIv*10 ~ NA, TRUE ~ straRetM1), 
    #                                            straRetM2 = case_when(straPxM2 > pxAtmIv*10 ~ NA, TRUE ~ straRetM2))

    # Calculate logVRP, replace infinites with NA
    ORATS_core <- ORATS_core %>% group_by(ticker) %>% 
        mutate(logVRP = (log(iv30d / lead(orHv20d, 20))) %>% replace(is.infinite(.), NA),  
               logVRPXern = (log(exErnIv30d / lead(orHvXern20d, 20))) %>% replace(is.infinite(.), NA),  
               .before = straProM1) %>% ungroup 
    # Misc stuff, IV percentile
    write_parquet(ORATS_core, "/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq")
}

# ORATS hv data loading, from core and dailies datasets
{
    historical_dir <- "/home/marco/trading/HistoricalData/ORATS/"
    
    # function for loading an ORATS core data file and returning a subset of columns
    quiet_read_csv <- purrr::quietly((.f = read_csv))
    quiet_fread <- purrr::quietly((.f = fread))
    
    load_orats_day <- function(filename) {
        print(filename)
        quiet_fread(glue::glue(paste0(historical_dir, "hvs/{filename}"))) %>%
            purrr::pluck("result")  
    }
    # Loads all ORATS hvs files, selecting interesting columns
    dir <- paste0(historical_dir, "hvs")
    files <- c(list.files(dir, pattern = "orats_hvs_201[3-9].*gz"), list.files(dir, "orats_hvs_202[0-9].*gz"))
    ORATS_hvs <- files %>% purrr::map_df(.f = load_orats_day)
    write_parquet(ORATS_hvs, "/home/marco/trading/HistoricalData/ORATS/ORATS_hvs.pq")
    
    # Loads all ORATS hvs files, selecting interesting columns
    load_orats_day <- function(filename) {
        print(filename)
        quiet_fread(glue::glue(paste0(historical_dir, "dailies/{filename}"))) %>%
            purrr::pluck("result") 
    }
    dir <- paste0(historical_dir, "dailies")
    files <- c(list.files(dir, pattern = "orats_dailies_201[3-9].*gz"), list.files(dir, "orats_dailies_202[0-9].*gz"))
    ORATS_dailies <- files %>% purrr::map_df(.f = load_orats_day)
    write_parquet(ORATS_dailies, "/home/marco/trading/HistoricalData/ORATS/ORATS_dailies.pq")
    
    # Merge into a final file with prices and historical volatility
    ORATS_prices <- merge(ORATS_dailies, ORATS_hvs, by=c("ticker", "tradeDate"))
    write_parquet(ORATS_prices, "/home/marco/trading/HistoricalData/ORATS/ORATS_prices.pq")
    
    
}


# ORATS core data general observations 
{
    ORATS_core <- read_parquet("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq") %>% arrange(tradeDate)
    # Simple backtest
    ORATS_core %>% filter(ticker=="AAPL") %>% group_by(expiryDate1) %>% reframe(M=mean(straRetM1, na.rm=T)) %>% mutate(PnL=cumsum(replace_na(M, 0))) %>% ggplot(aes(expiryDate1, PnL)) + geom_line() + geom_point()
    ## VRPs WARNING: sometimes iv6m and iv1y have many zeros 
    ORATS_core %>% mutate(VRP_30 = log(iv30d / lead(clsHv20d, 20)), VRP_90 = log(iv90d / lead(clsHv60d, 60)), VRP_180 = log(iv6m / lead(clsHv120d, 120)), VRP_365 = log(iv1yr / lead(clsHv252d, 252))) %>% 
        select(tradeDate, ticker, VRP_30, VRP_90, VRP_180, VRP_365) %>% pivot_longer(cols = c(VRP_30,  VRP_90,  VRP_180, VRP_365)) %>%  
        mutate(value = replace(value, is.infinite(value) | is.nan(value), NA)) %>% group_by(name, ticker) %>% reframe(M=mean(value, na.rm=T)) %>%  group_by(name) %>%  reframe(Mean=mean(M, na.rm=T), SD=sd(M, na.rm=T)/sqrt(n()), N=n())  %>% mutate(name = factor(name, levels=c("VRP_30", "VRP_90", "VRP_180", "VRP_365"))) %>% ggplot(aes(name, ymin=Mean-SD, ymax=Mean+SD)) + geom_errorbar(width = 0.5)
    ## Straddles cross-sectional return over tradeDate - all moments
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
    ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% filter(n()>252) %>% mutate(pred=confidence/100) %>% mutate(Decile=round(lag(pred), 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(logVRP , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Contango, (iv6m-iv30d is very similar)
    ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% filter(n()>252) %>% mutate(pred=runPercentRank(contango, 252)) %>% mutate(Decile=round(lag(pred), 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(logVRP , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Vol of Vol
    ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% filter(n()>252) %>% mutate(pred=runPercentRank(volOfIvol, 252)) %>% mutate(Decile=round(lag(pred), 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(logVRP , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # Avg Volume
    ORATS_core %>% group_by(ticker) %>% arrange(tradeDate) %>% filter(n()>500) %>% mutate(pred=runPercentRank(avgOptVolu20d, 252)) %>% mutate(Decile=round(lag(pred), 1)) %>% group_by(ticker, Decile) %>% reframe(Value=mean(straRetM1 , na.rm=T))  %>% group_by(Decile) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
}

# Plotting several observations
{
            
    # Cross sectional predictors, useful to select which stocks to trade
    dir <- "/home/marco/trading/Systems/Options/Plots/byDate/"
    setwd(dir)
    for(predictor in c("mktCap", "beta1y", "pxAtmIv", "confidence", "slope", "contango", "avgOptVolu20d", "iv30d", "clsHv20d", "volOfIvol")) {
        print(predictor)
        df <- ORATS_core %>% group_by(tradeDate) %>% mutate(Decile = ntile(!!sym(predictor), 8)) %>% group_by(ticker, Decile, Year=year(tradeDate))
        a <- df %>% reframe(Value=mean(straRetM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=mean(straLogM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straLogM1") + ggtitle("")
        d <- df %>% reframe(Value=mean(logVRP, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("logVRP") + ggtitle("")
        p <- a+b+d
        ggsave(paste0(predictor, ".png"), p, width = 12, height = 6)
    }
    # Single ticker predictors, useful to see how to trade one stock
    dir <- "/home/marco/trading/Systems/Options/Plots/byTicker/"
    setwd(dir)
    df <- ORATS_core %>% group_by(ticker) %>% arrange(tradeDate)  %>% collect %>% filter(n()>500) %>% ungroup
    for(predictor in c("pxAtmIv", "confidence", "slope", "contango", "avgOptVolu20d", "iv30d", "clsHv20d", "log(iv30d/clsHv20d)", "volOfIvol")) {
        print(predictor)
        #df <- df %>% group_by(ticker) %>% mutate(pred=runPercentRank(na.locf(eval(parse_expr(predictor)), na.rm=F), 252)) %>% mutate(Decile=ntile(lag(EMA(pred)), 8))  %>% group_by(ticker, Decile, Year=year(tradeDate))
        df <- df %>% group_by(ticker) %>% mutate(pred=eval(parse_expr(predictor))) %>% mutate(Decile=ntile(lag(pred), 8))  %>% group_by(ticker, Decile, Year=year(tradeDate))
        a <- df %>% reframe(Value=mean(straRetM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=mean(straLogM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straLogM1") + ggtitle("")
        d <- df %>% reframe(Value=mean(logVRP, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("logVRP") + ggtitle("")
        p <- a+b+d
        ggsave(paste0(predictor, ".png"), p, width = 12, height = 6)
    }
    ## EFTs
    ORATS_core_ds <- open_dataset("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq") 
    etfs_screener <- read_csv("/home/marco/trading/Systems/Options/etf-screener-01-29-2026.csv", show_col_types = F) 
    etfs_list <- etfs_screener %>% group_by(Symbol) %>% reframe(Volume = mean(`Options Vol`, na.rm=T)) %>% filter(Volume > 100) %>% pull(Symbol)
    ORATS_ETFs <- ORATS_core_ds %>% filter(ticker %in% etfs_list) %>% arrange(ticker, tradeDate) %>% collect
    # Cross-sections (over tradeDate) by year
    dir <- "/home/marco/trading/Systems/Options/Plots/ETFs/CrossSectional/byYear/"
    setwd(dir)
    for(predictor in c("mktCap", "beta1y", "pxAtmIv", "correlSpy1y", "confidence", "slope", "contango", "borrow30", "avgOptVolu20d", "ivHvXernRatio", "iv30d", "clsHv20d", "volOfIvol", "fbfexErn60_30")) {
        print(predictor)
        df <- ORATS_ETFs %>% group_by(tradeDate) %>% mutate(Decile = ntile(!!sym(predictor), 8)) %>% group_by(ticker, Decile, Year=year(tradeDate))
        a <- df %>% reframe(Value=mean(straRetM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=mean(logVRP, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("logVRP") + ggtitle("")
        p <- a+b
        ggsave(paste0(predictor, ".png"), p, width = 12, height = 6)
    }
    # Cross-sections (over tradeDate)  by volume
    dir <- "/home/marco/trading/Systems/Options/Plots/ETFs/CrossSectional/byVolume/"
    setwd(dir)
    for(predictor in c("mktCap", "beta1y", "pxAtmIv", "correlSpy1y", "confidence", "slope", "contango", "borrow30", "avgOptVolu20d", "ivHvXernRatio", "iv30d", "clsHv20d", "volOfIvol", "fbfexErn60_30")) {
        print(predictor)
        df <- ORATS_ETFs %>% group_by(tradeDate) %>% mutate(Decile = ntile(!!sym(predictor), 8), Volume= round(log(avgOptVolu20d, 10), 0), Volume = case_when(Volume > 6 ~ 6, Volume < 1 ~ 1, TRUE ~ Volume)) %>% group_by(ticker, Decile, Volume) # I do not summarize volume here because it results in gaps in the final plots
        a <- df %>% reframe(Value=mean(straRetM1, na.rm=T))  %>% group_by(Decile, Volume) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Volume) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=mean(logVRP, na.rm=T))  %>% group_by(Decile, Volume) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Volume) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("logVRP") + ggtitle("")
        p <- a+b
        ggsave(paste0(predictor, ".png"), p, width = 12, height = 6)
    }
    # Time series (over ticker)  by year
    dir <- "/home/marco/trading/Systems/Options/Plots/ETFs/TimeSeries/byYear/"
    setwd(dir)
    for(predictor in c("mktCap", "beta1y", "pxAtmIv", "correlSpy1y", "correlEtf1y", "etfIvHvXernRatio", "ivEtfRatio",  "confidence", "slope", "contango", "borrow30", "avgOptVolu20d", "ivHvXernRatio", "iv30d", "clsHv20d", "volOfIvol", "fbfexErn60_30")) {
        print(predictor)
        df <- ORATS_ETFs %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(Decile = ntile(!!sym(predictor), 8)) %>% group_by(ticker, Decile, Year=year(tradeDate))
        a <- df %>% reframe(Value=mean(straRetM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=mean(logVRP, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("logVRP") + ggtitle("")
        p <- a+b
        ggsave(paste0("Mean/", predictor, ".png"), p, width = 12, height = 6)
        a <- df %>% reframe(Value=sd(straRetM1, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=sd(logVRP, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("logVRP") + ggtitle("")
        p <- a+b
        ggsave(paste0("Std/", predictor, ".png"), p, width = 12, height = 6)
    }
    # Time series (over ticker)  by volume (Cross-sectional makes little sense)
    dir <- "/home/marco/trading/Systems/Options/Plots/ETFs/TimeSeries/byVolume/"
    setwd(dir)
    for(predictor in c("mktCap", "beta1y", "pxAtmIv", "correlSpy1y", "correlEtf1y", "etfIvHvXernRatio", "ivEtfRatio",  "confidence", "slope", "contango", "borrow30", "ivHvXernRatio", "iv30d", "clsHv20d", "volOfIvol", "fbfexErn60_30")) {
        print(predictor)
        df <- ORATS_ETFs %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(Volume = round(log(mean(avgOptVolu20d, na.rm=T), 10), 0), Volume = case_when(Volume > 6 ~ 6, TRUE ~ Volume), Decile = ntile(!!sym(predictor), 8)) %>% group_by(ticker, Decile, Volume)
        a <- df %>% reframe(Value=mean(straRetM1, na.rm=T))  %>% group_by(Decile, Volume) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Volume) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=mean(logVRP, na.rm=T))  %>% group_by(Decile, Volume) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Volume) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("logVRP") + ggtitle("")
        p <- a+b
        ggsave(paste0("Mean/", predictor, ".png"), p, width = 12, height = 6)
        a <- df %>% reframe(Value=sd(straRetM1, na.rm=T))  %>% group_by(Decile, Volume) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Volume) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=sd(logVRP, na.rm=T))  %>% group_by(Decile, Volume) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Volume) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("logVRP") + ggtitle("")
        p <- a+b
        ggsave(paste0("Std/", predictor, ".png"), p, width = 12, height = 6)
        a <- df %>% reframe(Value=skewness(straRetM1, na.rm=T))  %>% group_by(Decile, Volume) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Volume) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("straRetM1") + ggtitle(predictor)
        b <- df %>% reframe(Value=skewness(logVRP, na.rm=T))  %>% group_by(Decile, Volume) %>% reframe(M=mean(Value, na.rm=T), S=sd(Value, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Volume) + ggtitle(predictor) + theme(axis.title.x = element_blank(), axis.text.y = element_text(size=8), axis.text.x = element_blank(), strip.text = element_text(size=8)) + ylab("logVRP") + ggtitle("")
        p <- a+b
        ggsave(paste0("Skew/", predictor, ".png"), p, width = 12, height = 6)
    }
    # Double plots
    ORATS_ETFs %>% group_by(ticker) %>% arrange(tradeDate) %>% 
        mutate(logVRPm = EMA(lag(logVRP) %>% na.locf(na.rm=F), 252), Volume = round(log(avgOptVolu20d+1, 10), 0), Volume = case_when(Volume > 6 ~ 6, TRUE ~ Volume), 
               DecileX = ntile(logVRPm, 8), DecileY = ntile(iv30d, 8)) %>% 
        group_by(DecileX, DecileY, Volume)  %>%reframe(Value = mean(logVRP, na.rm=T)) %>% ggplot(aes(x=DecileX, y = DecileY, fill = Value)) + geom_tile()  + facet_wrap(~Volume, scales="free")+  scale_fill_gradient2(low = "blue",mid = "white",high = "red")
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
    ORATS_core_fundamentals <- ORATS_core_fundamentals %>% select(ticker, tradeDate, logVRP, straRetM1, straRetM2, all_of(cols_to_select)) %>% mutate(across(c(logVRP, all_of(cols_to_select)), ~ ifelse(is.infinite(.), NA, .))) 
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



# General observation about returns and realized volatility
{
    library(MASS)
    library(extraDistr)
    ### Calculate normal VS laplace as predictors
    # Only get ETFs and calculate running means and sds
    ORATS_dailies <- read_parquet("/home/marco/trading/HistoricalData/ORATS/ORATS_dailies.pq")
    ORATS_dailies_ETFs <- ORATS_dailies %>% filter(ticker %in% unique(etf_screener$Symbol))
    ORATS_dailies_ETFs <- ORATS_dailies_ETFs %>% group_by(ticker, tradeDate) %>% arrange(tradeDate)
    ORATS_dailies_ETFs <- ORATS_dailies_ETFs %>% group_by(ticker) %>% filter(n()>500) %>% 
        mutate(
            logret = c(NA, diff(log(clsPx))),
            mean_5 = runMean(logret, 5) * 5,
            mean_10 = runMean(logret, 10) * 10,
            mean_20 = runMean(logret, 20) * 20,
            mean_60 = runMean(logret, 60) * 60,
            mean_90 = runMean(logret, 90) * 90,
            mean_180 = runMean(logret, 180) * 180,
            mean_252 = runMean(logret, 252) * 252,
            sd_5 = runSD(logret, 5) * sqrt(252),
            sd_10 = runSD(logret, 10) * sqrt(252),
            sd_20 = runSD(logret, 20) * sqrt(252),
            sd_60 = runSD(logret, 60) * sqrt(252),
            sd_90 = runSD(logret, 90) * sqrt(252),
            sd_180 = runSD(logret, 180) * sqrt(252),
            sd_252 = runSD(logret, 252) * sqrt(252),
            log_ret_5 = log(dplyr::lead(clsPx, 5) / clsPx),
            log_ret_10 = log(dplyr::lead(clsPx, 10) / clsPx),
            log_ret_20 = log(dplyr::lead(clsPx, 20) / clsPx),
            log_ret_60 = log(dplyr::lead(clsPx, 60) / clsPx),
            log_ret_90 = log(dplyr::lead(clsPx, 90) / clsPx),
            log_ret_180 = log(dplyr::lead(clsPx, 180) / clsPx),
            log_ret_252 = log(dplyr::lead(clsPx, 252) / clsPx)
            )
    spy <- ORATS_dailies_ETFs %>% filter(ticker == "SPY")
    volumes <- ORATS_dailies_ETFs %>% group_by(ticker) %>% reframe(median_volume = median(stockVolume))
    ORATS_dailies_ETFs_top <- ORATS_dailies_ETFs %>% group_by(ticker) %>% 
        filter(ticker %in% (volumes %>% filter(median_volume>5000000) %>% pull(ticker))  & n()>2000)
    # Estimate the df paramer for later use (2 is quite ok)
    # fit_spy <- fitdistr(spy$logret %>% na.omit(), densfun = "t")
    # fit_all <- fitdistr(ORATS_dailies_ETFs_top$logret %>% na.omit(), densfun = "t")
    # Normal VS Laplace, Laplace seems to be the winner
    ORATS_dailies_ETFs_top <- ORATS_dailies_ETFs_top %>% mutate(
        logliknorm_5 = dnorm(log_ret_5, mean_5, sd_5, log=TRUE),
        logliknorm_10 = dnorm(log_ret_10, mean_10, sd_10, log=TRUE),
        logliknorm_20 = dnorm(log_ret_20, mean_20, sd_20, log=TRUE),
        logliknorm_60 = dnorm(log_ret_60, mean_60, sd_60, log=TRUE),
        logliknorm_90 = dnorm(log_ret_90, mean_90, sd_90, log=TRUE),
        logliknorm_180 = dnorm(log_ret_180, mean_180, sd_180, log=TRUE),
        logliknorm_252 = dnorm(log_ret_252, mean_252, sd_252, log=TRUE),
        logliklaplace_5 = dlaplace(log_ret_5, mean_5, sd_5, log=TRUE),
        logliklaplace_10 = dlaplace(log_ret_10, mean_10, sd_10, log=TRUE),
        logliklaplace_20 = dlaplace(log_ret_20, mean_20, sd_20, log=TRUE),
        logliklaplace_60 = dlaplace(log_ret_60, mean_60, sd_60, log=TRUE),
        logliklaplace_90 = dlaplace(log_ret_90, mean_90, sd_90, log=TRUE),
        logliklaplace_180 = dlaplace(log_ret_180, mean_180, sd_180, log=TRUE),
        logliklaplace_252 = dlaplace(log_ret_252, mean_252, sd_252, log=TRUE)
    )
    df <- ORATS_dailies_ETFs_top  %>% ungroup %>% dplyr::select(tradeDate, logliknorm_5:logliklaplace_252) %>% 
        pivot_longer(-tradeDate) %>% separate(name, into=c("dist", "horizon"), sep = "_")
    df %>% mutate(value = value %>% replace(., is.infinite(.), NA), horizon = factor(horizon, levels=sort(unique(as.numeric(horizon))))) %>% 
        group_by(dist, horizon) %>% reframe(M=mean(value, na.rm=T), S=sd(value, na.rm=T)) %>% 
        ggplot(aes(horizon, y=M, ymin=M-S, ymax=M+S, group=dist, color=dist))+ geom_errorbar(linewidth=2,width=0.5, position = "dodge")
    
    ### Calculate RV forecast using current RV as predictors
    # Merge price data with core data       
    etf_screener <- read_csv("/home/marco/trading/Systems/Options/etf-screener-01-29-2026.csv", show_col_types = F)
    ORATS_core_ETFs <- read_parquet("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq") %>% 
        dplyr::filter(ticker %in% unique(etf_screener$Symbol)) %>% 
        dplyr::select(ticker, tradeDate, orFcst20d, orIvFcst20d, orFcstInf)
    ORATS_prices_ETFs <- read_parquet("/home/marco/trading/HistoricalData/ORATS/ORATS_prices.pq") %>% 
        group_by(ticker) %>% 
        filter(ticker %in% unique(etf_screener$Symbol) & median(stockVolume) > 1000000 & n()>2000) %>% 
        group_by(ticker, tradeDate) %>% arrange(tradeDate) 
    ORATS_data <- ORATS_prices_ETFs %>% left_join(ORATS_core_ETFs, by=c("ticker", "tradeDate"))
    # Calculate running means/sd
    ORATS_data <- ORATS_data %>% group_by(ticker) %>% 
        mutate(
            logret = c(NA, diff(log(clsPx))),
            mean_5 = runMean(logret, 5) * 5,
            mean_10 = runMean(logret, 10) * 10,
            mean_20 = runMean(logret, 20) * 20,
            mean_60 = runMean(logret, 60) * 60,
            mean_90 = runMean(logret, 90) * 90,
            mean_120 = runMean(logret, 120) * 180,
            mean_252 = runMean(logret, 252) * 252,
            sd_5 = runSD(logret, 5) * sqrt(252),
            sd_10 = runSD(logret, 10) * sqrt(252),
            sd_20 = runSD(logret, 20) * sqrt(252),
            sd_60 = runSD(logret, 60) * sqrt(252),
            sd_90 = runSD(logret, 90) * sqrt(252),
            sd_120 = runSD(logret, 120) * sqrt(252),
            sd_252 = runSD(logret, 252) * sqrt(252)
        )
    # Calculate prediction error for a given set of predictors
    pred_error <- function(y, x) {
        rmse(x, y)
    }
    columns <- c("orFcst20d","orIvFcst20d","orFcstInf",
                 "clsHv5d","orHv5d",
                 "clsHv10d","orHv10d", "clsHv20d","orHv20d",
                 "clsHv60d","orHv60d","clsHv90d","orHv90d",
                 "clsHv120d","orHv120d", "clsHv252d", "orHv252d")
    df <- ORATS_data %>% group_by(ticker) %>% 
        dplyr::mutate(
            dplyr::across(dplyr::all_of(columns), ~pred_error(lead(sd_5, 5), .x/100), .names = "{.col}_5"
            )
        )%>% 
        dplyr::mutate(
            dplyr::across(dplyr::all_of(columns), ~pred_error(lead(sd_10, 10), .x/100), .names = "{.col}_10"
            )
        )%>% 
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(columns), 
                ~pred_error(lead(sd_20, 20), .x/100),
                .names = "{.col}_20"
                )
        ) %>% 
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(columns), 
                ~pred_error(lead(sd_60, 60), .x/100),
                .names = "{.col}_60"
            )
        )%>% 
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(columns), 
                ~pred_error(lead(sd_90, 90), .x/100),
                .names = "{.col}_90"
            )
        )%>% 
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(columns), 
                ~pred_error(lead(sd_120, 120), .x/100),
                .names = "{.col}_120"
            )
        )%>% 
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(columns), 
                ~pred_error(lead(sd_252, 252), .x/100),
                .names = "{.col}_252"
            )
        )
    df_to_use <- df %>% filter(tradeDate > "2021-01-01") %>% ungroup
    df_plot <- df_to_use  %>% ungroup %>% dplyr::select(ticker, orFcst20d_5:orHv252d_252) %>% distinct %>% 
        pivot_longer(-ticker) %>% separate(name, into=c("X", "Y"), sep = "_")
    df_plot<- df_plot %>% mutate(value = value %>% replace(., is.infinite(.), NA), 
                       Y = factor(Y, levels=sort(unique(as.numeric(Y))))) %>% 
        group_by(X, Y) %>% reframe(M=mean(value, na.rm=T)) 
    df_plot %>% 
        ggplot(aes(X, y=M, group=Y, color=Y))+ geom_point(size=5) + scale_color_colorblind() + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
    
    # VRP autocorrelation
    ORATS_ETFs %>% filter(dtExM1==25 ) %>% group_by(ticker) %>% mutate(logVRPnext = lead(logVRP), Year=year(tradeDate)) %>% ggplot(aes(logVRP, logVRPnext)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~Year) 
    
    # Straddle autocorrelation
    stra <- ORATS_core %>% mutate(Year=year(tradeDate)) %>% group_by(ticker) %>% filter(n()>500) %>% 
        group_by(ticker, expiryDate1) %>% 
        reframe(S = mean(straRetM1, na.rm=T)) %>% group_by(ticker) %>% 
        reframe(corr = cor(S, lag(S,1), use="pairwise.complete.obs"))    
    ggplot(stra, aes(corr)) + geom_histogram()  + geom_vline(xintercept = 0)
    
    ### Any goodd straddle predictor?
    ORATS_core <- read_parquet("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq") %>% 
        arrange(tradeDate) %>% group_by(ticker) %>% dplyr::filter(median(avgOptVolu20d)>10000 & n()>500 )
    # ivHvXernRatio z-score
    ORATS_core %>% group_by(ticker) %>% 
        mutate(Z=runZscore(log(ivHvXernRatio) %>% replace_na(0), 252)) %>% 
        filter(dtExM1 %in% c(8, 16, 25, 33) & year(tradeDate)>=2021) %>% 
        ggplot(aes(Z, straRetM1)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~dtExM1) + ylim(c(-0.5,0.5))
    # ivHvXernRatio
    ORATS_core %>% group_by(ticker) %>% 
        mutate(Z=log(ivHvXernRatio)) %>% 
        filter(dtExM1 %in% c(8, 16, 25, 33) & year(tradeDate)>=2021) %>% 
        ggplot(aes(Z, straRetM1)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~dtExM1) + ylim(c(-0.5,0.5))
    # Previous VRP
    ORATS_core %>% group_by(ticker) %>% 
        mutate(Z=lag(logVRP %>% na.locf(na.rm=F) %>% EMA(200), 30)) %>% 
        filter(dtExM1 %in% c(8, 16, 25, 33) & year(tradeDate)>=2021) %>% 
        ggplot(aes(Z, straRetM1)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~dtExM1) + ylim(c(-0.5,0.5))
    # Momentum
    ORATS_core %>% group_by(ticker) %>% 
        mutate(Z=EMA(abs(retAtmIv/clsHv20d) %>% replace_na(0), 60)) %>% 
        filter(dtExM1 %in% c(8, 16, 25, 33) & year(tradeDate)>=2021) %>% 
        ggplot(aes(Z, straRetM1)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~dtExM1) + ylim(c(-0.5,0.5))
    # Price
    ORATS_core %>% group_by(ticker) %>% 
        mutate(Z=pxAtmIv %>% log) %>% 
        filter(dtExM1 %in% c(8, 16, 25, 33) & year(tradeDate)>=2021) %>% 
        ggplot(aes(Z, straRetM1)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~dtExM1) + ylim(c(-0.5,0.5))
    # Steepness
    ORATS_core %>% group_by(ticker) %>% 
        mutate(Z=log(iv30d/iv90d) %>% replace_na(0) %>% runZscore(252)  ) %>% 
        filter(dtExM1 %in% c(8, 16, 25, 33) & year(tradeDate)>=2021) %>% 
        ggplot(aes(Z, straRetM1)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~dtExM1) + ylim(c(-0.5,0.5))

    
    
}
    
# Regime
{
    library(depmixS4)
    library(patchwork)
    library(data.table)
    
    
    tickers_list <- c("SPY", "IWM", "QQQ", "USO", "UNG", "GLD", "SLV")
    tickers_list <- c("SPY")
    ORATS_core_ds <- open_dataset("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq") 
    features <- c(
        "IVpercentile", "IVHV_zscore" ,"IV_mom", "vol_of_Ivol", "steepness_pct", 
        "vol_of_Ivol_pct", "skew", "skew_zscore", "price_mom", "price"
    )
    
    predictors <-  c("IVpercentile",
                     "IV_mom",
                     "IVHV_zscore"
                     #"skew_zscore"
                     )
    
    w <- 252
    df <-  ORATS_core_ds %>% filter(ticker %in% tickers_list) %>% collect %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(
        date = tradeDate,
        IVpercentile = iv30d  %>% runPercentRank(w),  
        IV_mom = RSI2(iv30d %>% log, 10, maType="EMA") %>% EMA(3) %>% runPercentRank(w), 
        steepness_pct = pmax(pmin(log(iv10d / iv90d), 1), -1) %>% na.locf(na.rm=F) %>% runPercentRank(w),
        IVHV_zscore = log(iv30d / clsHv20d) %>% runPercentRank(w),
        skew = slope %>% na.locf(na.rm=F) ,
        skew_zscore = skew  %>% runPercentRank(w),
        vol_of_Ivol = ew_sd_roll(c(NA, diff(log(iv30d)))%>% na.locf(na.rm=F)) ,
        vol_of_Ivol_pct = vol_of_Ivol %>% runPercentRank(w),
        IVVVOL_ratio = log(iv30d / vol_of_Ivol) %>% runPercentRank(w),
        price = RSI2(pxAtmIv, 20, maType="EMA") %>% EMA(5) %>% as.vector(), 
        price_mom = price  %>% scale  %>% as.vector()
    ) %>% ungroup  %>% arrange(ticker, tradeDate) %>% 
        dplyr::select(date, ticker, dtExM1, iv30d, clsHv20d, logVRP, straRetM1, all_of(features)) %>% na.omit
    # mat <- as.matrix(df[,3:ncol(df)])
    # df[,3:ncol(df)] <- matrix(sample(mat), ncol=(ncol(df)-2))
    
    # Rescale?
    df_clean <- df %>%
        dplyr::select(date, iv30d,all_of(features), everything()) %>%
        drop_na(all_of(features))
    df_scaled <- df_clean
    df_scaled[features] <- scale(df_clean[features])
    
    # PCA
    pc <- prcomp(df_scaled[, predictors], center = TRUE)
    df_hmm <- df_clean
    df_hmm$PC1 <- pc$x[,1]
    df_hmm$PC2 <- pc$x[,2]
    
    # HMM
    n_states <- 4
    mod <- depmix(
        list(IVpercentile ~ 1,
             IV_mom ~ 1,
             IVHV_zscore ~ 1
             ),
        data = df_hmm,
        nstates = n_states,
        family = list(gaussian(),
                      gaussian(), 
                      gaussian()
                      )
    )
    hmm_fit <- fit(mod, verbose = FALSE)
    
    # Switch rate
    df_scaled$regime_hmm <- factor(posterior(hmm_fit)$state)
    switch_rate <- df_scaled %>%
        mutate(switch = regime_hmm != lag(regime_hmm)) %>%
        summarise(avg_switch_rate = mean(switch, na.rm = TRUE))
    print(switch_rate)
    
    # Regime duration 
    dt <- as.data.table(df_scaled)
    dt[, run_id := rleid(regime_hmm)]
    regime_duration <- dt[, .(avg_duration = mean(.N)), by = regime_hmm]
    print(regime_duration)
    
    # T matrix
    trans_mat <- matrix(getpars(hmm_fit)[(n_states+1):(n_states+n_states^2)], n_states, n_states, byrow = TRUE)
    rownames(trans_mat) <- colnames(trans_mat) <- paste0("Regime_", 1:n_states)
    p_tm <- ggcorrplot::ggcorrplot(trans_mat)
    trans_mat %>% round(2) %>% print 
    
    # PCA
    pc <- prcomp(df_scaled[predictors])
    p_pca <- ggplot(
        data.frame(PC1 = pc$x[,1], PC2 = pc$x[,2], regime = df_scaled$regime_hmm),
        aes(PC1, PC2, color = regime)
    ) +
        geom_point(alpha = 0.7) +
        labs(title = "Regime Separation (PCA projection)") +
        theme_minimal()  + scale_color_colorblind()
    
    # Regime profiles
    regime_profile <- df_clean %>%
        mutate(regime_hmm = df_scaled$regime_hmm) %>%
        group_by(regime_hmm) %>%
        summarise(across(all_of(predictors), mean), .groups = "drop") %>%
        pivot_longer(-regime_hmm)
    print(regime_profile %>% pivot_wider() %>% arrange(.[c(2,3)]))
    p_profile1 <- ggplot(regime_profile, aes(name, value, fill = regime_hmm)) +
        geom_col(position = "dodge") +
        labs(title = "Average Feature Values by HMM Regime") +
        theme_minimal()
    p_profile2 <- ggplot(regime_profile, aes(regime_hmm, value, fill = name)) +
        geom_col(position = "dodge") +
        labs(title = "Average Feature Values by HMM Regime") +
        theme_minimal()
    
    # VRP prediction
    df_scaled %>% group_by(ticker) %>% mutate(mean_VRP = mean(logVRP, na.rm=T)) %>%  group_by(ticker, regime_hmm) %>% 
        reframe(Mvrp = mean(logVRP-mean_VRP)) %>% 
        pivot_wider(id_cols =regime_hmm, names_from = ticker, values_from = Mvrp) %>% arrange(SPY) %>% print

    # Regime over time
    year_s <- 2025
    ticker_i <- df_scaled$ticker[1]
    p_time <- df_scaled %>% filter(year(date)>=year_s & ticker == ticker_i) %>%  
        ggplot(aes(date, iv30d, color=regime_hmm)) + geom_point() + scale_color_colorblind()
    p_iv <- df %>% filter(year(date)>=year_s & ticker == ticker_i)%>% ggplot(aes(date, IVpercentile)) + geom_line()
    p_mom <- df %>% filter(year(date)>=year_s & ticker == ticker_i)%>% ggplot(aes(date, IV_mom)) + geom_line()
    p_volofIvol <- df %>% filter(year(date)>=year_s & ticker == ticker_i)%>% ggplot(aes(date, vol_of_Ivol_pct)) + geom_line()
    p_steepness <- df %>% filter(year(date)>=year_s & ticker == ticker_i)%>% ggplot(aes(date, steepness_pct)) + geom_line()
    
    ( p_time /  p_volofIvol / p_mom)  /
        (p_tm | p_pca) #/
        #(p_profile1 | p_profile2)

}


{
    library(slider)
    ORATS_core_ds <- open_dataset("/home/marco/trading/HistoricalData/ORATS/ORATS_core.pq") 
    etfs_screener <- read_csv("/home/marco/trading/Systems/Options/etf-screener-weekly-options.csv", show_col_types = F) 
    etfs_list <- etfs_screener %>% group_by(Symbol) %>% reframe(Volume = mean(`Options Vol`, na.rm=T)) %>% 
        filter(Volume > 1000) %>% pull(Symbol)
    ORATS_ETFs <- ORATS_core_ds %>% filter(ticker %in% etfs_list) %>% arrange(ticker, tradeDate) %>% 
        collect  %>% group_by(ticker)%>% dplyr::filter(n()>1000) 
    # Correlation between ETFs and SPY
    spy_cor <- ORATS_ETFs %>% dplyr::select(tradeDate, ticker, retAtmIv) %>% pivot_wider(id_cols = tradeDate, names_from = ticker, values_from = retAtmIv) %>% dplyr::select(-tradeDate) %>% cor(use="pairwise.complete.obs") %>% {.[,"SPY", .drop=F]}
    w <- 252
    df <-  ORATS_ETFs %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(
        VRP_5 = log(iv10d / lead(orHv5d, 6)) %>% if_else(is.infinite(.), NA, .) %>% na.locf(na.rm=F),
        VRP_20 = log(iv30d / lead(clsHv20d, 21)) %>% if_else(is.infinite(.), NA, .) %>% na.locf(na.rm=F),
        VRP_20_m = mean(VRP_20), #runMean(VRP_20, 252),
        VRP_20_sd = sd(VRP_20), #runSD(VRP_20, 252),
        VRPzscore = ((VRP_20 - VRP_20_m) / VRP_20_sd) %>% cap_forecast(),
        VRPntile = ntile(VRP_20, 7),
        volatility = calculate_volatility(retAtmIv),
        retAtmIv_abs = abs(retAtmIv),
        IVpercentile = iv30d  %>% runPercentRank(w),  
        vol_of_Ivol = ew_sd_roll(c(NA, diff(log(iv30d)))%>% na.locf(na.rm=F)) ,
        iv_mom = log(iv10d / iv1yr),
        rv_mom = log(orHv5d / clsHv252d),
        iv_richness = log(iv30d / vol_of_Ivol),
        TR = trend_ratio(retAtmIv),
        VCR = runMax(retAtmIv^2, 20) / runSum(retAtmIv^2, 20)
    ) %>% ungroup  %>% arrange(ticker, tradeDate)
    
    predictors <- c("iv10d","iv30d","stkPxChng1wk","correlSpy1y","avgOptVolu20d","orHv5d","clsHv20d",
                    "ivHvXernRatio","ivEtfRatio","etfIvHvXernRatio","fexErn60_30","ffexErn60_30",
                    "slope","contango","deriv","iv_mom", "rv_mom", "iv_richness", "TR","VCR","retAtmIv_abs")
    selected <- 
    # Correlation between predictors
    df %>% filter(ticker %in% c("IWM", "QQQ", "SPY") & dtExM1 == 25) %>% ungroup %>% 
        dplyr::select(all_of(predictors), VRP_5, VRP_20) %>%
        mutate(across(where(is.numeric),~replace(., is.infinite(.), NA))) %>% cor(use = "pairwise.complete.obs") %>% corrplot::corrplot()
    # Single ticker
    df %>% dplyr::filter(ticker == "SPY") %>% 
        dplyr::select(all_of(predictors), tradeDate, ticker, VRP_5, VRP_20, retAtmIv_abs) %>% 
        mutate(target = ntile(VRP_20, 7)) %>% dplyr::select(-VRP_5, -VRP_20, -retAtmIv_abs) %>% 
        pivot_longer(-c(tradeDate, ticker, target)) %>% group_by(name) %>% 
        mutate(bin = ntile(value, 7)) %>% group_by(bin, name) %>%
        reframe(M = mean(target, na.rm=T), S = sd(target, na.rm=T)/sqrt(n()), n()) %>% na.omit %>%
        ggplot(aes(x=bin, y=M, ymin = M-S*2, ymax=M+S*2)) + geom_line(color="gray") + geom_point() + geom_errorbar(width=0.1) + facet_wrap(~name)
    # Complete pooling ntile
    df %>% group_by(ticker) %>%  mutate(target = VRPzscore) %>% 
        dplyr::select(all_of(predictors), tradeDate, ticker, target) %>% group_by(ticker) %>% 
        pivot_longer(-c(tradeDate, ticker, target)) %>% group_by(ticker, name) %>% 
        mutate(bin = ntile(value, 7)) %>% group_by(bin, name) %>%
        reframe(M = mean(target, na.rm=T), S = sd(target, na.rm=T)/sqrt(n()), n()) %>% na.omit %>%
        ggplot(aes(x=bin, y=M, ymin = M-S*2, ymax=M+S*2)) + geom_line(color="gray") + geom_point() + geom_errorbar(width=0.1) + facet_wrap(~name, scales="free")
    # Partial pooling ntile
    df %>% group_by(ticker) %>%  mutate(target = VRPzscore) %>% 
        dplyr::select(all_of(predictors), tradeDate, ticker, target) %>% 
        pivot_longer(-c(tradeDate, ticker, target)) %>% group_by(ticker, name) %>% 
        mutate(bin = ntile(value, 7)) %>% group_by(ticker, bin, name) %>%
        reframe(P = mean(target, na.rm=T)) %>% group_by(bin, name) %>% 
        reframe(M = mean(P, na.rm=T), S = sd(P, na.rm=T), n()) %>% na.omit %>% 
        ggplot(aes(x=bin, y=M, ymin = M-S, ymax=M+S)) + geom_line(color="gray") + geom_point() + geom_errorbar(width=0.1) + facet_wrap(~name, scales="free")
    # TR vs VCR (as predictors of VRP)
    df %>% group_by(ticker) %>% mutate(VRP = ntile(VRP_20, 7), TR_bin = ntile(TR, 7), VCR_bin = ntile(VCR, 7)) %>% 
        group_by(TR_bin, VCR_bin) %>%  reframe(M=mean(VRP, na.rm=T)) %>% na.omit %>% 
        ggplot(aes(x=TR_bin, y=VCR_bin, fill=M)) + geom_tile(color = "white") + scale_fill_viridis_c() 
    # Single predictor VS VRP on single ticker
    df %>% dplyr::filter(ticker == "SPY") %>% mutate(VRP = ntile(VRP_20, 5)/2, pred = ntile(iv30d, 5)) %>% filter(dtExM1 == 25) %>% 
        ggplot(aes(x=1, y=pred , size=5, color=VRP)) + geom_jitter(width = 0.1) + scale_color_distiller(palette = "RdBu")
    # Modelling
    df_model_ntile <- df %>% group_by(ticker) %>% 
        mutate(target = VRPntile, 
               pred1 = ntile(iv30d, 7)-4, 
               pred2 = ntile(retAtmIv_abs, 7)-4, 
               pred3 = ntile(TR, 7)-4, 
               pred4 = ntile(VCR, 7)-4)  
    df_model_zscore <- df %>% group_by(ticker) %>% 
        mutate(target = VRPzscore, 
               pred1 = runZscore(iv30d, 252) %>% cap_forecast(2), 
               pred2 = runZscore(retAtmIv_abs, 252) %>% cap_forecast(2), 
               pred3 = runZscore(TR, 252) %>% cap_forecast(2), 
               pred4 = runZscore(VCR, 252) %>% cap_forecast(2)) 
    fit_ntile <- brm(target ~ pred1 + pred2 + pred3 + pred4, df_model_ntile %>% filter(dtExM1==25))
    fit_zscore <- brm(target ~ pred1 + pred2 + pred3 + pred4, df_model_zscore %>% filter(dtExM1==25))
    # Predictions
    date <- "2026-01-02"
    new_data <-  df_model_zscore %>% filter(tradeDate == date) %>% ungroup %>% dplyr::select(ticker, pred1, pred2, pred3, pred4) 
    preds <- predict(fit_zscore, newdata = new_data)[,1]
    target_m <- df_model_zscore %>% filter(tradeDate == date) %>% ungroup %>% pull(VRP_20_m) 
    target_sd <- df_model_zscore %>% filter(tradeDate == date) %>% ungroup %>% pull(VRP_20_sd)
    future <- data.frame(ticker = new_data$ticker, VRP_mean = (exp(target_m)-1) * 100, VRP_pred = (exp(preds * target_sd + target_m) - 1) * 100)
    
    new_data <-  df_model_zscore %>% filter(ticker == "UVIX") %>% ungroup %>% dplyr::select(ticker, tradeDate, pred1, pred2, pred3, pred4) 
    preds <- predict(fit_zscore, newdata = new_data)[,1]
    target_m <- df_model_zscore %>% filter(ticker == "UVIX") %>% ungroup %>% pull(VRP_20_m) 
    target_sd <- df_model_zscore %>% filter(ticker == "UVIX") %>% ungroup %>% pull(VRP_20_sd)
    future <- data.frame(ticker = new_data$ticker, tradeDate = new_data$tradeDate, VRP_mean = (exp(target_m)-1) * 100, VRP_pred = (exp(preds * target_sd + target_m) - 1) * 100)
    future %>% ggplot(aes(tradeDate, VRP_mean)) + geom_line() + geom_line(aes(y = VRP_pred), color="black")
    
    # on strike backtest
    res_strangle <- backtest_short_strangle("SPY", 0.2, 20, 20, start_date = "2021-01-01")
    res_straddle <- backtest_short_strangle("SPY", 0.5, 20, 20, start_date = "2021-01-01")
    trades_strangle <- summarize_trades(res_strangle)
    trades_straddle <- summarize_trades(res_straddle)
    trades <- trades_strangle
    df_ticker <- df %>% dplyr::filter(ticker == "SPY") %>% mutate(pred = ntile(iv30d, 5))
    df_gex <- ORATS_GEX$SPX %>%  mutate(pred = ntile(-GEX_vl, 5))
    joined <- inner_join(trades, df_ticker, by=c("entry_date" = "tradeDate"))
    joined %>% ggplot(aes(pred %>% jitter, final_pnl_pct_notional)) + geom_point(alpha=0.5)
    joined %>% group_by(pred) %>% reframe(M = median(final_pnl_pct_notional, na.rm=T), S = mad(final_pnl_pct_notional, na.rm=T)/sqrt(n())) %>% 
        ggplot(aes(x = pred, y = M, ymin = M - S*2, ymax = M + S*2)) + geom_errorbar(width=0.2) + geom_point(aes(y=M))
    joined %>% dplyr::select(entry_date, final_pnl, pred) %>% pivot_longer(c(-entry_date, -final_pnl)) %>% 
        group_by(value = factor(value)) %>% reframe(tradeDate = entry_date,  eq = cumsum(replace_na(final_pnl,0))) %>% 
        ggplot(aes(tradeDate, eq, color=value, group=factor(value))) + geom_line(size=2) + scale_color_colorblind()
    joined %>% dplyr::select(entry_date, final_pnl, pred) %>% pivot_longer(c(-entry_date, -final_pnl)) %>% 
        group_by(value = factor(value)) %>% reframe(M = mean(final_pnl, na.rm=T) / sd(final_pnl, na.rm=T)* 16)
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



