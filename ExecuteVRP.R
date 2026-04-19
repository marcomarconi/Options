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
    library(magrittr)
    source("/home/marco/trading/Systems/Common/RiskManagement.R")
    source("/home/marco/trading/Systems/Common/Common.R")
    source("/home/marco/trading/Systems/Common/Indicators.R")
    theme_set(theme_bw(base_size = 20))
    
}



# Functions
{
    # Signals
    {
            IV_signal <- function(df, n=7, keep=F, cross_sectional = F, name = "IV_S") {
                df %>% group_by(ticker) %>% arrange(tradeDate) %>%
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(iv10d:iv1yr, ~ntile(., n), .names = "{.col}_S")) %>% 
                    mutate(!!name :=  rowSums(across(iv10d_S:iv1yr_S)) %>% ntile(n)) %>% 
                    {if(!keep) select(., -(iv10d_S:iv1yr_S)) else .}
            }
            HV_signal <- function(df, n=7, keep=F, cross_sectional = F, name = "HV_S") {
                df %>% group_by(ticker) %>% arrange(tradeDate) %>% 
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(orHv10d:clsHv252d, ~ntile(., n), .names = "{.col}_S")) %>% 
                    mutate(!!name :=  rowSums(across(orHv10d_S:clsHv252d_S)) %>% ntile(n))  %>% 
                    {if(!keep) select(., -(orHv10d_S:clsHv252d_S)) else .}
            }
            
            
            ivHv_signal <- function(df, n=7, keep=F, cross_sectional = F, name = "ivhv_S") {
                df %>% group_by(ticker) %>% arrange(tradeDate) %>% 
                    mutate(
                        logivhv = ivHvXernRatio,
                        ivhvRatio10d = EMA(logivhv, 5), 
                        ivhvRatio30d = EMA(logivhv, 20), 
                        ivhvRatio90d = EMA(logivhv, 60), 
                        ivhvRatio6m = EMA(logivhv, 180), 
                        ivhvRatio1yr = EMA(logivhv, 252))  %>% 
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(ivhvRatio10d:ivhvRatio1yr, ~ntile(., n), .names = "{.col}_S")) %>% 
                    mutate(!!name :=   rowSums(across(ivhvRatio10d_S:ivhvRatio1yr_S)) %>% ntile(n))  %>% 
                    {if(!keep) select(., -(ivhvRatio10d:ivhvRatio1yr), -(ivhvRatio10d_S:ivhvRatio1yr_S)) else .}
            }
            etf_signal  <- function(df, n=7, keep=F, cross_sectional = F, name = "etf_S") {
                df %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(naETFratio = na.locf(etfIvHvXernRatio), na.rm=F) %>% 
                    mutate(etf5 = EMA(naETFratio, 5), etf20 = EMA(naETFratio, 20), etf60 = EMA(naETFratio, 60), etf120 = EMA(naETFratio, 120), etf252 = EMA(naETFratio, 252)) %>% 
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(etf5:etf252, ~ntile(., n), .names = "{.col}_S")) %>% 
                    mutate(!!name := rowSums(across(etf5_S:etf252_S)) %>% ntile(n))  %>% 
                    {if(!keep) select(., -(etf5:etf252), -(etf5_S:etf252_S), -naETFratio) else .}
            }
            
            contango_signal <- function(df, n=7, keep=F, cross_sectional = F, name = "contango_S") {
                df %>% group_by(ticker) %>% arrange(tradeDate) %>% 
                    mutate(contango5 = EMA(contango, 5), contango20 = EMA(contango, 20), contango60 = EMA(contango, 60), contango120 = EMA(contango, 120), contango252 = EMA(contango, 252)) %>% 
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(contango5:contango252, ~ntile(., n), .names = "{.col}_S")) %>% 
                    mutate(!!name :=  rowSums(across(contango5_S:contango252_S)) %>% ntile(n))  %>% 
                    {if(!keep) select(., -(contango5:contango252), -(contango5_S:contango252_S)) else .}
            }
            
            volatility_signal  <- function(df, n=7, keep=F, cross_sectional = F, name = "vol_S") {
                df %>% group_by(ticker) %>% arrange(tradeDate) %>% 
                    mutate(vol5 = EMA(log(Volatility), 5), vol20 = EMA(log(Volatility), 20), vol60 = EMA(log(Volatility), 60), vol120 = EMA(log(Volatility), 120), vol252 = EMA(log(Volatility), 252)) %>% 
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(vol5:vol252, ~ntile(., n), .names = "{.col}_S")) %>% 
                    mutate(!!name :=  rowSums(across(vol5_S:vol252_S)) %>% ntile(n))  %>% {if(!keep) select(.,  -(vol5:vol252), -(vol5_S:vol252_S)) else .}
            }
            
            rsi_signal  <- function(df, n=7, keep=F, cross_sectional = F, name = "rsi_S") {
                df %>% group_by(ticker) %>% arrange(tradeDate) %>% 
                    mutate(rsi1 = RSI2(pxAtmIv, 5, maType = EMA), 
                           rsi2 = RSI2(pxAtmIv, 20, maType = EMA), 
                           rsi3 = RSI2(pxAtmIv, 60, maType = EMA), 
                           rsi4 = RSI2(pxAtmIv, 120, maType = EMA), 
                           rsi5 = RSI2(pxAtmIv, 252, maType = EMA)) %>%  
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(rsi1:rsi5, ~round(abs(cap_forecast(as.vector(scale(.)), cap = 2)*3)+1), .names = "{.col}_S")) %>% 
                    mutate(!!name :=  rowMeans(across(rsi1_S:rsi5_S)) %>% round(0))  %>% {if(!keep) select(., -(rsi1:rsi5), -(rsi1_S:rsi5_S)) else .}
            }
            fbf_signal  <- function(df, n=7, keep=F, cross_sectional = F, name = "fbf_S") {
                df  %>% group_by(ticker) %>% arrange(tradeDate) %>% 
                    mutate(fbf5 = EMA(fbfexErn60_30, 5), 
                           fbf20 = EMA(fbfexErn60_30, 20), 
                           fbf60 = EMA(fbfexErn60_30, 60), 
                           fbf120 = EMA(fbfexErn60_30, 180), 
                           fbf252 = EMA(fbfexErn60_30, 252)) %>% 
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(fbf5:fbf252, ~ntile(., n), .names = "{.col}_S")) %>% 
                    mutate(!!name := rowSums(across(fbf5_S:fbf252_S)) %>% ntile(n))  %>% {if(!keep) select(., -(fbf5:fbf252), -(fbf5_S:fbf252_S)) else .}
            }
            vvol_signal  <- function(df, n=7, keep=F, cross_sectional = F, name = "vvol_S") {
                df  %>% group_by(ticker) %>% arrange(tradeDate) %>% 
                    mutate(vvol5 = EMA(volOfIvol, 5), 
                           vvol20 = EMA(volOfIvol, 20), 
                           vvol60 = EMA(volOfIvol, 60), 
                           vvol120 = EMA(volOfIvol, 180), 
                           vvol252 = EMA(volOfIvol, 252)) %>%  
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(vvol5:vvol252, ~ntile(., n), .names = "{.col}_S")) %>%
                    mutate(!!name :=  rowSums(across(vvol5_S:vvol252_S)) %>% ntile(n))  %>% {if(!keep) select(., -(vvol5:vvol252), -(vvol5_S:vvol252_S)) else .}
            }
            VRP_signal  <- function(df, n=7, k=20, keep=F, cross_sectional = F, name = "VRP_S") { 
                df %>% group_by(ticker) %>% arrange(tradeDate) %>% mutate(lagVRP =  lag(VRP, k) %>% na.locf(na.rm=F)) %>% 
                    mutate(VRPm5 = EMA(lagVRP, 5), 
                           VRPm20 = EMA(lagVRP, 20), 
                           VRPm60 = EMA(lagVRP, 60), 
                           VRPm120 = EMA(lagVRP, 120), 
                           VRPm252 = EMA(lagVRP, 252)) %>% 
                    {if(cross_sectional) group_by(., tradeDate) else group_by(., ticker) }  %>% 
                    mutate(across(VRPm5:VRPm252, ~ntile(., n), .names = "{.col}_S")) %>% 
                    mutate(!!name := rowSums(across(VRPm5_S:VRPm252_S)) %>% ntile(n))  %>% {if(!keep) select(., -lagVRP, -(VRPm5:VRPm252), -(VRPm5_S:VRPm252_S)) else .}
            }
        }
    
}


# Parameters (maybe put them in a config file?)
{
    
    main_dir <- "/home/marco/trading/Systems/Options/ExecuteVRP/"
    core_dir <- paste0(main_dir, "Core")
    logs_dir <- paste0(main_dir, "Logs")
    orats_downloader <- paste0(main_dir, "orats_downloader_core.py")
    current_file_ETF <- paste0(main_dir, "ORATS_ETF.pq")
    current_file_STOCK <- paste0(main_dir, "ORATS_STOCK.pq")
    stocks_earnings <- paste0(main_dir, "stocks-earnings.csv")

}

# Signals weights
s_weights <- c(
    IV_S = 1/2, 
    HV_S = 1/2, 
    etf_S = 1/4, 
    etf_C = 1/4,
    ivHv_S = 1/4, 
    vvol_S = 1/2, 
    rsi_S = 1,
    fbf_S = 1/2, 
    fbf_C = 1/2,
    VRP_S = 1/4         
)

# Download data from ORATS
{
    system(paste("python3", orats_downloader))
}

# Merge it with current
{
    previous_data <- read_parquet(current_file_ETF) %>% arrange(tradeDate)
    last_day <- previous_data$tradeDate %>% unique %>% sort %>% tail(1)
    core_data <- open_csv_dataset(core_dir) %>% collect %>%  arrange(tradeDate)
    core_data <- core_data %>% filter(ticker %in% unique(previous_data$ticker) & tradeDate > last_day)
    core_data <- core_data[colnames(previous_data)]
    current_data <- rbind(previous_data, core_data) %>% arrange(tradeDate)
    current_data <- current_data %>% group_by(ticker) %>% arrange(tradeDate) %>% 
        mutate(retAtmIv = replace_na(c(0, diff(log(pxAtmIv))), 0),retAtmIv = case_when(abs(retAtmIv) > 0.1 ~ 0, TRUE ~ retAtmIv), 
               Volatility = calculate_volatility(retAtmIv),
               VRP = (log(exErnIv30d / lead(orHvXern20d, 20))) %>% replace(is.infinite(.), NA), 
               laggedVRP = lag(VRP, 20), 
               VRP20 = SMA(na.locf(laggedVRP, na.rm=F), 20),
               VRP60 = SMA(na.locf(laggedVRP, na.rm=F), 60),
               VRP120 = SMA(na.locf(laggedVRP, na.rm=F), 120),
               VRP250 = SMA(na.locf(laggedVRP, na.rm=F), 250),
               VRP500 = SMA(na.locf(laggedVRP, na.rm=F), 500),
               rvPctile1y = runPercentRank(clsHv20d, 252)*100,
               .after = tradeDate,
               ) %>% ungroup %>% 
        relocate(avgOptVolu20d, .after = tradeDate)
}

# Get signals
{
    current_data <- current_data %>% 
        IV_signal(cross_sectional = F, name = "IV_S")  %>%
        etf_signal(cross_sectional = F, name = "etf_S") %>% mutate(etf_S = 7-etf_S+1) %>% 
        etf_signal(cross_sectional = T, name = "etf_C") %>% mutate(etf_C = 7-etf_C+1) %>% 
        HV_signal(cross_sectional = F, name = "HV_S") %>% 
        ivHv_signal(cross_sectional = F, name = "ivHv_S") %>% mutate(ivHv_S = 7-ivHv_S+1) %>% 
        vvol_signal(cross_sectional = F, name = "vvol_S") %>% 
        rsi_signal(cross_sectional = F, name = "rsi_S") %>% mutate(rsi_S = 7-rsi_S+1) %>% 
        fbf_signal(cross_sectional = F, name = "fbf_S") %>% mutate(fbf_S = 7-fbf_S+1) %>% 
        fbf_signal(cross_sectional = T, name = "fbf_C") %>% mutate(fbf_C = 7-fbf_C+1) %>% 
        VRP_signal(cross_sectional = F, name = "VRP_S") %>% mutate(VRP_S = 7-VRP_S+1)
    
    current_data <- current_data %>% mutate(across(matches("(_S|_C)$"), ~((.-4))))  %>%  relocate(matches("(_S|_C)$"), .after=1) 
    #s_weights[] <- 1 /  length(s_weights) # identical weights
    s_weights <- s_weights / sum(s_weights)    
    current_data <- current_data %>%
        mutate(Signal = 
                   IV_S * s_weights["IV_S"] +
                   HV_S * s_weights["HV_S"]+
                   etf_S * s_weights["etf_S"]+
                   etf_C * s_weights["etf_C"]+
                   ivHv_S * s_weights["ivHv_S"]+
                   vvol_S * s_weights["vvol_S"]+
                   rsi_S * s_weights["rsi_S"]+
                   fbf_S * s_weights["fbf_S"]+
                   fbf_C * s_weights["fbf_C"]+
                   VRP_S * s_weights["VRP_S"],
               Signal = round(Signal*100,1), avgVRP = rowMeans(across(VRP20:VRP500))*100, .after = 1)
    last_data <- current_data %>% arrange(tradeDate) %>% group_by(tradeDate) %>%  group_split() %>%  tail(1) %>% .[[1]]
    earnings <- read_csv(stocks_earnings, show_col_types = FALSE) %>% dplyr::rename(ticker = Symbol, earnings_date = `Latest Earnings`) %>% dplyr::select(ticker, earnings_date) %>% mutate(earnings_date = as.Date(earnings_date))
    last_data <- full_join(last_data, earnings, by="ticker") %>% mutate(days_to_earnings = as.numeric(earnings_date - tradeDate), .after=tradeDate)
    
    last_date <-  unique(last_data$tradeDate)
    if(last_date != today()) {
        warning(paste("Last date does not correspond to today in ", last_date))
    }
    now_string <- paste0(gsub("-| |:", "", now()), ".csv")
    write_csv(last_data, paste0(logs_dir, now_string))
    
    last_data_reduced <- last_data %>% filter(avgOptVolu20d > 1000) %>% ungroup
    
    # Plots
    last_data_reduced %>% ungroup %>% select(matches("(_S|_C)$")) %>% cor(use="pairwise.complete.obs") %>% abs %>% corrplot::corrplot(order="hclust", method = "number")
    #last_data_reduced %>% ungroup %>% select(ticker, matches("(_S|_C)$")) %>% pivot_wider(names_from = ticker, ) %>% cor(use="pairwise.complete.obs") %>% abs %>% corrplot::corrplot(order="hclust", method = "number")
    ggplot(last_data_reduced, aes(laggedVRP, ivPctile1y,label=ticker)) + geom_label()
    ggplot(last_data_reduced, aes(log(iv30d), -log(fbfexErn60_30), label=ticker)) + geom_label()
    ggplot(last_data_reduced, aes(log(ivHvXernRatio), log(ivEtfRatio), label=ticker)) + geom_label()
    ggplot(last_data_reduced, aes(rvPctile1y, laggedVRP, label=ticker)) + geom_label()
}

