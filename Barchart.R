# Barchart_vols_stocks: barchart volatility data, created by joining all the single files
# Stocks_Info: Fundamental data about stocks from barchart
# ETFs_Info: Fundamental data about ETFs from barchart


# Yahoo Load assets prices 
{
    Price_data_stocks <- list()
    #Price_data_ETFs <- list()
    Returns <- list()
    type <- "Stocks/"# Stocks or ETFs
    dir <- paste0(main_dir, "YahooPrice/", type)
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
                     HV3 = runSD(ReturnPrice, period) * sqrt(period), 
                     RV = lead(HV1, vol_shift),
                     Momentum1 = EMA(ReturnPrice, vol_shift)/runSD(ReturnPrice, vol_shift),
                     Momentum2 = RSI(na.locf(AdjClose, na.rm=F), vol_shift, maType=EMA)/50-1
        )
        df <- arrange(df, Date)
        Price_data_stocks[[symbol]] <- df
    }
}

# Yahoo Load single file assets prices  
{
    vol_shift <- 21; period <- 252
    stock_price_data <- read_csv("/home/marco/trading/HistoricalData/Barchart/Options/stock_price_data.csv")
    stock_price_data <- 
        stock_price_data %>% rename(AdjClose = `Adj Close`) %>% arrange(Date) %>% 
        group_by(Symbol) %>% 
        mutate(
            ReturnPrice = log(AdjClose / lag(AdjClose)), 
            ReturnPrice = replace_na(ReturnPrice, 0),
            HV1 = runSD(ReturnPrice, vol_shift) * sqrt(period), 
            HV2 = calculate_volatility(ReturnPrice, long_span = period, short_span = vol_shift, period = period),
            RV = lead(HV1, vol_shift),
            Momentum1 = EMA(ReturnPrice, 21)/runSD(ReturnPrice, 21),
            Momentum2 = RSI(na.locf(AdjClose, na.rm=F), 21, maType=EMA)/50-1
        )
    stock_price_data <- arrange(stock_price_data, Date)
}

# Barchart historical volatility data (scrape from pages like https://www.barchart.com/stocks/quotes/AAPL/options-history)
{
    # Load barchart volatility prices, and clean that data
    {
        main_dir <- "/home/marco/trading/HistoricalData/Barchart/Options/"
        type <- "Stocks/" # Stocks or ETFs
        Options_data <- list()
        dir <- paste0(main_dir, "BarchartVolatility/", type)
        for(f in list.files(dir, pattern = "*.csv")){
            symbol <- sub(".csv", "", f)
            df <- fread(paste0(dir, f)) %>% as.data.frame
            df$Date <- as.Date(df$Date)
            # Columns after the 7th randomly appears
            df <- df[,1:9]
            colnames(df) <- c("Date", "ImpVol", "IVChg", "IVRank", "IVPctl", "PCVol", "OptionsVol", "PCOI","TotalOI")
            # Remove percentages symbols and commas
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
                #print(n)
                if(nrow(options_data[[n]]) < period | is.null(price_data[[n]]) ) return(NULL)
                else {
                    full_join(options_data[[n]], price_data[[n]], by="Date") %>% mutate(Symbol=n, .after=Date) %>% arrange(Date) %>% 
                        rename(IV = ImpVol) %>%        
                        mutate(
                            IV = case_when(IV == 0 ~ NA, TRUE ~ IV),
                            IV = IV / 100,
                            IVChg = IVChg / 100,
                            VRP = IV - RV,
                            VRPlog = log(IV / RV),
                            VRPadj = VRPlog / HV1
                        ) 
                }
            }
            )
            return(a)
        }
        # Call this for ETFs
        {
            Barchart_vols_ETFs <- load_price_data(Options_data, Price_data_ETFs)
            names(Barchart_vols_ETFs) <- names(Options_data)
            Barchart_vols_ETFs <- do.call(rbind, Barchart_vols_ETFs) %>% inner_join(., ETFs_Info, keep=FALSE, by="Symbol")
            write_csv(Barchart_vols_ETFs, "/home/marco/trading/HistoricalData/Barchart/Options/Barchart_vols_ETFs.csv")
        }
        # Call this for Stocks
        {
            Barchart_vols_stocks <- load_price_data(Options_data, Price_data_stocks)
            names(Barchart_vols_stocks) <- names(Options_data)
            Barchart_vols_stocks <- do.call(rbind, Barchart_vols_stocks) %>% inner_join(., Stocks_Info, keep=FALSE, by="Symbol")
            write_csv(Barchart_vols_stocks, "/home/marco/trading/HistoricalData/Barchart/Options/Barchart_vols_stocks.csv")
        }
    }
}

# Barchart historical single option data (the ones individually scraped from pages like https://www.barchart.com/stocks/quotes/SPY%7C20200814%7C321.00C/price-history/historical)
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


# Barchart vol data general observations
{
    # Load saves Stocks and ETFs data
    Barchart_vols_stocks <- read_csv("/home/marco/trading/HistoricalData/Barchart/Options/Barchart_vols_stocks.csv")
    Dolthub_vols <- read_csv("/home/marco/trading/HistoricalData/Barchart/Options/Dolthub_vols.csv")
    Dolthub_vols_chains <- read_csv("/home/marco/trading/HistoricalData/Barchart/Options/Dolthub_vols_chains.csv")
    # Barchart_vols_stocks and Dolthub_vols_chains (calculated by me using option chains data) seems more similar than Barchart_vols_stocks and Dolthub_vols, maybe because with Dolthub_vols I used the already provided "hv_current" see above and not my own volatility
    #ETFs <- readRDS("/home/marco/trading/Systems/Options/ETFs.RDS")
    
    ## ETFs
    {
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
    }
    
    ## Stocks
    # From Barhchart data, Keep only Stocks with < 20% NAs in ImpVol betweem 2021 and 2024 (most data have NAs in the first years)
    stocks <- Barchart_vols_stocks %>% group_by(Symbol) %>% filter(Date > "2020-01-01") %>% filter(sum(is.na(IV))/length(IV) < 0.1) %>% ungroup %>% filter(VRPlog < Inf)
    # Recent VRPs
    Barchart_vols_ETFs %>% filter(Date > "2022-01-01") %>%  group_by(Symbol) %>% reframe(M=median(VRPlog, na.rm=T), S=mad(VRPlog, na.rm=T)/sqrt(n())*2, N=n()) %>% View
    # VRP by class
    classv <- "Sector"
    stocks %>% group_by(Symbol) %>% reframe(VRP=median(VRPlog, na.rm=T), Class=first(!!rlang::sym(classv) )) %>% ggplot(aes(y=VRP, x=Class)) + geom_boxplot() + geom_hline(yintercept = 0)+ theme(axis.text.x = element_text(angle = 45))
    stocks %>% mutate(Decile = ntile((`Market Cap`), 5)) %>% group_by(Symbol, Decile) %>% reframe(VRP=median(VRPlog, na.rm=T)) %>% ggplot(aes(y=VRP, x=Decile, group=Decile)) + geom_boxplot() + geom_hline(yintercept = 0)+ theme(axis.text.x = element_text(angle = 45)) 
    stocks %>% mutate(Decile = ntile((`Market Cap`), 5)) %>% group_by(Symbol, Decile, Year=year(Date)) %>% reframe(VRP=median(VRPlog, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(VRP, na.rm=T), S=sd(VRP, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    # VRP by price and returns
    stocks %>% mutate(Decile = ntile(AdjClose, 5)) %>% group_by(Symbol, Decile, Year=year(Date)) %>% reframe(VRP=median(VRPlog, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(VRP, na.rm=T), S=sd(VRP, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    stocks %>% mutate(Decile = ntile(ReturnPrice %>% abs %>% na.locf0() %>% SMA(20) %>% lag, 5)) %>% group_by(Symbol, Decile, Year=year(Date)) %>% reframe(VRP=median(VRPlog, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(VRP, na.rm=T), S=sd(VRP, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    # VRP by some vol prediction (like IV, HV1, log(IV/HV1), Momentum...)
    stocks %>% group_by(Symbol) %>% mutate(Decile = ntile(lag(IV, 1), 10)) %>% 
        group_by(Symbol, Decile, Year=year(Date))  %>% reframe(VRP=median(VRPlog, na.rm=T))  %>%
        group_by(Decile, Year) %>% reframe(M=mean(VRP, na.rm=T), S=sd(VRP, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    # VRP by vol of vol
    stocks %>% group_by(Symbol) %>% mutate(VolVol = runSD(log(HV1/lag(HV1)) %>% na.locf(na.rm=F), 21) %>% lag) %>% 
        mutate(Decile = ntile(VolVol, 10)) %>%  group_by(Symbol, Decile, Year=year(Date)) %>% reframe(VRP=median(VRPlog, na.rm=T)) %>% 
        group_by(Decile, Year)  %>% reframe(M=mean(VRP, na.rm=T), S=sd(VRP, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()+ facet_wrap(~Year)    
    # VRP by week day
    stocks %>% group_by(Symbol, Decile=wday(Date), Year=year(Date)) %>% reframe(VRP=median(VRPlog, na.rm=T))  %>% 
        group_by(Decile, Year) %>% reframe(M=mean(VRP, na.rm=T), S=sd(VRP, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    # VRP by cost (dolthub data only)
    stocks %>% mutate(Cost = (bid-ask)/bid, Decile = ntile(Cost, 5)) %>% group_by(Symbol, Decile, Year=year(Date)) %>% reframe(VRP=median(VRPlog, na.rm=T))  %>% group_by(Decile, Year) %>% reframe(M=mean(VRP, na.rm=T), S=sd(VRP, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year)
    # VRP by SPY correlation
    stocks %>% ungroup %>% full_join(., SPY_correlations, by="Symbol")%>% mutate(Decile = ntile(corReturn, 5)) %>% group_by(Symbol, Decile, Year=year(Date)) %>% reframe(VRP=mean(VRPlog, na.rm=T)) %>% filter(!is.nan(VRP)) %>% group_by(Decile, Year) %>% reframe(M=mean(VRP), S=sd(VRP)/sqrt(n())*2, N=n()) %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar() + facet_wrap(~Year) 
    # VRP by higher moments (not much here)
    stocks  %>% group_by(Symbol) %>% mutate(Skew = rollapply(ReturnPrice, width=100, skew,  fill=NA, align="right") %>% lag) %>% 
        mutate(Decile = ntile(Skew, 10)) %>%  group_by(Symbol, Decile, Year=year(Date)) %>% reframe(VRP=median(VRPlog, na.rm=T)) %>% 
        group_by(Decile, Year) %>% reframe(M=mean(VRP, na.rm=T), S=sd(VRP, na.rm=T)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()+ facet_wrap(~Year)     
    stocks %>% group_by(Symbol) %>% mutate(CR = carver_ratio(Return, n=21),  Decile = ntile(lag(CR), 10)) %>% group_by(Decile) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    stocks %>% group_by(Symbol) %>% mutate(CR = carver_ratio(Return, n=21),  Decile = ntile(lag(CR), 10)) %>% group_by(Decile, Sector) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()  + facet_wrap(~Sector, scales = "free") 
    # Using ad hod clustering (see stocks example for clustering)
    stocks %>% full_join(clusters, by="Symbol") %>% group_by(Symbol) %>% group_by(Date, Cluster) %>% reframe(M=mean(VRPlog), S=sd(VRPlog)/sqrt(n())*2, N=n()) %>% na.omit %>% ggplot(aes(x=Decile, ymin=M-S, ymax=M+S)) + geom_errorbar()
    # IV pacf autocorrelation by market cap (or something else)
    a <- stocks %>% mutate(Decile = ntile((`Market Cap`), 5)) %>% group_by(Decile) %>% reframe(Pacf=unlist(as.data.frame(pacf(IV)$acf)))
    a$x <- rep(1:55,5); a$Pacf <- as.vector(a$Pacf)
    ggplot(a) + geom_bar(aes(x, Pacf), stat="identity") + facet_wrap(~Decile)
}
