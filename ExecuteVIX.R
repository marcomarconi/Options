library(tidyverse)
library(data.table)
library(lubridate)

# Functions
{
# load all scraped file from barchart (using bc-utils) and create a constant maturity contract from the front two contracts
constant_maturity <- function(dir, expiry_file, contract="VI", year_str=6:7, month_str=8:9){
    month_code <- setNames(1:12,c("f", "g", "h", "j", "k", "m", "n", "q", "u", "v", "x", "z"))
    # the file containing the expity dates
    expiry <- read_csv(expiry_file) %>% rename(Date=`Exp Date`)
    # load all scraped file from barchart in bc-utils format ("VI_YYYYMMDD.csv")
    files <- list.files(scrape_dir, ".csv")
    dfs <- list()
    for(f in files)
        dfs[[f]] <- fread(paste0(scrape_dir, f)) %>% rename(Date=Time) %>% mutate(Name=f, Date=as.Date(Date)) %>% select(Date, Name, Close) 
    # sort the contracts chronologically using the first date as reference
    start_dates <- lapply(dfs, function(x) head(x, 1)$Date) %>% unlist %>% sort
    dfs <- dfs[names(start_dates)]
    # recreate the contract name as used by barchart, for example "VI_20230800" to "VIQ23"
    months_str <- sapply(names(dfs), function(x) names(month_code)[as.numeric(substr(x, month_str[1],  month_str[2]))]) %>% toupper()
    years_str <- sapply(names(dfs), function(x) substr(x, year_str[1],  year_str[2]))
    contracts_names <- data.frame(C=contract, M=months_str, Y=years_str) %>% unite("X", C:Y, sep = "") %>% unlist
    names(dfs) <- contracts_names
    # iterate over the contract and set the DTE using the expiry date
    for(n in names(dfs)){
        if(!n %in% expiry$Symbol)
            stop(paste("Symbol", n,  "not present in the expiry file."))
        dfs[[n]]$Symbol <- n
        expiry_date <- as.Date(expiry[expiry$Symbol==n,]$Date)
        dfs[[n]]$DTE <- abs(as.numeric(dfs[[n]]$Date - expiry_date))
        dfs[[n]]$Dist <- abs(dfs[[n]]$DTE - 30)
    }
    # concatenate the contract and calculate the constant maturity
    dfs_concat <- do.call(rbind, dfs) %>% arrange(Date)
    constant_maturity_contract <- dfs_concat %>% mutate(Date=as.Date(Date)) %>%  group_by(Date) %>% 
        reframe(Symbol1=dplyr::nth(Symbol, 1), Symbol2=dplyr::nth(Symbol, 2), DTE1=dplyr::nth(DTE, 1), DTE2=dplyr::nth(DTE, 2), Close1=dplyr::nth(Close, 1), Close2=dplyr::nth(Close, 2), Dist1=dplyr::nth(Dist, 1), Dist2=dplyr::nth(Dist, 2)) %>% 
        mutate(W= ((Dist1 + Dist2) - Dist1) / (Dist1 + Dist2)) %>% mutate(CM = W*Close1 + (1-W)*Close2) 
    return(constant_maturity_contract)
}
}

# Parameters 
{
    main_dir <- "/home/marco/trading/Systems/Options/ExecutionVIX/"
    scrape_dir <- paste0(main_dir, "Scrape/")
    logs_dir <- paste0(main_dir, "Logs/")
    plots_dir <- paste0(main_dir, "Logs/Plots/")
    scrape_barchart <- paste0(main_dir, "Download_Barchart.py")
    scrape_yahoo <- paste0(main_dir, "Download_Yahoo.sh")
    target_vol <- 0.20
    dry_run <- FALSE
}

