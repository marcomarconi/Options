suppressMessages(library(tidyverse))
suppressMessages(library(data.table))
suppressMessages(library(lubridate))
suppressMessages(library(TTR))
suppressMessages(library(roll))
suppressMessages(library(zoo))
suppressMessages(library(optparse))

source("/home/marco/trading/Systems/Common/RiskManagement.R")

# Functions
{
# load all scraped file from barchart (using bc-utils) and create a constant maturity contract from the front two contracts
constant_maturity <- function(dir, expiry_file, contract="VI", year_str=6:7, month_str=8:9){
    month_code <- setNames(1:12,c("f", "g", "h", "j", "k", "m", "n", "q", "u", "v", "x", "z"))
    # the file containing the expity dates
    expiry <- read_csv(expiry_file, show_col_types = F) %>% rename(Date=`Exp Date`)
    # load all scraped file from barchart in bc-utils format ("VI_YYYYMMDD.csv")
    files <- list.files(dir, ".csv")
    dfs <- list()
    for(f in files)
        dfs[[f]] <- fread(paste0(dir, f)) %>% rename(Date=Time) %>% mutate(Name=f, Date=as.Date(Date)) %>% select(Date, Name, Close) 
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
        expiry_date <- as.Date(expiry[expiry$Symbol==n,]$Date)
        dfs[[n]] <- mutate(dfs[[n]],
                           Symbol = n,
                           Expiry = expiry_date,
                           DTE = abs(as.numeric(Date - expiry_date)) - 1,
                           Dist = abs(DTE - 30),
                           Return = log(Close/lag(Close))
                           )
    }
    # concatenate the contract and calculate the constant maturity
    dfs_concat <- do.call(rbind, dfs) %>% arrange(Date) %>% filter(DTE >= 0) %>%  mutate(Date=as.Date(Date)) %>%  group_by(Date) %>% mutate(Contract=row_number()) 
    constant_maturity_contract <- dfs_concat %>% mutate(Date=as.Date(Date)) %>%  group_by(Date) %>% 
        reframe(Symbol1=dplyr::nth(Symbol, 1), Symbol2=dplyr::nth(Symbol, 2), 
                Expiry1=dplyr::nth(Expiry, 1), Expiry2=dplyr::nth(Expiry, 2), 
                DTE1=dplyr::nth(DTE, 1), DTE2=dplyr::nth(DTE, 2), 
                VX1=dplyr::nth(Close, 1), VX2=dplyr::nth(Close, 2),
                Return1=dplyr::nth(Return, 1), Return2=dplyr::nth(Return, 2), 
                Dist1=dplyr::nth(Dist, 1), Dist2=dplyr::nth(Dist, 2)
                ) %>% 
        mutate(W = ((Dist1 + Dist2) - Dist1) / (Dist1 + Dist2)) %>% 
        mutate(W = case_when(DTE1 >= 30  ~ 1, TRUE ~ W)) %>% 
        mutate(CM = W*VX1 + (1-W)*VX2, CMr = W * Return1 + (1-W) * Return2, VX1r=Return1) 
    return(constant_maturity_contract)
}
}

# Parameters 
{
    main_dir <- "/home/marco/trading/Systems/Options/ExecutionVIX/"
    expity_file <- paste0(main_dir, "VI_expiry.csv")
    scrape_barchart_dir <- paste0(main_dir, "Scrape_barchart/")
    scrape_yahoo_dir <- paste0(main_dir, "Scrape_yahoo/")
    logs_dir <- paste0(main_dir, "Logs/")
    plots_dir <- paste0(main_dir, "Logs/Plots/")
    scrape_barchart_script <- paste0(main_dir, "Download_Barchart.py")
    scrape_yahoo_script <- paste0(main_dir, "Download_Yahoo.sh")
    scrape_yahoo_file <- paste0(main_dir, "Instruments_yahoo.txt")
    target_vol <- 0.25
    cutoff <- -1.5
    dry_run <- FALSE
}

{
    # Read command arguments
    option_list = list(
        make_option(c("-c", "--capital"),  type="double", help="Account Capital."),
        make_option(c("-f", "--fx"),  default=1.0, type="double", help="FX."),
        make_option(c("-d", "--dryrun"), action="store_true", default=FALSE, help="Do not write any file.")
    );
    opt_parser = OptionParser(option_list=option_list);
    opt = parse_args(opt_parser);
    capital <- opt$capital
    fx <- opt$fx
    dry_run <- opt$dryrun
    
    print(paste("Capital:", capital, "Target Volatility:", target_vol, "Dry run:", dry_run))
    
    # Set general variables and the current directory
    today_string <- gsub("-", "", today())
    now_string <- gsub("-| |:", "", now())
    setwd(main_dir)
    
    # Create constant maturity contract
    print("Downloading contracts from Barchart and create constant maturity contract.")
    if(!dry_run)
        system(paste("python3", scrape_barchart_script, scrape_barchart_dir))
    CM <- constant_maturity(scrape_barchart_dir, expity_file)
    
    # Load indices
    print("Downloading indices from Yahoo and load.")
    if(!dry_run)
        system(paste("bash", scrape_yahoo_script, scrape_yahoo_dir, scrape_yahoo_file))
    VIX <- read_csv(paste0(scrape_yahoo_dir, "^VIX.csv"), show_col_types = F) %>% select(Date, `Adj Close`) %>% rename(VIX=`Adj Close`)
    VIX3M <- read_csv(paste0(scrape_yahoo_dir, "^VIX3M.csv"), show_col_types = F) %>% select(Date, `Adj Close`) %>% rename(VIX3M=`Adj Close`)
    VVIX <- read_csv(paste0(scrape_yahoo_dir, "^VVIX.csv"), show_col_types = F) %>% select(Date, `Adj Close`) %>% rename(VVIX=`Adj Close`)
    
    # Merge all the data
    total <- Reduce(function(...) full_join(..., by = "Date"), list(CM, VIX, VIX3M, VVIX)) %>% arrange(Date) %>% na.omits
    total <- mutate(total, 
                    Premium = log(CM / VIX3M), 
                    VIX_Basis = log(VIX / VIX3M), 
                    VX_basis = log(VX1 / VX2), 
                    logVVIX = log(VVIX), 
                    logVIX = log(VIX), 
                    Return = VX1r, leadReturn = lead(Return),
                    Volatility = calculate_volatility(VX1r))
    total <- mutate(total, 
                    Premium = na.locf(Premium, na.rm = F), 
                    Premium_zscore = (Premium - runMean(Premium, 252)) / runSD(Premium, 252),
                    Forecast = case_when(Premium_zscore <= cutoff ~ 1, Premium_zscore > cutoff ~ -1, TRUE ~ 0),
                    Exposure = capital * target_vol / Volatility,
                    PositionOptimal = (Exposure * fx * Forecast) / (1 * VX1),
                    )
    total <- arrange(total, desc(Date))
    if(!dry_run)
        write_csv(total, paste0(logs_dir, "/", now_string, ".POSITIONS.csv"))
    print(total, width=Inf)
    
}
