# Simulate Options Strategies

{
    source("/home/marco/trading/Systems/Options//OptionsCommon.R")
    source("/home/marco/trading/Systems/Options//ChatGPT_sim.R")
    nyse <- timeDate::holidayNYSE(2000:year(Sys.Date()) +1)
    create.calendar(name='NYSE', holidays=nyse, weekdays=c('saturday', 'sunday'))
    bizdays.options$set(default.calendar='NYSE')
}

# Check options portfolio
{
    portfolio <- read_csv("/home/marco/trading/Systems/Options/Portfolio_Strangles.csv", show_col_types = F)
    for(i in 1:nrow(portfolio)) {
        x <- list(portfolio[i,])[[1]]
        x[["start_date"]] <- today()
        expiry_days <- as.numeric(as.Date(x[["end_date"]]) -  as.Date(x[["start_date"]]))
        trading_days <- bizdays(x[["start_date"]], x[["end_date"]], "NYSE")
        trading_days <- ifelse(trading_days == 1, 2, trading_days)
        gbm <- gbm_vec(nsim = 10000, t = trading_days, mu = 0, sigma = x[["sigma"]], S0 = x[["S0"]], dt = 1./252, only_terminal = TRUE)
        res <- option_expiration_profit(gbm, X=c(x[["X_call"]], x[["X_put"]]), 
                                 type = c("call", "put"), pos = c(x[["pos"]],x[["pos"]]), 
                                 premium = c(x[["px_call"]],x[["px_put"]]),
                                 expiry_days =  expiry_days, verbose = F)
        print(paste(x[["ticker"]], round(mean(res)/sd(res)*sqrt(expiry_days/365),2)))
    }
    portfolio <- read_csv("/home/marco/trading/Systems/Options/Portfolio_Calendars.csv", show_col_types = F)
    for(i in 1:nrow(portfolio)) {
        x <- list(portfolio[i,])[[1]]
        x[["start_date"]] <- today()
        expiry_days_front <- as.numeric(as.Date(x[["end_date_front"]]) -  as.Date(x[["start_date"]]))
        expiry_days_back <- as.numeric(as.Date(x[["end_date_back"]]) -  as.Date(x[["start_date"]]))
        trading_days <- bizdays(x[["start_date"]], x[["end_date_front"]], "NYSE")
        trading_days <- ifelse(trading_days == 1, 2, trading_days)
        gbm <- gbm_vec(nsim = 10000, t = trading_days, mu = 0, sigma = x[["sigma"]], S0 = x[["S0"]], dt = 1./252, only_terminal = TRUE)
        res <- sim_calendar_profit(gbm, S0 = x[["S0"]], X_front = x[["X_front"]], X_back = x[["X_back"]],
                                   px_front = x[["px_front"]], px_back = x[["px_back"]],
                                   front_days = expiry_days_front, back_days = expiry_days_back, vrp_back = 0,
                                   plotting = F, verbose = F, type = x[["type"]], pos = x[["pos"]])
        print(paste(x[["ticker"]], round(mean(res)/sd(res)*sqrt(expiry_days_front/365),2)))
    }
}

# Quick simulations, with IV
{
    
    # Single option only
    {
        S0 = 26450
        start_date <- "2026-04-16"
        end_date <- "2026-04-21"
        expiry_days <- as.numeric(as.Date(end_date) -  as.Date(start_date))
        trading_days <- bizdays(start_date, end_date, "NYSE")
        X <- 25600
        px <- 32
        sigma <- 21 / 100
        type <- "put"
        pos <- -1
        gbm <- gbm_vec(nsim = 10000, t = trading_days, mu = 0, sigma = sigma, S0 = S0, dt = 1./252)
        res <- sim_single_profit(gbm = gbm, S0 = S0, X = X, px = px, expiry_days = expiry_days, pos = pos, contracts = 20, type = type, plotting = T, verbose = T)
        
    }
    # Strangle
    {
        S0 = 38.69
        start_date <- "2026-04-01"
        end_date <- "2026-04-17"
        expiry_days <- as.numeric(as.Date(end_date) -  as.Date(start_date))
        trading_days <- bizdays(start_date, end_date, "NYSE")
        X_call <- 41
        X_put <- 36
        px_call <- 0.53
        px_put <- 0.35
        sigma <- 27.22 / 100
        pos <- -1
        cost <- 0
        gbm <- gbm_vec(nsim = 10000, t = trading_days, mu = 0, sigma = sigma, S0 = S0, dt = 1./252)
        print(paste("Total price:", -px_call-px_put+cost))
        res <- sim_strangle_profit(gbm, S0 = S0,  cost = cost,
                                   X_call = X_call, X_put = X_put, 
                                   px_call = px_call, px_put = px_put, expiry_days =  expiry_days, plotting = T, verbose = T, pos = pos)
    }
    
    
    
    # Calendar 
    {
        S0 <- 82.77
        front_days <- 10
        back_days <- 45
        trading_days <- bizdays("2026-03-31", "2026-04-10", "NYSE")
        X_front <- 82.5
        X_back <- 83
        px_front <- 2.20
        px_back <- 4.10
        sigma <- 34 / 100
        vrp_back <- 0
        type <- "call"
        pos <- -1
        gbm <- gbm_vec(nsim = 10000, t = trading_days, mu = 0, sigma = sigma, S0 = S0, dt = 1./252, only_terminal = F)
        #gbm <- simulate_gbm(S0 = , mu = 0, sigma = sigma, T = (trading_days-1) / 252, n_paths = 10000, long = F, dt = 1./252)$prices
        print(paste("Total price:", -px_front+px_back))
        res <- sim_calendar_profit(gbm, S0 = S0, X_front = X_front, X_back = X_back,px_front = px_front, px_back = px_back,
                                   front_days = front_days, back_days = back_days, vrp_back = vrp_back,
                                   plotting = T, verbose = T, type = type, pos = pos)
    }
        
    # Bull Put
    {
        S0 <- 38.37
        short_X <- 37.5
        long_X <- 28
        short_price <- 0.57
        long_price <- 0.02
        start_date <- "2026-02-23"
        end_date <- "2026-03-20"
        expiry_days <- as.numeric(as.Date(end_date) -  as.Date(start_date)) 
        trading_days <- bizdays(start_date, end_date, "NYSE")
        #expiry_days <- 24
        drift <- 0/100
        rv <- 26.61/100
        pos <- 1
        gbm <- gbm_vec(10000, t = trading_days, S0 = S0, sigma = rv, mu = drift, dt = 1./252)
        res <- sim_bull_put(gbm, S0 = S0, short_X = short_X, long_X = long_X,short_price = short_price, long_price = long_price,
                            expiry_days = expiry_days, drift = drift,  plotting = T, verbose = T, pos = pos)
        
    }
    
        
}
    
    


    
    
# Testing profitability with drift (shorter holding is necessary to counteract drift)
{
    VRP <- 0
    mu = 0
    sigma <- 0.2
    r <- 0
    df <- simulate_option_prices(vrp_level = VRP, mu = mu, sigma = sigma)
    plot_option_summary(df)
    # df <- data.frame(pxAtmIv = as.vector(gbm_vec(1, 2672, mu, sigma, 100, dt = 1/252)),
    #                  dtExM1=rep(30:1, length.out = 2672),
    #                  dtExM2 = 100,
    #                  atmIvM1 = sigma
    #                  ) %>%
    #     mutate(
    #         hiStrikeM1 = round(pxAtmIv),
    #         hiStrikeM2 = round(pxAtmIv),
    #         straPxM1 = price_straddle(pxAtmIv, hiStrikeM1, r, atmIvM1, dtExM1 / 365),
    #         straPxM2 = 0,
    #         atmIvM2 = 0
    #     )
    # For some reason I cannot calculate the price of a given straddle after some n days... ok, I have to adjust for stock "speed"
    timeWeek <- 7
    df <- df %>% mutate(
        straPxM1_7 = case_when(
            dtExM1 > timeWeek ~ price_straddle(lead(pxAtmIv, timeWeek), hiStrikeM1, r, 
                                               lead(atmIvM1, timeWeek),  
                                               dtExM1/365  - (timeWeek)/252), TRUE ~ NA), # adjustment here
                                    .after = straPxM1        ) %>%
        mutate(
            straProM1_7 = straPxM1_7 - straPxM1,
            straRetM1_7 = straProM1_7 / pxAtmIv,
        ) %>%
        mutate(
            timeWeek_i = row_number() %% timeWeek,
                .after = straRetM1_7,
        )
    df %>% filter(dtExM1 == 25) %>% mutate(PnL = cumsum(straProM1 %>% replace_na(0))) %>% pull(PnL) %>% plot.ts
    df %>% filter(timeWeek_i == 0) %>% mutate(PnL = cumsum(straProM1_7 %>% replace_na(0))) %>% pull(PnL) %>% plot.ts
}

# Sharpe ratio simulations 
{
    VRP <- 0.2
    mu = 0.0
    sigma <- 0.2
    DTE <- 25
    df <- simulate_option_prices(vrp_level = VRP, mu = mu, sigma = sigma)
    #df_dba <- ORATS_core_ds  %>% filter(ticker == "DBA") %>% collect
    #df_spy <- ORATS_core_ds  %>% filter(ticker == "SPY") %>% collect
    plot_option_summary(df)
    # Simulate 7 uncorrelated strategies
    res <- list()
    for(i in 1:7){
        print(i)
        res[[i]] <- simulate_option_prices(vrp_level = VRP, mu = mu, sigma = sigma, end_date = "2010-01-01")
    }
    # Get each sim returns
    rets <- lapply(res, function(df){
        df %>% dplyr::filter(dtExM1 == DTE) %>% pull(straRetM1) %>% {-(.)}
    }) %>% do.call(cbind, .)
    # Get all equities
    eqs <- apply(rets, 2, cumsum)
    matplot2(eqs)
    # Every simulation SR
    apply(rets, 2, function(x) adj_sharpe_ratio(x, period = 252/DTE))
    # All simulation SR
    rowMeans(rets) %>% adj_sharpe_ratio(., period = 252/DTE)
}

# Calculating ROI, use target volatility (you can set total_profit with a bespoke example)
{
    capital <- 20000
    target_vol <- 0.25
    S0 <- 20
    X <- 20
    days <- 30
    iv_call <- 30/100
    iv_put <- 30/100
    total_cost <- 0
    rv <- 20 / 100
    r <- 0
    set.seed(NULL)
    gbm <- simprice(s0 = S0, v = rv, r = 0, tt=days/252, d = 0, trials = 10000, periods = days-1, long = F)[,-1] %>% as.matrix() %>% t
    call <- option_sim_profit(gbm, type = "call", X = X, tt_start = days/365, tt_end = 1/365, v = iv_call, r = r); 
    put <- option_sim_profit(gbm, type = "put", X = X, tt_start = days/365, tt_end = 1/365, v = iv_put, r = r); 
    profit <- (-call$profit -put$profit -total_cost ) * 100
    total_profit <- profit * 12 # number of trades per year
    q <- quantile(total_profit, c(0.16, 1-0.16)) # the corresponding quantile in a normal distribution (1 sd)
    w <- (capital * target_vol)  / abs(q[1])  
    weighted_total_profit <- w * total_profit
    quantile(weighted_total_profit, c(0.16, 1-0.16)) # check it's target_vol * capital
    # ROI
    roi <- round(mean(weighted_total_profit) / capital * 100, 1)
    print("Calculating ROI...")
    print(paste0("IV: ", mean(iv_call, iv_put)*100, "%, RV: ",rv*100, "%"))
    print(paste0("ROI: ", roi, "%"))
    
}


    
# Simulate some strategies like E.Sinclair in Positional Options Trading
{
    
    ## Good Straddle  
    gbm <- gbm_vec(10000, sigma = 0.2)
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
    gbm <- gbm_vec(10000, 30,  sigma = 0.3)
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
    gbm <- gbm_vec(1, 30, sigma = 0.3) %>% as.vector
    call_path_short <- bscall(gbm, 100, 0.3, 0, seq(30/365, 1/365, length.out=30), 0)
    call_path_long <- bscall(gbm, 100, 0.3, 0, seq(60/365, 31/365, length.out=30), 0)
    call_spread <- call_path_long - call_path_short 
    put_path_short <- bsput(gbm, 100, 0.3, 0, seq(30/365, 1/365, length.out=30), 0)
    put_path_long <- bsput(gbm, 100, 0.3, 0, seq(60/365, 31/365, length.out=30), 0)
    put_spread <- put_path_long - put_path_short365
    straddle_path <- -call_path_short-put_path_short + call_path_long+put_path_long
    matplot2(cbind(call_spread, put_spread, straddle_path/2))
    
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
        gbm <- gbm_vec(10000, front_days, S0 = S0, sigma = rv)
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



