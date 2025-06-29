
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
    # Put spread (with delta given)
    {
        S0 <- 79.33
        X1 <- 78
        X2 <- 75
        days <- 44
        iv1 <- 7.91/100
        iv2 <- 11.49/100
        drift <- 0/100
        rv <- mean(c(5.72, 4.94, 5.41, 10.04, 7.86))/100
        r <- 0.045
        gbm <- gbm_vec(10000, t = days, S0 = S0, dt = 1/252, sigma = rv, mu = drift)
        put1 <- option_sim_profit(gbm, type = "put", X = X1, tt_start = days/365, tt_end = 1/365, v = iv1); 
        put2 <- option_sim_profit(gbm, type = "put", X = X2, tt_start = days/365, tt_end = 1/365, v = iv2); 
        profit <- (-put1$profit +put2$profit ) * 100
        profit %>% hist
        profit %>% summary
    }
    # Put spread (with delta estimated)
    {
        S0 <- 79.33
        d1 <- 0.3
        d2 <- 0.1
        days <- 45
        iv1 <- 0.40
        iv2 <- 0.45
        rv <- 0.35
        r <- 0.045
        X1 <- get_delta_put(S0, days/365, sigma = iv1, r = r, delta_target = d1)
        X2 <- get_delta_put(S0, days/365, sigma = iv2, r = r, delta_target = d2)
        gbm <- gbm_vec(10000, t = days, S0 = S0, dt = 1/252, sigma = rv)
        put1 <- option_sim_profit(gbm, type = "put", X = X1, tt_start = days/365, tt_end = 1/365, v = iv1); 
        put2 <- option_sim_profit(gbm, type = "put", X = X2, tt_start = days/365, tt_end = 1/365, v = iv2); 
        profit <- (-put1$profit   + -put2$profit ) * 100
        profit %>% hist
        profit %>% summary
    }
    # Call spread (with delta given)
    {
        S0 <- 22.64
        X1 <- 26
        X2 <- 34
        days <- 34
        iv1 <- 70.36/100
        iv2 <- 97.78/100
        drift <- -44/100
        rv <- mean(c(53.05))/100
        r <- 0.045
        gbm <- gbm_vec(10000, t = days, S0 = S0, dt = 1/252, sigma = rv, mu = drift)
        call1 <- option_sim_profit(gbm, type = "call", X = X1, tt_start = days/365, tt_end = 1/365, v = iv1); 
        call2 <- option_sim_profit(gbm, type = "call", X = X2, tt_start = days/365, tt_end = 1/365, v = iv2); 
        profit <- (-call1$profit +call2$profit ) * 100
        print(summary(profit))
        profit %>% hist(50)
        table(profit>0)/length(profit)
    }
    # Calendars
    {
        S0 <- 43.34
        X1 <- 43
        X2 <- 43
        front_days <- 11
        back_days <- 39
        front_IV <- 90.37/100
        back_IV <- 76.50/100
        rv <- mean(c(90))/100
        gbm <- gbm_vec(10000, front_days, S0 = S0, dt = 1/252, sigma = rv)
        front_put <- option_sim_profit(gbm, type = "put", X = X1, tt_start = front_days/365, tt_end = 1/365, v = front_IV); 
        front_call <- option_sim_profit(gbm, type = "call", X = X1, tt_start = front_days/365, tt_end = 1/365, v = front_IV); 
        back_put <- option_sim_profit(gbm, type = "put", X = X2, tt_start = back_days/365, tt_end = (back_days-front_days)/365, v = back_IV); 
        back_call <- option_sim_profit(gbm, type = "call", X = X2, tt_start = back_days/365, tt_end = (back_days-front_days)/365, v = back_IV); 
        profit <- (-front_put$profit -front_call$profit   + back_put$value - back_put$premium + back_call$value - back_call$premium) * 100
        print(summary(profit))
        profit %>% hist(50)
        table(profit>0)/length(profit)
    }
    # Strangle/Straddle (with prices)
    {
        S0 <- 33.07
        X_call <- 33
        X_put <- 33
        days <- 48
        price_call <- 2.08
        price_put <- 1.80
        total_cost <- 0.0 # in case of iron condor
        rv <- mean(c(31)) / 100
        r <- 0.02
        iv_call <- bscallimpvol(S0, X_call, r=r, days/365, d=0, price = price_call)
        iv_put <- bsputimpvol(S0, X_put, r=r, days/365, d=0, price = price_put)
        gbm <- gbm_vec(10000, t = days, S0 = S0, dt = 1/252, sigma = rv)
        gbm <- simprice(s0 = S0, v = rv, r = 0, tt=days/252, d = 0, trials = 10000, periods = days-1, long = F)[,-1] %>% as.matrix() %>% t
        call <- option_sim_profit(gbm, type = "call", X = X_call, tt_start = days/365, tt_end = 1/365, v = iv_call, r = r); 
        put <- option_sim_profit(gbm, type = "put", X = X_put, tt_start = days/365, tt_end = 1/365, v = iv_put, r = r); 
        print(paste("IV call:", round(iv_call, 2)))
        print(paste("IV put:", round(iv_put, 2)))
        profit <- (-call$profit -put$profit -total_cost ) 
        profit %>% hist(50)
        print((profit*100) %>% summary)
        print(paste("As percentage of premium: ", round(mean(profit) / (price_call + price_put) * 100, 2), "%"))
        table(profit>0)/length(profit)
    }
    # Straddle (with cost, i.e. a butterfly)
    {
        S0 <- 18.92
        X <- 19
        days <- 34
        iv_call <- 45.70/100
        iv_put <- 62.70/100
        total_cost <- 0
        rv <- mean(c(45.31)) / 100
        r <- 0.045
        gbm <- gbm_vec(10000, t = days, S0 = S0, dt = 1/252, sigma = rv)
        call <- option_sim_profit(gbm, type = "call", X = X, tt_start = days/365, tt_end = 1/365, v = iv_call, r = r); 
        put <- option_sim_profit(gbm, type = "put", X = X, tt_start = days/365, tt_end = 1/365, v = iv_put, r = r); 
        profit <- (-call$profit -put$profit -total_cost ) * 100
        profit %>% hist(50)
        print(profit %>% summary)
        table(profit>0)/length(profit)
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
    
    option_combo_sim <- function(gbm, X, type, pos, iv, days_start, days_end, r=0, d=0){
       opts <- sapply(1:length(X), 
                          function(i){
                              ret <- option_sim_profit(gbm, type = type[i], X = X[i], tt_start = days_start[i]/365, tt_end = days_end[i]/365, v = iv[i]); 
                              return(pos[i] *  (ret$value - ret$premium))
                          }
                      ) 
       return(opts)
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
