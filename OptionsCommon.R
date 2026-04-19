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



# Convenience functions
{
    
    c4 <- function(n) sqrt(2/(n-1)) * gamma(n/2) / gamma((n-1)/2)
    
    
    
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
    # -------------------------------
    # Safe binomial pricer: CRR with automatic JR fallback
    # -------------------------------
    binomial_option_price <- function(S, K, r, q = 0, tau, sigma, steps = 200,
                                      type = c("call","put"),
                                      american = FALSE,
                                      model = c("CRR","JR")) {
        type  <- match.arg(type)
        model <- match.arg(model)
        
        dt <- tau / steps
        if (dt <= 0) stop("tau must be > 0 and steps must be >= 1")
        
        make_tree <- function(model) {
            if (model == "CRR") {
                u <- exp(sigma * sqrt(dt))
                d <- 1 / u
                edrq <- exp((r - q) * dt)
                p <- (edrq - d) / (u - d)
                list(u=u, d=d, p=p)
            } else { # JR
                # Jarrow–Rudd centered tree: guarantees 0 <= p <= 1 with p = 0.5
                mu <- (r - q - 0.5 * sigma^2) * dt
                u  <- exp(mu + sigma * sqrt(dt))
                d  <- exp(mu - sigma * sqrt(dt))
                p  <- 0.5
                list(u=u, d=d, p=p)
            }
        }
        
        pars <- make_tree(model)
        # Fallback to JR if CRR probability invalid
        if (model == "CRR" && (!is.finite(pars$p) || pars$p < 0 || pars$p > 1)) {
            pars <- make_tree("JR")
        }
        
        u <- pars$u; d <- pars$d; p <- pars$p
        disc <- exp(-r * dt)
        
        # Terminal prices and payoffs
        j  <- 0:steps
        ST <- S * (u^j) * (d^(steps - j))
        values <- if (type == "call") pmax(ST - K, 0) else pmax(K - ST, 0)
        
        # Backward induction
        for (n in steps:1) {
            values <- disc * (p * values[2:(n+1)] + (1 - p) * values[1:n])
            if (american) {
                j  <- 0:(n-1)
                S_n <- S * (u^j) * (d^(n-1 - j))
                intrinsic <- if (type == "call") pmax(S_n - K, 0) else pmax(K - S_n, 0)
                values <- pmax(values, intrinsic)
            }
        }
        
        values[1]
    }
    
    # -------------------------------
    # Robust IV via uniroot + safe pricing
    # -------------------------------
    implied_vol_binomial <- function(price, S, K, r, q = 0, tau, steps = 200,
                                     type = c("call","put"),
                                     american = FALSE,
                                     model = c("CRR","JR"),
                                     lower = 1e-6, upper = 5, tol = 1e-7, maxiter = 100) {
        type  <- match.arg(type)
        model <- match.arg(model)
        
        f <- function(sigma) {
            out <- try(
                binomial_option_price(S, K, r, q, tau, sigma, steps, type, american, model),
                silent = TRUE
            )
            if (inherits(out, "try-error") || !is.finite(out)) return(NA_real_)
            out - price
        }
        
        f_low <- f(lower)
        f_up  <- f(upper)
        
        # Expand upper bound until we bracket or give up
        tries <- 0
        while (!is.na(f_low) && !is.na(f_up) && f_low * f_up > 0 && tries < 25) {
            upper <- upper * 1.6
            f_up  <- f(upper)
            tries <- tries + 1
        }
        
        if (is.na(f_low) || is.na(f_up) || f_low * f_up > 0) {
            stop("Failed to bracket IV. Check inputs (units!) or try model='JR' and/or more steps.")
        }
        
        uniroot(function(s) f(s), lower = lower, upper = upper, tol = tol, maxiter = maxiter)$root
    }
    
    
# Simulating options outcomes
{
    # Strangle/Straddle (with prices)
    sim_strangle_profit <- function(gbm, S0, X_call, X_put, px_call, px_put, expiry_days, 
                                    cost = 0, pos = -1, r = 0, d = 0, opt_period = 365, spot_period = 252, contracts = 100,
                                    plotting = F, verbose = F){
        tte <- expiry_days/opt_period
        tte1 <- 0.01/opt_period
        iv_call <- bscallimpvol(S0, X_call, r=r, tte, d=d, price = px_call)
        iv_put <- bsputimpvol(S0, X_put, r=r, tte, d=d, price = px_put)
        call <- option_sim_profit(gbm, type = "call", X = X_call, tt_start = tte, tt_end = tte1, v = iv_call, r = r); 
        put <- option_sim_profit(gbm, type = "put", X = X_put, tt_start = tte, tt_end = tte1, v = iv_put, r = r); 
        profit <-( (call$profit + put$profit) * pos -cost ) * contracts
        if(verbose) {
            print(paste("IV call:", round(iv_call*100, 2), "%"))
            print(paste("IV put:", round(iv_put*100, 2), "%"))
            print(paste("Profit as percentage of premium:", round(mean(profit) / ((px_call + px_put) * abs(pos)), 2), "%"))
            print(paste("Probability of profit:", (table(profit>0)/length(profit))[2]*100, "%"))
            ann_mean <-  mean(profit) * (opt_period / expiry_days)
            ann_sd <-  sd(profit) * sqrt(opt_period / expiry_days)
            ann_SR <- mean(profit) / sd(profit)  * sqrt(expiry_days / opt_period)
            print(paste("Annualized mean profit:", ann_mean %>% round(2)))
            print(paste("Annualized sd profit:", ann_sd%>% round(2)))
            print(paste("Annualized SR:", ann_SR %>% round(2)))
            print(profit %>% summary)
        }
        if(plotting) {
            profit %>% hist(50)
        }
        return(profit)
    }
    
    # Strangle/Straddle (with prices)
    sim_bullput_profit <- function(gbm, S0, short_X, long_X, short_price, long_price, expiry_days, drift = 0,
                                   cost = 0, pos = 1, r = 0, d = 0, opt_period = 365, spot_period = 252, contracts = 100,
                                   plotting = F, verbose = F){
        tte <- expiry_days/opt_period
        tte1 <- 0.01/opt_period
        short_iv <- bsputimpvol(S0, short_X, r=r, expiry_days/opt_period, d=0, price = short_price)
        long_iv <- bsputimpvol(S0, long_X, r=r, expiry_days/opt_period, d=0, price = long_price) 
        put_short <- option_sim_profit(gbm, type = "put", X = short_X, tt_start = expiry_days/opt_period, tt_end = 1/opt_period, v = short_iv); 
        put_long <- option_sim_profit(gbm, type = "put", X = long_X, tt_start = expiry_days/opt_period, tt_end = 1/opt_period, v = long_iv); 
        profit <- ((-put_short$profit +put_long$profit )  * pos -cost )* contracts
        if(verbose) {
            print(paste("Total price:", -short_price+long_price))
            print(paste("short IV:", round(short_iv*100, 2), "%"))
            print(paste("long IV:", round(long_iv*100, 2), "%"))
            print(paste("Profit as percentage of premium:", round(mean(profit) / ((short_price-long_price) * abs(pos)), 2), "%"))
            print(paste("Probability of profit:", (table(profit>0)/length(profit))[2]*100, "%"))
            ann_mean <-  mean(profit) * (opt_period / expiry_days)
            ann_sd <-  sd(profit) * sqrt(opt_period / expiry_days)
            ann_SR <-  mean(profit) / sd(profit)  * sqrt(opt_period / expiry_days)
            print(paste("Annualized mean profit:", ann_mean %>% round(2)))
            print(paste("Annualized sd profit:", ann_sd%>% round(2)))
            print(paste("Annualized SR:", ann_SR %>% round(2)))
            print(profit %>% summary)
        }
        if(plotting) {
            profit %>% hist(50)
        }
        return(profit)
    }
    
    sim_calendar_profit <- function(gbm, S0, X_front, X_back, px_front, px_back, 
                                    front_days, back_days, vrp_back = 0,
                                    type = "put", cost = 0, pos = -1, r = 0, d = 0, opt_period = 365, spot_period = 252,  contracts = 100,
                                    plotting = F, verbose = F){
        tte_front <- front_days/opt_period
        tte_back <- back_days/opt_period
        tte_backTOfront <- (back_days-front_days+1)/opt_period
        tte1 <- 0.01/opt_period
        iv_front <- ifelse(type == "call", 
                           bscallimpvol(S0, X_front, r=r, tte_front, d=d, price = px_front), 
                           bsputimpvol(S0, X_front, r=r, tte_front, d=d, price = px_front))
        iv_back <- ifelse(type == "call", 
                          bscallimpvol(S0, X_back, r=r, tte_back, d=d, price = px_back), 
                          bsputimpvol(S0, X_back, r=r, tte_back, d=d, price = px_back)) 
        iv_backTOfront <- iv_back * (1 - vrp_back) # Removing the expected premium
        forward_vol <- sqrt( (tte_back * iv_back^2 - tte_front * iv_front^2) / (tte_back - tte_front) )
        forward_factor <- (iv_front - sqrt(forward_vol^2)) / sqrt(forward_vol^2)
        front <- option_sim_profit(gbm, type = type, X = X_front, tt_start = tte_front, tt_end = tte1, v = iv_front); 
        back <- option_sim_profit(gbm, type = type, X = X_back, tt_start = tte_back, tt_end = tte_backTOfront, v = iv_backTOfront); 
        profit <- ((front$profit  - back$value + px_back) * pos - cost) * contracts
        if(verbose) {
            print(paste("IV front:", round(iv_front*100, 2), "%"))
            print(paste("IV back:", round(iv_back*100, 2), "%"))
            print(paste("IV back to front:", round(iv_backTOfront*100, 2), "%"))
            print(paste("Forward Volatility:", round(forward_vol*100, 2), "%"))
            print(paste("Forward Factor:", round(forward_factor, 2)))
            print(paste("Profit as percentage of premium:", round(mean(profit) / ((px_front + px_back) * abs(pos)), 2), "%"))
            print(paste("Probability of profit:", (table(profit>0)/length(profit))[2]*100, "%"))
            ann_mean <-  mean(profit) * (opt_period / front_days)
            ann_sd <-  sd(profit) * sqrt(opt_period / front_days)
            SR <- mean(profit) / sd(profit) 
            print(paste("Annualized mean profit:", ann_mean %>% round(2)))
            print(paste("Annualized sd profit:", ann_sd%>% round(2)))
            print(paste("SR:", SR %>% round(2)))
            print(profit %>% summary)
        }
        if(plotting) {
            profit  %>% hist(50)
        }
        return(profit)
    }
    
    sim_single_profit <- function(gbm, S0, X, px, expiry_days, 
                                    type = "put", cost = 0, pos = -1, r = 0, d = 0, opt_period = 365, spot_period = 252, contracts = 100,
                                  plotting = F, verbose = F){
        tte <- expiry_days/opt_period
        tte1 <- 0.01/opt_period
        iv <- ifelse(type == "call", 
                           bscallimpvol(S0, X, r=r, tte, d=d, price = px), 
                           bsputimpvol(S0, X, r=r, tte, d=d, price = px))
        single <- option_sim_profit(gbm, type = type, X = X, tt_start = tte, tt_end = tte1, v = iv); 
        profit <- (single$profit  * pos) * contracts
        if(verbose) {
            print(paste("IV:", round(iv*100, 2), "%"))
            print(paste("Profit as percentage of premium:", round(mean(profit) / (px*pos) * 100, 2)* pos, "%"))
            print(paste("Probability of profit:", (table(profit>0)/length(profit))[2]*100, "%"))
            ann_mean <-  mean(profit) * (opt_period / expiry_days)
            ann_sd <-  sd(profit) * sqrt(opt_period / expiry_days)
            ann_SR <- ann_mean / ann_sd
            print(paste("Annualized mean profit:", ann_mean %>% round(2)))
            print(paste("Annualized sd profit:", ann_sd%>% round(2)))
            print(paste("Annualized SR:", ann_SR %>% round(2)))
            print(profit %>% summary)
        }
        if(plotting) {
            profit  %>% hist(50)
        }
        return(profit)
    }
    
    # Generic option combo profit simulator, with IV calculation
    sim_combo_profit <- function(gbm, X, type, pos, iv, days_start, days_end, r=0, d=0){
        opts <- sapply(1:length(X), 
                       function(i){
                           ret <- option_sim_profit(gbm, type = type[i], X = X[i], tt_start = days_start[i]/365, tt_end = days_end[i]/365, v = iv[i]); 
                           return(pos[i] *  (ret$value - ret$premium))
                       }
        ) 
        return(opts)
    }
    
    # Generic option combo terminal profit simulation, no IV calculation
    option_expiration_profit <- function(gbm, X, type, pos, premium, expiry_days, opt_period = 365, size = 100, verbose=FALSE){
        opts <- sapply(1:length(X), 
                       function(i){
                           if(type[i] == "call")
                               return((apply(cbind(gbm - X[i], 0), 1, max) - premium[i]) * pos[i])
                           else if(type[i] == "put")
                               return((apply(cbind(X[i] - gbm, 0), 1, max) - premium[i]) * pos[i])
                           else 
                               stop(paste0("Wrong option type", type[i]))
         
                       }
        ) 
        profit <- rowSums(opts) * size
        if(verbose) {
            print(paste("Profit as percentage of premium:", round(mean(profit) / sum(premium*size) * 100, 2), "%"))
            print(paste("Probability of profit:", (table(profit>0)/length(profit))[2]*100, "%"))
            ann_mean <-  mean(profit) * (opt_period / expiry_days)
            ann_sd <-  sd(profit) * sqrt(opt_period / expiry_days)
            ann_SR <- ann_mean / ann_sd
            print(paste("Annualized mean profit:", ann_mean %>% round(2)))
            print(paste("Annualized sd profit:", ann_sd%>% round(2)))
            print(paste("Annualized SR:", ann_SR %>% round(2)))
            print(profit %>% summary)
        }
        return(profit)
    }
}
    
    
}
    


