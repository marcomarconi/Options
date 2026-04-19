# Options Pricing Simulator  -----------------------------
{
library(lubridate)
library(tidyverse)
library(zoo)
library(timeDate)
library(TTR)
library(rugarch)
library(ggplot2)
library(patchwork)
library(roll)    
source("/home/marco/trading/Systems/Common/Common.R")
    source("/home/marco/trading/Systems/Options/OptionsCommon.R")
}

# Functions for Option pricing simulation 
{
# 3) Wrapper with EGARCH controls + target_ann_vol + optional day-1 anchoring
simulate_garch_gbm_df <- function(
        S0, mu, start_date, end_date,
        alpha = 0.05, beta = 0.94, gamma1  = 0,  
        period = 252, 
        target_ann_vol = 0.16,      
        seed = NULL
) {
    # --- weekday trading dates ---
    all_days <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
    wd <- as.POSIXlt(all_days)$wday
    dates <- all_days[wd %in% 1:5]
    n_steps <- length(dates)
    if (n_steps <= 0) stop("No trading days between start_date and end_date.")

    sim <- gbm_garch_vec(nsim = 1, t = n_steps, mu = mu, target_vol = target_ann_vol, alpha = alpha, beta = beta, gamma1 = gamma1, S0 = S0, dt = 1./period)

    # --- output ---
    tibble(
        tradeDate = dates,
        pxAtmIv   = as.vector(sim$gbm),
        garchVol  = as.vector(sim$vol),  # daily stdev
        garchRet  = as.vector(sim$eps)     # shock
    )
}

# Simulates Nelson-Siegel model parameters (beta0, beta1, beta2) as AR(1) processes.
#
# Args:
#   n_days: Number of time steps
#   beta0_start, beta1_start, beta2_start: Initial values
#   ar: Autoregressive coefficient
#   sd: Innovation standard deviation
#
# Returns:
#   List of numeric vectors for each beta parameter
simulate_ns_params <- function(n_days,
                               beta0_start = 0.25, beta1_start = -0.05, beta2_start = 0.05,
                               ar = 0.95, sd = 0.005) {
    sim_ar1 <- function(start, min_val = -Inf, max_val = Inf) {
        x <- numeric(n_days)
        x[1] <- start
        for (t in 2:n_days) {
            x[t] <- ar * x[t - 1] + rnorm(1, 0, sd)
            x[t] <- max(min(x[t], max_val), min_val)
        }
        x
    }
    list(
        beta0 = sim_ar1(beta0_start, min_val = 0.05, max_val = 0.6),
        beta1 = sim_ar1(beta1_start, min_val = -0.15, max_val = 0.1),
        beta2 = sim_ar1(beta2_start, min_val = -0.1, max_val = 0.2)
    )
}

# Constructs an implied volatility curve using Nelson-Siegel functional form.
#
# Args:
#   tenors_days: Vector of option tenors in days
#   beta0, beta1, beta2: Nelson-Siegel parameters
#   lambda: Decay factor for curvature
#   scale_to: Target value to scale curve to (e.g., match to annVol)
#   anchor_index: Index of the tenor to match when scaling
#
# Returns:
#   Named numeric vector with IVs: iv10d, iv30d, iv90d, iv6m, iv1yr
simulate_ns_iv_curve <- function(tenors_days = c(10, 30, 90, 126, 252),
                                 beta0 = 0.2, beta1 = -0.05, beta2 = 0.1, lambda = 1.2,
                                 scale_to = NULL, anchor_index = 2) {
    tau <- tenors_days / 252
    term1 <- (1 - exp(-lambda * tau)) / (lambda * tau)
    term2 <- term1 - exp(-lambda * tau)
    iv <- beta0 + beta1 * term1 + beta2 * term2
    iv <- pmax(iv, 0.001)
    if (!is.null(scale_to)) {
        anchor_iv <- iv[anchor_index]
        scale_factor <- scale_to / anchor_iv
        iv <- iv * scale_factor
    }
    names(iv) <- c("iv10d", "iv30d", "iv90d", "iv6m", "iv1yr")
    return(iv)
}

# Finds the number of days to the first two future expiration dates.
#
# Args:
#   tradeDate: Current trade date (Date)
#   exDates: Vector of expiration dates
#
# Returns:
#   List with first two positive calendar day differences
get_days_to_exp <- function(tradeDate, exDates) {
    future_ex <- exDates[exDates > tradeDate]
    future_ex <- as.Date(future_ex, origin = "1970-01-01")
    diffs <- as.numeric(future_ex - as.Date(tradeDate))
    list(head(diffs, 2))
}

# Computes the price of a European call or put using the Black-Scholes formula.
#
# Args:
#   S: Spot price
#   K: Strike price
#   r: Risk-free rate (annual)
#   sigma: Volatility (annualized)
#   tau: Time to expiration in years
#   type: 'call' or 'put'
#
# Returns:
#   Option price (numeric)
bs_price <- function(S, K, r, sigma, tau, type = c("call", "put")) {
    type <- match.arg(type)
    d1 <- (log(S / K) + (r + 0.5 * sigma^2) * tau) / (sigma * sqrt(tau))
    d2 <- d1 - sigma * sqrt(tau)
    if (type == "call") {
        return(S * pnorm(d1) - K * exp(-r * tau) * pnorm(d2))
    } else {
        return(K * exp(-r * tau) * pnorm(-d2) - S * pnorm(-d1))
    }
}

# Computes the price of an ATM straddle (call + put at the money).
#
# Args:
#   S, K, r, sigma, tau: See bs_price
#
# Returns:
#   Total price of call and put options
price_straddle <- function(S, K, r, sigma, tau) {
    call <- bs_price(S, K, r, sigma, tau, "call")
    put <- bs_price(S, K, r, sigma, tau, "put")
    return(call + put)
}

# Estimates the implied volatility from a given straddle price using bisection.
#
# Args:
#   straddle_price: Observed straddle price
#   S: Spot price
#   r: Risk-free rate
#   tau: Time to expiration (in years)
#   tol: Tolerance for convergence
#   max_iter: Max iterations allowed
#
# Returns:
#   Implied volatility (numeric) or NA if not converged
implied_vol <- function(straddle_price, S, r, tau, tol = 1e-5, max_iter = 100) {
    target <- straddle_price
    low <- 0.0001
    high <- 5
    for (i in 1:max_iter) {
        mid <- (low + high) / 2
        est <- price_straddle(S, S, r, mid, tau)
        if (abs(est - target) < tol) return(mid)
        if (est > target) high <- mid else low <- mid
    }
    return(NA_real_)
}
}


    
# Simulate options prices similar to ORATS    
simulate_option_prices <- function(S0 = 100, mu = 0, r = 0, sigma = 0.3, 
                                   alpha = 0, beta = 0, gamma1 = 0, ns_lambda = 1, 
                                   vrp_level = 0, vrp_slope = 0, 
                                   start_date = "2001-01-01", end_date = "2025-08-01", tenors = c(10, 30, 90, 126, 252), 
                                   period = 252, year_days = 365){

    
    sd_error <- c("10" = 0.05768221, "20" = 0.02671039, "60" = 0.008496699, "120" = 0.004292088, "252" = 0.001969423)
    
    prices <- simulate_garch_gbm_df(
        S0 = S0, mu = mu, 
        start_date = as.Date(start_date), end_date = as.Date(end_date), gamma1 = gamma1, target_ann_vol = sigma, 
        alpha = alpha, beta = beta, period = period, 
        seed = NULL
    )
    
    
    ns_params <- simulate_ns_params(length(prices$tradeDate))
    prices$beta0 <- ns_params$beta0
    prices$beta1 <- ns_params$beta1
    prices$beta2 <- ns_params$beta2
    
    all_months <- seq(from = start_date %>% as.Date %m+% months(0), to = end_date %>% as.Date  %m+% months(12), by = "month")
    expirations <- data.frame(
        dt = sapply(all_months, function(m) {
            days <- seq(from = m + days(14), length = 7, by = "1 day")
            as.Date(days[wday(days) == 6][1])
        })
    )
    
    output <- prices %>%
        rowwise() %>%
        mutate(
            dt_ex = list(get_days_to_exp(tradeDate, expirations$dt)[[1]]),
            dtExM1 = if (!is.null(dt_ex) && length(dt_ex) >= 1) dt_ex[1] else NA_real_,
            dtExM2 = if (!is.null(dt_ex) && length(dt_ex) >= 2) dt_ex[2] else NA_real_,
            expiryDate1 = tradeDate + dtExM1 - 1,
            expiryDate2 = tradeDate + dtExM2 - 1,
            hiStrikeM1 = round(pxAtmIv, 0),
            hiStrikeM2 = round(pxAtmIv, 0),
            tauM1 = dtExM1 / year_days,
            tauM2 = dtExM2 / year_days,
            tau30 = 30 / year_days,
            annVol = garchVol,
            qVolM1 = annVol * (1 + vrp_level + vrp_slope * sqrt(tauM1)),
            qVolM2 = annVol * (1 + vrp_level + vrp_slope * sqrt(tauM2)),
            qVol30 = annVol * (1 + vrp_level + vrp_slope * sqrt(tau30)),
            straPxM1 = if (!is.na(dtExM1) && dtExM1 > 0) price_straddle(pxAtmIv, hiStrikeM1, r, qVolM1, tauM1) else NA_real_,
            straPxM2 = if (!is.na(dtExM2) && dtExM2 > 0) price_straddle(pxAtmIv, hiStrikeM2, r, qVolM2, tauM2) else NA_real_,
            atmIvM1 = if (!is.na(straPxM1) && dtExM1 > 0) implied_vol(straPxM1, pxAtmIv, r, tauM1) else NA_real_,
            atmIvM2 = if (!is.na(straPxM2) && dtExM2 > 0) implied_vol(straPxM2, pxAtmIv, r, tauM2) else NA_real_,
            ivs = list(
                simulate_ns_iv_curve(
                tenors_days = tenors,
                beta0 = beta0, beta1 = beta1, beta2 = beta2, lambda = ns_lambda,
                scale_to = qVol30,
                anchor_index = 2
            )),
            iv10d = ivs[1],
            iv30d = ivs[2],
            iv90d = ivs[3],
            iv6m = ivs[4],
            iv1yr = ivs[5],
            iv10d = iv30d,iv90d = iv30d,iv6m = iv30d,iv1yr = iv30d,
            d1 = if (!is.na(dtExM1) && dtExM1 > 0) (log(1) + (r + 0.5 * annVol^2) * (dtExM1 / year_days)) / (annVol * sqrt(dtExM1 / year_days)) else NA_real_,
            d2 = if (!is.na(d1) && !is.na(dtExM1)) d1 - annVol * sqrt(dtExM1 / year_days) else NA_real_,
            delta = if (!is.na(d1)) pnorm(d1) else NA_real_,
            gamma = if (!is.na(d1) && annVol > 0) dnorm(d1) / (pxAtmIv * annVol * sqrt(dtExM1 / year_days)) else NA_real_,
            vega = if (!is.na(d1)) pxAtmIv * dnorm(d1) * sqrt(dtExM1 / year_days) else NA_real_,
            theta = if (!is.na(d1)) - (pxAtmIv * dnorm(d1) * annVol) / (2 * sqrt(dtExM1 / year_days)) - r * pxAtmIv * pnorm(d1) else NA_real_,
            rho = if (!is.na(d1)) pxAtmIv * sqrt(dtExM1 / year_days) * pnorm(d1) else NA_real_
        ) %>%
        ungroup() %>%
        arrange(tradeDate) %>%
        mutate(
               pxAtmIvM1 = pxAtmIv[match(expiryDate1, tradeDate)],
               pxAtmIvM2 = pxAtmIv[match(expiryDate2, tradeDate)],
               straProM1 = abs(pxAtmIvM1 - hiStrikeM1) - straPxM1, 
               straProM2 = abs(pxAtmIvM2 - hiStrikeM2) - straPxM2,
               straRetM1 = straProM1 / pxAtmIv, 
               straRetM2 = straProM2 / pxAtmIv, 
               ) %>% 
        mutate(
            retAtmIv = log(pxAtmIv / lag(pxAtmIv)),
            orHv10d = roll_sd(retAtmIv, 10) * sqrt(period),
            orHv20d = roll_sd(retAtmIv, 20) * sqrt(period),
            clsHv20d = roll_sd(retAtmIv, 20) * sqrt(period),        
            clsHv60d = roll_sd(retAtmIv, 60) * sqrt(period),        
            clsHv120d = roll_sd(retAtmIv, 120) * sqrt(period),        
            clsHv252d = roll_sd(retAtmIv, period) * sqrt(period),
            logVRP =  log(iv30d / lead(orHv20d, 20)) - sd_error["20"],
            contango = iv90d - iv30d, slope = 0,
            ticker = "TICKER"
        ) %>%
        select(ticker, tradeDate, pxAtmIv, annVol, dtExM1, dtExM2, expiryDate1, expiryDate2, hiStrikeM1, hiStrikeM2, pxAtmIvM1, pxAtmIvM2,
               #beta0, beta1, beta2,
               iv10d, iv30d, iv90d, iv6m, iv1yr,
               straPxM1, straPxM2, atmIvM1, atmIvM2, straProM1, straProM2, straRetM1, straRetM2,
               delta, gamma, vega, theta, rho,
               retAtmIv, orHv10d, orHv20d, clsHv20d, clsHv60d, clsHv120d, clsHv252d, 
               logVRP, contango, slope)
    return(output)
}

# S0 <- 100
# mu <- 0
# r <- 0
# sigma <- 0.3
# alpha = 0.04
# beta = 0.95
# gamma1 <- 0.3
# ns_lambda <- 1.2
# vrp_level = 0 # VRP level
# vrp_slope = 0.00 # VRP slope over time
# set.seed(NULL)
# 
# output <- simulate_option_prices()
# 
# 

# Plotting option prices summaries!    
plot_option_summary <- function(output, start_date = "2001-01-01") { # some plot is filtered by dtExM1 (some not, with a reason!)
    df <- output
    df <- df %>% mutate(iv30d_S = ntile(iv30d, 7), lead_RV = lead(clsHv20d, 20)) %>% 
        group_by(dtExM1) %>% mutate(VRPd = logVRP - lag(logVRP, 1), .after=logVRP) %>% ungroup  %>% filter(tradeDate > start_date)
    
    p1 <- ggplot(df, aes(x = tradeDate, y = pxAtmIv)) +
        geom_line(color = "steelblue") +
        labs(title = "Stock Price", x = NULL, y = NULL) +
        theme_minimal()
    
    p2 <- ggplot(df , aes(x = tradeDate)) +
        geom_line(aes(y = lead_RV, color = "RV (20d)")) +
        geom_line(aes(y = iv30d, color = "IV30d")) +
        labs(title = "Future Realized vs IV30d", x = NULL, y = NULL) +
        scale_color_manual(values = c("RV (20d)" = "black", "IV30d" = "red")) +
        theme_minimal() + theme(legend.position = c(0.1, 0.9), legend.title = element_blank())
    
    p3 <- ggplot(df %>% filter(dtExM1 == 25), aes(x = logVRP)) +
        geom_histogram(fill = "darkorange", color="darkorange4") + geom_vline(xintercept = 0) +
        labs(title = NULL, x = NULL, y = "logVRP 25 dte") +
        theme_minimal()
    
    p3_ <- ggplot(df, aes(x = logVRP)) +
        geom_histogram(fill = "darkorange", color="darkorange4") + geom_vline(xintercept = 0) +
        labs(title = NULL, x = NULL, y = "logVRP all dte") +
        theme_minimal()
    
    
    p4 <- ggplot(df %>% filter(dtExM1 == 25) , aes(x = jitter(iv30d_S), y = VRPd)) +
        geom_point(color = "black") + geom_smooth(method = "lm") +
        labs(title = "logVRP", x = NULL) +
        theme_minimal()
    
    
    
    a <- data.frame(expiry = factor(c("10d","30d","90d","6m","1yr"), levels=c("10d","30d","90d","6m","1yr")), 
                    M = summarize(df , across(iv10d:iv1yr, ~mean(if_else(.==0, NA, .), na.rm=T) )) %>% t, 
                    S = summarize(df , across(iv10d:iv1yr,  ~sd(if_else(.==0, NA, .), na.rm=T)/sqrt(n())*2)) %>% t)
    p5 <- ggplot(a, aes(x = expiry, ymin = M-S, y=M, ymax=M+S, group = 1 )) +
        geom_errorbar(color = "black", width = 0.25) + geom_line(color = "blue") +  geom_point(color = "blue") +
        labs(y = "Term Structure", x = NULL) +
        theme_minimal()
    
    # logVRP corrected by c4, which is std sampling error
    b <- df %>% mutate(across(iv10d:iv1yr, ~if_else(.==0, NA, .))) %>% 
        mutate(d10 = log(iv10d / (lead(orHv10d, 10))) - (1-c4(10))*2, 
               d30 = log(iv30d / (lead(orHv20d, 20))) - (1-c4(20))*2, 
               d90 = log(iv90d / (lead(clsHv60d, 60))) - (1-c4(60))*2, 
               m6 = log(iv6m / (lead(clsHv120d, 120))) - (1-c4(120))*2, 
               y1 = log(iv1yr / (lead(clsHv252d, 252))) - (1-c4(252))*2)  %>% #filter(dtExM1 == 25) %>% 
        select(tradeDate, ticker, d10, d30, d90, m6, y1) %>% pivot_longer(cols = c(d10, d30, d90, m6, y1)) %>%  
        mutate(value = replace(value, is.infinite(value) | is.nan(value), NA)) %>% group_by(name) %>%  na.omit %>% 
        reframe(M=mean(value, na.rm=T), S=sd(value, na.rm=T)/sqrt(n()), N=n())  %>% 
        mutate(name = factor(name, levels=c("d10", "d30", "d90", "m6", "y1"))) %>% rename(expiry=name)
    p6 <- ggplot(b, aes(x = expiry, ymin = M-S, y=M, ymax=M+S, group = 1 )) +
        geom_errorbar(color = "black", width = 0.25) + geom_line(color = "blue") +  geom_point(color = "blue") +
        labs(y = "logVRP", x = NULL) +    theme_minimal()
    
    # dtExM1 filter AFTER the diff
    p7 <- ggplot(df %>% mutate(iv30d_diff = c(0,diff(iv30d))) %>% filter(dtExM1 <= 30) , aes(retAtmIv*100, iv30d_diff)) + geom_point() + geom_smooth(method = "lm") +
        labs(title = "Spot/Vol correlation", x = NULL, y = NULL) +   theme_minimal()
    
    p8 <- ggplot(df %>% filter(dtExM1 == 25) , aes(tradeDate)) + geom_hline(yintercept = 0, color = "red") + geom_line(aes(tradeDate, logVRP))  +  labs(y = "logVRP 30d", x = NULL) +   theme_minimal() + theme(axis.text.x =  element_blank())
    p9 <- ggplot(df %>% filter(dtExM1 == 25) , aes(tradeDate)) + geom_hline(yintercept = 0, color = "red") + geom_line(aes(tradeDate, contango))  +  labs(y = "Contango", x = NULL) +   theme_minimal()  + theme(axis.text.x =  element_blank())
    p10 <- ggplot(df %>% filter(dtExM1 == 25) , aes(tradeDate)) + geom_hline(yintercept = 0, color = "red") + geom_line(aes(tradeDate, slope))  +  labs(y = "Skew Slope", x = NULL) +   theme_minimal()
    
    p11 <- ggplot(df %>% filter(dtExM1 == 25) %>% mutate(PnL1 = cumsum(replace_na(straRetM1, 0)), PnL2 = cumsum(replace_na(straRetM2, 0))), aes(x = tradeDate)) + geom_line(aes(y = PnL1), color="blue") + geom_line(aes(y = PnL2), color="red") +
        labs(title = NULL, x = NULL, y = "Straddle returns 25dte") + theme_minimal()
    (p1 / (p7 | ggplot()) | p2 | (p3 / p3_)) / ((p5 / p6) |  (p8/p9/p10)  | p11)
}
 


