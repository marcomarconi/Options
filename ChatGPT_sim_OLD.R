
# 1) EGARCH volatility simulator
simulate_egarch_condvol <- function(
        n_steps,
        seed = NULL,
        omega_e = -0.5,     # EGARCH intercept tuned for ~target vol
        alpha1  = 0.06,      # like GARCH(1,1) alpha
        beta1   = 0.92,      # like GARCH(1,1) beta
        gamma1 = 0,         # leverage effect
        shape  = 30,        # large => ~Gaussian
        skew   = 1,         # symmetric
        burn_in = 250
) {
    if (!is.null(seed)) set.seed(seed)
    
    spec <- ugarchspec(
        variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
        mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
        distribution.model = ifelse(shape >= 30 && skew == 1, "norm", "sstd"),
        fixed.pars = list(
            omega  = omega_e, alpha1 = alpha1, beta1 = beta1, gamma1 = gamma1,
            shape  = shape,  skew   = skew
        )
    )
    
    path <- ugarchpath(spec, n.sim = n_steps, m.sim = 1, n.start = burn_in)
    
    sigma_vec <- as.numeric(sigma(path))
    eps_vec <- tryCatch(path@path$residSim[,1], error = function(e) NULL)
    if (is.null(eps_vec)) eps_vec <- as.numeric(path@path$seriesSim[,1])
    
    tibble(t = seq_len(n_steps), sigma = sigma_vec, eps = eps_vec)
}


# 2) GBM given conditional vol
simulate_gbm_from_vol <- function(vols_df, S0, mu, period) {
    mu_daily <- mu / period
    vols_df %>%
        arrange(t) %>%
        mutate(
            r_t  = mu_daily - 0.5 * sigma^2 + eps,
            logS = cumsum(replace_na(r_t, 0)),
            px   = S0 * exp(logS)
        )
}
