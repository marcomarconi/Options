library(dplyr)
library(readr)
library(lubridate)

compute_leg_attribution <- function(path,
                                    leg_name,
                                    purchase_date,
                                    quantity = 1) {
    df <- read_csv(path, show_col_types = FALSE) |>
        mutate(
            IV   = as.numeric(gsub("%","", IV)) / 100,
            Time = ymd(Time)
        )  |> rename(Price = `Price~`) |> 
        arrange(Time) |>
        mutate(
            dS  = Price - lag(Price),
            dIV = IV - lag(IV),
            dR  = 0,
            Mid = (Bid+Ask)/2,
            # ---- realized pnl ----
            #realized_pnl = quantity * (Latest - lag(Latest)),
            realized_pnl = quantity * (Mid - lag(Mid)),
            
            # ---- attribution components ----
            delta_pnl = quantity * lag(Delta) * dS,
            gamma_pnl = quantity * 0.5 * lag(Gamma) * (dS^2),
            spot_pnl = delta_pnl + gamma_pnl,
            
            # Vega per 1% vol point
            vega_pnl  = quantity * lag(Vega) * (dIV * 100),
            
            theta_pnl = quantity * lag(Theta),
            rho_pnl   = quantity * lag(Rho) * dR,
            
            explained_pnl =
                delta_pnl + gamma_pnl + vega_pnl + theta_pnl + rho_pnl,
            
            residual_pnl = realized_pnl - explained_pnl,
            
            leg = leg_name
        ) |>
        filter(Time > purchase_date)
    
    return(df)
}


{
purchase_date <- ymd("2026-02-18")

files <- list(
           list("~/Downloads/tur_20260320_4000p_price-history-03-14-2026.csv", "leg1", -100),
           list("~/Downloads/tur_20260320_4500c_price-history-03-14-2026.csv", "leg2", -100)
)

strategy_attr <- bind_rows(lapply(files, function(x) compute_leg_attribution(x[[1]], x[[2]], purchase_date, x[[3]])))


strategy_daily <- strategy_attr |>
    group_by(Time) |>
    summarise(
        realized_pnl = sum(realized_pnl, na.rm = TRUE),
        delta_pnl    = sum(delta_pnl,    na.rm = TRUE),
        gamma_pnl    = sum(gamma_pnl,    na.rm = TRUE),
        spot_pnl    = sum(spot_pnl,    na.rm = TRUE),
        vega_pnl     = sum(vega_pnl,     na.rm = TRUE),
        theta_pnl    = sum(theta_pnl,    na.rm = TRUE),
        residual_pnl = sum(residual_pnl, na.rm = TRUE)
    ) |>
    ungroup()


strategy_plot <- strategy_daily |>
    pivot_longer(
        cols = c(spot_pnl, vega_pnl, theta_pnl, residual_pnl),
        names_to = "component",
        values_to = "pnl"
    )

p1 <- ggplot(strategy_plot, aes(x = Time, y = pnl, fill = component)) +
    geom_col() +
    labs(
        title = "Strategy PnL Attribution (Aggregated Across Legs)",
        x = "Time",
        y = "PnL",
        fill = "Component"
    ) + geom_hline(yintercept = 0) + 
    theme_minimal() + scale_fill_colorblind()

df_cum <- strategy_daily |>
    select(Time,
           spot_pnl, vega_pnl, theta_pnl, residual_pnl) |>
    pivot_longer(
        cols = -Time,
        names_to = "component",
        values_to = "pnl"
    ) |>
    arrange(Time) |>
    group_by(component) |>
    mutate(cumulative_pnl = cumsum(pnl)) |>
    ungroup()

p2 <- ggplot(df_cum, aes(x = Time, y = cumulative_pnl, fill = component)) +
    geom_col() +
    labs(
        title = "Cumulative Option PnL Attribution",
        x = "Time",
        y = "Cumulative PnL",
        fill = "Component"
    ) + geom_hline(yintercept = 0)+
    theme_minimal() + scale_fill_colorblind()

p3 <- ggplot(strategy_attr, aes(x = Time, y = Price)) +
    geom_line() + geom_point() + theme_minimal()

p1 / p2 / p3
}
