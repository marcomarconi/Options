
library(quantmod)
source("/home/marco/trading/Systems/Options//OptionsCommon.R")

dir <- "/home/marco/trading/HistoricalData/ORATS/API/strikes/"

# function for loading an ORATS core data file and returning a subset of columns
quiet_read_csv <- purrr::quietly((.f = read_csv))
quiet_fread <- purrr::quietly((.f = fread))

load_orats_day <- function(filename, dir, cols_to_extract) {
    #print(filename)
    # quiet_read_csv(glue::glue("/media/marco/Elements/ORATS/cores/{filename}")) #%>%
    quiet_fread(glue::glue(paste0(dir, {filename}))) %>%
        purrr::pluck("result")  %>%  
        dplyr::select(all_of(cols_to_extract)) %>%  mutate(across(dte:residualRate, ~as.numeric(.)))
        
}

cols_to_extract <- c(
    "ticker",
    "tradeDate",
    "expirDate",
    "dte",
    "strike",
    "spotPrice",    
    "delta",
    "gamma",
    "vega",
    "theta",
    "callValue",
    "putValue",
    "smvVol",
    "callMidIv",
    "putMidIv",
    "callVolume",    # optional (tie-break)
    "putVolume",      # optional (tie-break)
    "callOpenInterest",
    "putOpenInterest",
    "residualRate"
)


GEX_calculations <- function(df, ticker) {
    df <- df %>% mutate(
        tradeDate = as.Date(tradeDate),
        gamma = if_else(gamma < 0, 0, gamma),
        smvVol = if_else(smvVol<=0, NA, smvVol),
        callMidIv = if_else(callMidIv<=0, NA, callMidIv),
        putMidIv = if_else(putMidIv<=0, NA, putMidIv),
        gamma = if_else(gamma<=0 | gamma > 0.1, NA, gamma)
    ) 
    
    # Safe
    df$residualRate <- 0
    
    GEX_strike <- df %>% mutate( 
        gex_call = gamma * callOpenInterest * spotPrice^2, 
        gex_put = -gamma * putOpenInterest * spotPrice^2
    ) %>% group_by(tradeDate, strike) %>% 
        reframe(gex = sum(gex_call, na.rm=T) + sum(gex_put, na.rm=T), 
                volume_strike = callVolume+putVolume,
                spotPrice = first(spotPrice),
                smvVol = first(smvVol)
                )
    
    total_gex <- GEX_strike %>% 
        group_by(tradeDate) %>%
        reframe(spotPrice = first(spotPrice), 
                volume = sum(volume_strike),
                smvVol = mean(smvVol),  
                GEX = sum(gex, na.rm=T),
                call_wall = strike[which.max(gex)],
                put_wall  = strike[which.min(gex)]) %>% ungroup %>% 
        mutate(
            avg_volume = runMean(volume, 21),
            GEX_vl = GEX / avg_volume,
        )
    
    # gamma_flip <- GEX_strike %>%
    #     arrange(tradeDate, strike) %>%
    #     group_by(tradeDate) %>%
    #     mutate(
    #         sign_change = sign(gex) != lag(sign(gex))
    #     ) %>%
    #     filter(sign_change) %>%
    #     summarise(gamma_flip = first(strike))
    # 
    # nearest_flip_interp <- GEX_strike %>%
    #     arrange(tradeDate, strike) %>%
    #     group_by(tradeDate) %>%
    #     mutate(
    #         gex_lag    = lag(gex),
    #         strike_lag = lag(strike),
    #         sign_change = sign(gex) != sign(gex_lag)
    #     ) %>%
    #     filter(sign_change) %>%
    #     mutate(
    #         flip_interp = strike_lag - gex_lag*(strike-strike_lag)/(gex-gex_lag),
    #         dist_to_spot = abs(flip_interp-spotPrice)
    #     ) %>%
    #     slice_min(dist_to_spot,n=1) %>%
    #     summarise(
    #         nearest_gamma_flip = first(flip_interp)
    #     )
    # 
    # front_gex <- df %>%
    #     group_by(tradeDate) %>%
    #     mutate(front_dte = min(dte[dte >= 0], na.rm=TRUE)) %>%
    #     filter(dte == front_dte) %>%
    #     mutate(
    #         gex_call = gamma * callOpenInterest * spotPrice ,
    #         gex_put  =-gamma * putOpenInterest) %>%
    #     reframe(
    #         spotPrice = first(spotPrice),
    #         FrontGEX = sum(gex_call + gex_put, na.rm=TRUE)
    #     )
    # 
    # lambda <- 0.05
    # weighted_gex_exp <- df %>%
    #     mutate(
    #         w = exp(-lambda*dte),
    #         gex_call = gamma * callOpenInterest * w * spotPrice,
    #         gex_put  =-gamma * putOpenInterest * w * spotPrice
    #     ) %>%
    #     group_by(tradeDate) %>%
    #     reframe(
    #         WeightedGEX = sum(gex_call+gex_put, na.rm=TRUE)
    #     )
    
    DEX <- df %>%
        mutate(
            dex = delta * (callOpenInterest - putOpenInterest)  * spotPrice
        ) %>%
        group_by(tradeDate) %>%
        summarise(
            DEX = sum(dex, na.rm = TRUE)
        )
    
    VEX <- df %>%
        mutate(
            tt=dte/365,
            d1c=(log(spotPrice/strike)+(residualRate+callMidIv^2/2)*tt)/(callMidIv*sqrt(tt)),
            d2c=d1c-callMidIv*sqrt(tt),
            d1p=(log(spotPrice/strike)+(residualRate+putMidIv^2/2)*tt)/(putMidIv*sqrt(tt)),
            d2p=d1p-putMidIv*sqrt(tt),
            vanna_call=-gamma*spotPrice*d2c/callMidIv,
            vanna_put =-gamma*spotPrice*d2p/putMidIv,
            vex_call = vanna_call * callOpenInterest * spotPrice,
            vex_put = vanna_put * putOpenInterest * spotPrice
        ) %>%
        group_by(tradeDate) %>%
        summarise(
            VEX=sum(vex_call+vex_put,na.rm=TRUE)
        )
    
    VEX2 <- df %>%
        mutate(
            tt=dte/365,
            d1=(log(spotPrice/strike)+(residualRate+smvVol^2/2)*tt)/(smvVol*sqrt(tt)),
            d2=d1-smvVol*sqrt(tt),
            vanna = -gamma*spotPrice*d2/smvVol,
            vex = vanna *(callOpenInterest-putOpenInterest) * spotPrice
        ) %>%
        group_by(tradeDate) %>%
        summarise(
            VEX2=sum(vex,na.rm=TRUE)
        )
    
    CEX <- df %>%
        mutate(
            tt=dte/365,
            d1c=(log(spotPrice/strike)+
                     (residualRate+callMidIv^2/2)*tt)/
                (callMidIv*sqrt(tt)),
            d2c=d1c-callMidIv*sqrt(tt),
            charm_call=
                -dnorm(d1c)*
                (2*residualRate*tt-d2c*callMidIv*sqrt(tt))/
                (2*tt*callMidIv*sqrt(tt)), 
            d1p=(log(spotPrice/strike)+
                     (residualRate+putMidIv^2/2)*tt)/
                (putMidIv*sqrt(tt)),
            d2p=d1p-putMidIv*sqrt(tt),
            charm_put=
                -dnorm(d1p)*
                (2*residualRate*tt-d2p*putMidIv*sqrt(tt))/
                (2*tt*putMidIv*sqrt(tt)),
            cex_call=charm_call * callOpenInterest  * spotPrice *1/365,
            cex_put=charm_put * putOpenInterest  * spotPrice * 1/365
        ) %>%
        group_by(tradeDate) %>%
        summarise(
            CEX=sum(cex_call+cex_put,na.rm=TRUE)
        )
    
    GEXplus <- total_gex %>%
        #left_join(front_gex, by = "tradeDate") %>%
        #left_join(weighted_gex_exp, by = "tradeDate") %>%
        #left_join(gamma_flip, by = "tradeDate") %>%
        #left_join(nearest_flip_interp, by = "tradeDate") %>% 
        left_join(DEX, by = "tradeDate") %>% 
        left_join(VEX, by = "tradeDate") %>% 
        left_join(VEX2, by = "tradeDate") %>% 
        left_join(CEX, by = "tradeDate") %>% 
        #left_join(VIX_all) %>% 
        arrange(tradeDate) %>% 
        mutate(
            ret_1 = replace_na(c(0, diff(log(spotPrice))), 0),
            ret_1_abs = abs(ret_1),
            ret_1f = lead(ret_1),
            ret_1f_abs = abs(ret_1f),
            ret_1f_abs_diff = log(ret_1f_abs / ret_1_abs),
            ret_20 = runSum(ret_1, 20),
            ret_20_abs = abs(ret_20),
            ret_20f = lead(ret_20, 20),
            ret_20f_abs = abs(ret_20f),
            ret_20f_abs_diff = log(ret_20f_abs / ret_20_abs),
            VRP_1 = smvVol - (ret_1f_abs*sqrt(252))
        )
    
    
    GEXplus <- GEXplus %>% mutate(
        ticker = ticker,        
        sPW = put_wall
    )
    return(GEXplus)
}


tickers <- c("SPX", "TLT", "AAPL", "IBM", "KO", "IEF", "SLV", "UNG", "USO", "XLE", "XLI", "XLK", "GLD")

ORATS_GEX <- list()
for(ticker in tickers) {
    print(ticker)
    ticker_dir <- paste0(dir, ticker, "/")
    files <- list.files(ticker_dir, "orats_.*_20[2][0-9].*gz")
    df <- files %>% purrr::map_df(.f = load_orats_day, ticker_dir, cols_to_extract) %>% mutate(ticker = ticker)
    ORATS_GEX[[ticker]] <- GEX_calculations(df,ticker)
}

VIX1D <- getSymbols("^VIX1D",env=NULL ) %>% as.data.frame() %>% rownames_to_column("tradeDate") %>% dplyr::select(tradeDate, VIX1D.Adjusted) %>% mutate(tradeDate = as.Date(tradeDate))
VIX <- getSymbols("^VIX",env=NULL ) %>% as.data.frame() %>% rownames_to_column("tradeDate") %>% dplyr::select(tradeDate, VIX.Adjusted)%>% mutate(tradeDate = as.Date(tradeDate))
VIX9D <- getSymbols("^VIX9d",env=NULL ) %>% as.data.frame() %>% rownames_to_column("tradeDate") %>% dplyr::select(tradeDate, VIX9D.Adjusted)%>% mutate(tradeDate = as.Date(tradeDate))
VIX3M <- getSymbols("^VIX3M",env=NULL ) %>% as.data.frame() %>% rownames_to_column("tradeDate") %>% dplyr::select(tradeDate, VIX3M.Adjusted)%>% mutate(tradeDate = as.Date(tradeDate))
VIX6M <- getSymbols("^VIX6M",env=NULL ) %>% as.data.frame() %>% rownames_to_column("tradeDate") %>% dplyr::select(tradeDate, VIX6M.Adjusted)%>% mutate(tradeDate = as.Date(tradeDate))
VIX_all <-  Reduce(function(...) full_join(..., by = "tradeDate"), list(VIX1D, VIX9D, VIX, VIX3M, VIX6M)) %>%   rename_with(~ gsub("\\.Adjusted", "", .x))

# Merge them all
all_GEXs <- do.call(rbind, ORATS_GEX)

# All measures by ticker
all_GEXs %>% dplyr::filter(ticker == "SPX") %>% 
    dplyr::select(tradeDate, ret_1f, GEX, VEX, CEX, DEX) %>%  
    pivot_longer(-c(tradeDate, ret_1f)) %>% 
    ggplot(aes(value, ret_1f)) + geom_vline(xintercept = 0)+ geom_point(size=0.1) + facet_wrap(~name, scales="free")

# Return prediction by ticker
all_GEXs %>% dplyr::filter(ticker == "SPX") %>% 
    dplyr::select(tradeDate, ret_1f_abs, GEX, VEX, CEX, DEX) %>% 
    pivot_longer(-c(tradeDate, ret_1f_abs)) %>% group_by(name) %>% 
    mutate(ntile_ret = ntile(ret_1f_abs, 8), bin=ntile(value,8), Year=year(tradeDate)) %>%
    group_by(name, bin) %>%
    reframe(
        avg_abs_ret=mean(ntile_ret, na.rm=T), sb_abs_ret=sd(ntile_ret, na.rm=T)/sqrt(n())
    ) %>% ggplot(aes(x=bin, y=avg_abs_ret, ymin = avg_abs_ret-sb_abs_ret*2, ymax=avg_abs_ret+sb_abs_ret*2)) + geom_line(color="gray") + geom_point() + geom_errorbar(width=0.1) + facet_wrap(~name)

# Return prediction by measure
all_GEXs %>% 
    mutate(P = GEX) %>% 
    dplyr::select(tradeDate, P, ticker, ret_1f_abs)%>% 
    pivot_longer(-c(tradeDate, ret_1f_abs, ticker)) %>% group_by(ticker) %>% 
    mutate(ntile_ret = ntile(ret_1f_abs, 8), bin=ntile(value,8)) %>%
    group_by(name, bin, ticker) %>%
    reframe(
        avg_abs_ret=mean(ntile_ret, na.rm=T), sb_abs_ret=sd(ntile_ret, na.rm=T)/sqrt(n())
    ) %>% ggplot(aes(x=bin, y=avg_abs_ret, ymin = avg_abs_ret-sb_abs_ret*2, ymax=avg_abs_ret+sb_abs_ret*2)) + geom_line(color="gray") + geom_point() + geom_errorbar(width=0.1) + facet_wrap(~ticker)

all_GEXs %>% mutate(P = GEX_vl/spotPrice^2)  %>% ggplot(aes(tradeDate, P, color=ticker)) + geom_line(size=2) + scale_color_colorblind()

# Try to use SPX as predictor for all
spx <- all_GEXs %>% filter(ticker == "SPX") %>% select(tradeDate, GEX, VEX, CEX, DEX) %>% rename(GEX_spx = GEX, CEX_spx = CEX, VEX_spx = VEX, DEX_spx = DEX)
all_GEXs_spx <- all_GEXs %>% full_join(spx)
# Binning
all_GEXs_spx %>% dplyr::filter(ticker == "KO")  %>%
    dplyr::select(tradeDate, ret_1, GEX, VEX, CEX, GEX_spx, VEX_spx, CEX_spx) %>% 
    pivot_longer(-c(tradeDate, ret_1)) %>% group_by(name) %>% 
    mutate(ntile_ret = ntile(abs(ret_1), 8), bin=ntile(value,8), Year=year(tradeDate)) %>%
    group_by(name, bin) %>%
    reframe(
        avg_abs_ret=mean(ntile_ret, na.rm=T), sb_abs_ret=sd(ntile_ret, na.rm=T)/sqrt(n())
    ) %>% ggplot(aes(x=bin, y=avg_abs_ret, ymin = avg_abs_ret-sb_abs_ret*2, ymax=avg_abs_ret+sb_abs_ret*2)) + geom_line(color="gray") + geom_point() + geom_errorbar(width=0.1) + facet_wrap(~name)
# Scatterplot
all_GEXs_spx %>% dplyr::filter(ticker == "AMD") %>% 
    dplyr::select(tradeDate, ret_1, GEX, VEX, CEX, GEX_spx, CEX_spx, VEX_spx) %>%  
    pivot_longer(-c(tradeDate, ret_1)) %>% 
    ggplot(aes(value, ret_1)) + geom_vline(xintercept = 0)+ geom_point(size=0.1) + facet_wrap(~name, scales="free")
# All binning
all_GEXs_spx %>% dplyr::filter(!ticker %in% c("SPX", "SPY")) %>% 
    dplyr::select(tradeDate, ticker, ret_1f_abs, GEX, VEX, CEX, DEX, GEX_spx, VEX_spx, CEX_spx, DEX_spx) %>% 
    pivot_longer(-c(tradeDate, ret_1f_abs, ticker)) %>% group_by(name, ticker) %>% 
    mutate(ntile_ret = ntile(ret_1f_abs, 8), bin=ntile(value,8), Year=year(tradeDate)) %>%
    group_by(name, ticker, bin) %>%
    reframe(
        avg_abs_ret=mean(ntile_ret, na.rm=T)
    ) %>% ggplot(aes(x=bin, y=avg_abs_ret, color=ticker, group=ticker)) + geom_line(color="gray") + geom_point() + facet_wrap(~name)



# Compute VIX quartiles
{
q1 <- quantile(GEXplus$VIX, 0.25, na.rm=TRUE)
q2 <- quantile(GEXplus$VIX, 0.50, na.rm=TRUE)
df_plot_vix <- GEXplus %>%
    mutate(vix_group = case_when(
        VIX < q1 ~ "VIX <Q1",
        VIX >= q1 & VIX < q2 ~ "VIX Q1-Q2",
        TRUE ~ NA_character_
    )) %>%
    filter(!is.na(vix_group))
p1 <- ggplot(df_plot_vix,
       aes(x = ret,
           color = vix_group)) +
    geom_density(linewidth=1.1) +
    scale_x_continuous(
        labels = scales::percent,
        limits = c(-0.03,0.03)
    ) +
    labs(
        title="SPX Distributions On Lowest Quartiles of VIX",
        x="1-day SPX Return",
        y=""
    ) +
    theme_minimal(base_size=15)

q3 <- quantile(GEXplus$GEX, 0.75, na.rm=TRUE)
q2 <- quantile(GEXplus$GEX, 0.50, na.rm=TRUE)
df_plot <- GEXplus %>%
    mutate(gex_group = case_when(
        GEX > q3 ~ "GEX >Q3",
        GEX >= q2 & GEX < q3 ~ "GEX Q2-Q3",
        TRUE ~ NA_character_
    )) %>%
    filter(!is.na(gex_group))
p2 <- ggplot(df_plot,
       aes(x = ret,
           color = gex_group)) +
    geom_density(linewidth=1.1) +
    scale_x_continuous(
        labels = scales::percent,
        limits = c(-0.03,0.03)
    ) +
    labs(
        title="SPX Distributions On Lowest Quartiles of VIX",
        x="1-day SPX Return",
        y=""
    ) +
    theme_minimal(base_size=15)
p1 / p2
}



# Model
df_m <- all_GEXs %>% filter(ticker=="GLD") %>% 
    mutate(
        zGEX=scale(GEX),
        zVEX=scale(VEX),
        zCEX=scale(CEX)
    )
lm(log(abs(ret)+1e-4) ~ zGEX + zVEX + zCEX,
   data=df_m) %>% summary

# all VIX vs ret
GEXplus %>% dplyr::select(tradeDate, VIX1D:ret) %>% pivot_longer(-c(tradeDate, ret)) %>% 
    ggplot(aes(log(value), abs(ret))) + geom_point() + geom_smooth(method="lm") + facet_wrap(~name, scales="free") #+ ylim(c(-0, 0.05))

# intraday dips?
{
    df_intra <- read_csv("Downloads/amd_intraday-15min_historical-data-05-17-2026.csv")
    df_intra <- df_intra %>% mutate(tradeDate=as.Date(Time)) %>% 
        group_by(tradeDate) %>% reframe(dip = log(last(Latest)-min(Latest))) 
    df_intra <- full_join(ORATS_GEX[["AMD"]], df_intra) %>% mutate(dip_1 = lead(dip))
    plot(df_intra$VEX, df_intra$dip_1)
}
