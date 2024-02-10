library(tidyverse)
library(derivmkts)
library(cmdstanr)
{
N <- 100
S <- 100
v <- 0.3
K <- seq(0.1, 150, length.out=N)
r <- 0
tt <- 1
d <- 0
e <- rnorm(length(K), 0, 0.1)
calls <- (((bscall(S, K, v, r, tt, d) %>% log)+e) %>% exp) 
puts <- (((bsput(S, K, v, r, tt, d) %>% log)+e) %>% exp ) 

mod <- cmdstan_model("BSM.stan")
fit <- mod$sample(
  data = list(N=N, calls=calls, puts=puts, K=K, S=S, tt=tt, v=v),
  chains = 1,
  refresh = 500 # print update every 500 iters
)
}
spy <- read_csv("~/Downloads/spy_month.csv")
N <- nrow(spy) / 2
fit <- mod$sample(
    data = list(N=N, calls=spy[spy$Type=="Call",]$Last, puts=spy[spy_m$Type=="Put",]$Last, 
                K=spy[spy$Type=="Call",]$Strike, S=476.68, tt=1/12),
    chains = 1,
    refresh = 500 # print update every 500 iters
)

spy <- read_csv("~/Downloads/spy_year.csv")
N <- nrow(spy) / 2
fit <- mod$sample(
    data = list(N=N, calls=spy[spy$Type=="Call",]$Last, puts=spy[spy$Type=="Put",]$Last, 
                K=spy[spy$Type=="Call",]$Strike, S=476.68, tt=1, r=0.045),
    chains = 1,
    refresh = 500 # print update every 500 iters
)
