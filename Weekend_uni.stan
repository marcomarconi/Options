data {
  int<lower=0> N;
  int<lower=0> M;
  int<lower=0> J;
  vector[N] y;
  matrix[N, M] X;
  vector[N] k; // Asset
  int family;
}

parameters {
  real mu_I;
  vector[M] mu_B;
  real sigma_I;
  vector[M] sigma_B;
  real alpha;
  real<lower=0, upper=1> tau;
}

model {
    mu_I ~ normal(0, 1);
    mu_B ~ normal(0, 1);
    sigma_I ~ normal(0, 1);
    sigma_B ~  normal(0, 1);
    alpha ~ normal(0, 1);
    tau ~ beta(1, 1);
    {
        for(i in 1:N) {
            real mu = mu_I + X[i,] * mu_B;
            real sigma = exp(sigma_I + X[i,] * sigma_B);
            if(family == 0)
                y[i] ~ normal(mu, sigma);
            else if (family == 1)
                y[i] ~ skew_normal(mu, sigma, alpha); 
            else if (family == 2)
                y[i] ~ skew_double_exponential(mu, sigma, tau); 
        }
    }
}

generated quantities{
    array[N] real log_lik;
    array[N] real y_hat;
    for(i in 1:N) {
        real mu = mu_I + X[i,] * mu_B;
        real sigma = exp(sigma_I + X[i,] * sigma_B);
        if(family == 0) {
            log_lik[i] = normal_lpdf(y[i] | mu, sigma);
            y_hat[i] = normal_rng(mu, sigma);
        }
        else if (family == 1) {
            log_lik[i] = skew_normal_lpdf(y[i] | mu, sigma, alpha);
            y_hat[i] = skew_normal_rng(mu, sigma, alpha);
        }
        else if (family == 2) {
            log_lik[i] = skew_double_exponential_lpdf(y[i] | mu, sigma, tau);
            y_hat[i] = skew_double_exponential_rng(mu, sigma, tau);
        }
    }
    
}
