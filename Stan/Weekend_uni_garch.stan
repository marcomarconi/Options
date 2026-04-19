data {
  int<lower=0> N;
  int<lower=0> M;
  int<lower=0> J;
  vector[N] y;
  matrix[N, M] X;
  vector[N] k; // Asset
  real sigma1;
}

parameters {
  real mu_I;
  vector[M] mu_B;
  real<lower=0>  sigma_I;
  vector<lower=0>[M] sigma_B;
  real<lower=0, upper=1> sigma_r;
  real<lower=0, upper=(1-sigma_r)> sigma_ar;
  real alpha;
}
transformed parameters {
  array[N] real<lower=0> sigma;
  array[N] real mu;
  sigma[1] = sigma1;
  mu[1] = 0;
  for (i in 2:N) {
      mu[i] = mu_I + X[i,] * mu_B;
      sigma[i] = sqrt(sigma_I +
                     sigma_r * pow(y[i - 1] - mu[i], 2) +
                     exp(X[i,]) * sigma_B + 
                     sigma_ar * pow((sigma[i - 1]), 2));
  }
}
model {
    mu_I ~ normal(0, 1);
    mu_B ~ normal(0, 1);
    sigma_I ~ normal(0, 1);
    sigma_B ~  normal(0, 1);
    alpha ~ normal(0, 1);
    {
        for(i in 1:N) {
            y[i] ~ skew_normal(mu[i],  sigma[i] , alpha); 
        }
    }
}

generated quantities{
    array[N] real log_lik;
    array[N] real y_hat;
    for(i in 1:N) {
        log_lik[i] = skew_normal_lpdf(y[i] | mu[i], sigma[i], alpha);
        y_hat[i] = skew_normal_rng(mu[i], sigma[i], alpha);
    }
}
