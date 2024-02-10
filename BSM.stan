data {
  int<lower=0> N;
  vector[N] calls;
  vector[N] puts;
  vector[N] K;
  real S;
  real tt;
  real r;
}

transformed data {
    
}

parameters {
  real<lower=0> v;
  real<lower=0> sigma;
}
transformed parameters {
  vector[N] C;
  vector[N] P;
  vector[N] d1;
  vector[N] d2;
  d1 = (log(S/K) + (r + v^2/2)*tt) / (v*sqrt(tt));
  d2 = d1 - v*sqrt(tt);
  C = Phi(d1) * S - Phi(d2) .* K * exp(-r * tt);
  P = Phi(-d2) .* K *  exp(-r * tt) - Phi(-d1) * S;
}



model {
  v ~ exponential(1);
  sigma ~ exponential(1);
  for(i in 1:N) {
      if(calls[i] > 0)
            calls[i] ~ lognormal(log(C[i]), sigma);
      if(puts[i] > 0)
            puts[i] ~ lognormal(log(P[i]), sigma);
  }
}


