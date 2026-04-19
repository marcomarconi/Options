functions{
    
    real multi_skew_normal_lpdf(vector y, vector xi, vector alpha, vector omega, matrix L) {
         return(multi_normal_cholesky_lpdf(y | xi, diag_pre_multiply(omega, L)) +
                normal_lcdf(dot_product(alpha, (y - xi) ./ omega) | 0, 1));
    }
    
    vector multi_skew_normal_rng(vector xi, vector alpha, vector omega, matrix L) {
        int K = dims(xi)[1];
        matrix[K,K] Rho = L * L';
        real aCa = alpha' * Rho * alpha;
        vector[K] delta = (1 / sqrt(1 + aCa)) * Rho * alpha;
        matrix[K+1, K+1] cov_star =  append_row(append_col(1, delta'), 
                                     append_col(delta, Rho));
        vector[K+1] y_tmp = multi_normal_rng(rep_vector(0, K+1), cov_star);
        real ind = (y_tmp[1] > 0) ? 1 : -1;
        return(xi +  y_tmp[2:(K+1)] * ind .* omega);
    }
}

data {
  int<lower=0> N;
  int<lower=2> K;
  int<lower=0> M;
  array[N] vector[K] X1;
  array[N] vector[K] X2;
  array[N] vector[K] X3;
  array[N] vector[K] Y;
}
parameters {
  vector[K] xi_I; 
  vector[M] xi_B;
  vector[K] omega_I;
  vector[M] omega_B;
  real alpha;
  cholesky_factor_corr[K] L;
}
transformed parameters{
    corr_matrix[K] Rho = L * L';
}
model {
  alpha ~ normal(0,1)  ;
  xi_I ~ normal(0, 1);
  xi_B ~ normal(0, 1);
  omega_I ~ normal(0, 1);
  omega_B ~  normal(0, 1);
  L ~ lkj_corr_cholesky(K);
  vector[K] alpha_k = rep_vector(alpha, K);
  for(i in 1:N) {
      vector[K] xi = xi_I + X1[i] * xi_B[1] + X2[i] * xi_B[2] + X3[i] * xi_B[3];
      vector[K] omega = exp(omega_I + X1[i] * omega_B[1] + X2[i] * omega_B[2] + X3[i] * omega_B[3]);
      Y[i] ~ multi_skew_normal(xi, alpha_k, omega, L);
  }
}
generated quantities{
    array[N] real log_lik;
    array[N] vector[K] Y_hat;
    array[N] vector[K] Y_res;
    vector[K] alpha_k = rep_vector(alpha, K);
    for (i in 1:N) {
        vector[K] xi = xi_I + X1[i] * xi_B[1] + X2[i] * xi_B[2] + X3[i] * xi_B[3];
        vector[K] omega = exp(omega_I + X1[i] * omega_B[1] + X2[i] * omega_B[2] + X3[i] * omega_B[3]);
        log_lik[i] = multi_skew_normal_lpdf(Y[i] | xi, alpha_k, omega, L) ;
        Y_hat[i] = multi_skew_normal_rng(xi, alpha_k, omega, L) ; 
        Y_res[i] = Y[i] - Y_hat[i];
    }
    
}
