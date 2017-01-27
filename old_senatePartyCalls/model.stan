functions {
  vector rescale(vector x) {
    return((x - mean(x)) / sd(x));
  }
}
data {
  int<lower = 1> N;
  int<lower = 1> K_f;
  int<lower = 1> K_r;
  int<lower = 1> J;
  int<lower = 1> L;
  int<lower = 1, upper = J> jj[N];
  matrix[N, K_f] X_f;
  matrix[N, K_r] X_r;
  matrix[J, L] u;
  vector[N] y;
}
parameters {
  vector[K_f] beta_f;
  real<lower = 0> sigma;

  vector<lower = 0>[K_r] tau;
  matrix[L, K_r] gamma;
  matrix[K_r, J] z;
  cholesky_factor_corr[K_r] L_Omega;
}
transformed parameters {
  matrix[J, K_r] b_r;
  b_r = u * gamma + (diag_pre_multiply(tau, L_Omega) * z)';
}
model {
  beta_f ~ normal(0, 10);
  sigma ~ cauchy(0, 2.5);
  tau ~ cauchy(0, 2.5);
  to_vector(gamma) ~ normal(0, 5);
  to_vector(z) ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  {
    vector[N] x_beta_jj;
    for (n in 1:N) {
      x_beta_jj[n] = dot_product(X_r[n], b_r[jj[n]]);
    }
    y ~ normal(x_beta_jj + X_f * beta_f, sigma);
  }
}
