data {
  int<lower=0> N;
  int<lower=0> N_pools;
  matrix[N, N_pools] X_1;
  matrix[N, N_pools] X_2;
  int<lower=0,upper=1> y[N];
  int<lower=0> N_project;
  vector[N_project] x_project;
}
parameters {
  vector[N_pools] alpha;
  vector[N_pools] beta;
  real alpha_hyper;
  real<lower = 0> alpha_hyper_sd;
  real beta_hyper;
  real<lower = 0> beta_hyper_sd;
}
model {
  alpha ~ normal( alpha_hyper, alpha_hyper_sd);
  beta ~ normal( beta_hyper, beta_hyper_sd);
  y ~ bernoulli_logit( X_1 *alpha + X_2 * beta);
}
generated quantities{
  matrix[N_project, N_pools] y_project_pools;
  vector[N_project] y_project_hyper;
  
  for(n_idx in 1:N_project){
    // Loop through pools
    for(p_idx in 1:N_pools){
      y_project_pools[n_idx, p_idx] =
	inv_logit(alpha[p_idx] +
		  x_project[n_idx] * beta[p_idx]);
    }
    // calc hyper
      y_project_hyper[n_idx] =
	inv_logit(alpha_hyper +
		  x_project[n_idx] * beta_hyper);
  }
}

