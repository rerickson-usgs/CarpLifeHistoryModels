data {
  int<lower=0> N; // num individuals
  vector[N] x; // individual predictors
  vector[N] y; // outcomes
}
parameters {
  real beta0;
  real beta1;
  real<lower = 0> sigma;
}
model {
  y ~ normal(beta0 + beta1 * x, sigma);
}
