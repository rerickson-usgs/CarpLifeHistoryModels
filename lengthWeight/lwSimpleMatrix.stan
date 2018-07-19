data {
  int<lower = 0> N; // num individuals
  int<lower = 0> K; // num ind predictors 
  matrix[N, K] x; // individual predictors
  vector[N] y; // outcomes
}
parameters {
  vector[K] beta;
  real<lower = 0> sigma;
}
model {
  y ~ normal(  x * beta, sigma);
}
