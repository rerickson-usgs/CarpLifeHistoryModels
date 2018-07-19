data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0,upper=1> y[N];
  int<lower=0> nProject;
  vector[nProject] xProject;
}
parameters {
  real alpha;
  real beta;
}
model {
  y ~ bernoulli_logit(alpha + beta * x);
}
generated quantities{
  vector[nProject] yProject;

  for(n in 1:nProject){
    yProject[n] = inv_logit(alpha + beta * xProject[n]);
  }
  
}
