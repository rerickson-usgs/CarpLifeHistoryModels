data {
  int<lower = 0>           nFish; // number of fish
  int<lower = 0>           nSites; // number of sites
  int<lower = 0>           poolID[nFish] ; // Dummy variable to ID pool
  row_vector<lower = 0>[nFish] age; // Age of fish
  row_vector<lower = 0>[nFish] length; // Length of fish
 // Hyperparameters below:
  real hp_tau;  // default 2.5
  real hp_sigma;  // unsure what default is
  real hp_omega; // default 2
  // prior for mu_gamma
  real p_mu_gamma;
  real p_mu_gammaSD;
  int<lower = 0> nProject; // number of projection points
  vector[nProject] ageProject; // fish ages to simulate lengths for
}
parameters {
  cholesky_factor_corr[2] L_Omega; // prior correlation, Cholesky scale
  matrix[2, nSites] mu_beta_raw; // This will be transformed into mu_beta
  vector<lower=0>[2] tau; // prior scale
  real<lower = 0> sigmaLength; // observation error
  vector[2] mu_gamma; // group coeffs
}
transformed parameters {
  matrix[2, nSites] mu_beta_cor = // correlated site-level variation, without mu_gamma
    diag_pre_multiply(tau, L_Omega) * mu_beta_raw;
    // This is part of the MVN non-central parameterization
    // The mean vector (mu_gamma) still needs to be added, but that is done below
  row_vector[nSites]  Linf = //theoretical maximum length
    exp(mu_beta_cor[1] + mu_gamma[1]);
  row_vector[nSites]  K = // growth coefficient
    exp(mu_beta_cor[2]  + mu_gamma[2]);
}
model{
  row_vector[nFish] vonBplaceholder =
    Linf[poolID]  .*  (1.0 - exp( - K[poolID] .* ( age )));
  L_Omega ~ lkj_corr_cholesky(hp_omega);
  to_vector(mu_beta_raw) ~ normal(0,1);
  tau ~ exponential(1/hp_tau);
  sigmaLength ~ exponential(1/hp_sigma);
  mu_gamma ~ normal( p_mu_gamma, p_mu_gammaSD);
  length ~ normal(vonBplaceholder, sigmaLength);
}
generated quantities{

  real<lower = 0>  Linf_bar;
  /* real  t0_bar; */
  real  K_bar;
  real  M[nSites];
  real  M_bar;
  matrix[nSites, nProject] siteProjections;
  vector[nProject] hyperProjection;
  
  Linf_bar = exp(mu_gamma[1]);
  K_bar    = exp(mu_gamma[2]);
  /* t0_bar   = exp(mu_gamma[3]) - 10.0 ; */

  // Use model to make projections
  for(site in 1:nSites){
    // Estimate M from Then et al. 2014
    // * 100 converts from m to cm
    M[site] =  (4.118 * (K[site] ^ (0.73))) * (Linf[site] * 100) ^(-0.33);
    // create projections for each site
    for( p in 1:nProject){
      siteProjections[ site, p] = normal_rng(Linf[site] * (1.0 - exp(-1.0 * K[site] * (ageProject[p] ))), sigmaLength);
    }
  }

  // Estimate M using hyperparameters
  M_bar =  (4.118 * (K_bar ^ (0.73))) * (Linf_bar * 100) ^(-0.33);
  
  // simulate using equations
  for( p in 1:nProject){
    hyperProjection[p] = normal_rng(Linf_bar * (1.0 - exp(-1.0 * K_bar * (ageProject[p] ))), sigmaLength);
  }
}
