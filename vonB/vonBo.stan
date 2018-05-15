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
  cholesky_factor_corr[3] L_Omega; // prior correlation, Cholesky scale
  matrix[3, nSites] mu_beta_raw; // This will be transformed into mu_beta
  vector<lower=0>[3] tau; // prior scale
  real<lower = 0> sigmaLength; // observation error
  vector[3] mu_gamma; // group coeffs
  /* vector[3] mu_gammaSD; // group coeffs */
}
transformed parameters {
  matrix[3, nSites] mu_beta_cor = // correlated site-level variation, without mu_gamma
    diag_pre_multiply(tau, L_Omega) * mu_beta_raw;
    // This is part of the MVN non-central parameterization
    // The mean vector (mu_gamma) still needs to be added, but that is done below
  row_vector[nSites]  Linf = //theoretical maximum length
    exp(mu_beta_cor[1] + mu_gamma[1]);
  row_vector[nSites]  K = // growth coefficient
    exp(mu_beta_cor[2]  + mu_gamma[2]);
  row_vector[nSites]  t0 = // hypothetical age at which fish's size = 0
    exp(mu_beta_cor[3] + mu_gamma[3]) - 10.0;
}
model{
  row_vector[nFish] vonBplaceholder =
    Linf[poolID]  .*  (1.0 - exp( - K[poolID] .* ( age - t0[poolID])));
  L_Omega ~ lkj_corr_cholesky(hp_omega);
  to_vector(mu_beta_raw) ~ normal(0,1);
  tau ~ exponential(1/hp_tau);
  sigmaLength ~ exponential(1/hp_sigma);
  mu_gamma ~ normal( p_mu_gamma, p_mu_gammaSD);
  length ~ normal(vonBplaceholder, sigmaLength);
}
generated quantities{

  real<lower = 0>  Linf_bar;
  real  t0_bar;
  real  K_bar;
  real  M[nSites];
  real  M_bar;
  matrix[nSites, nProject] siteProjections;
  vector[nProject] hyperProjection;
  
  Linf_bar = exp(mu_gamma[1]);
  K_bar    = exp(mu_gamma[2]);
  t0_bar   = exp(mu_gamma[3]) - 10.0 ;

  // Use model to make projections
  for(site in 1:nSites){
    // Estimate M from Then et al. 2014
    // * 100 converts from m to cm
    M[site] =  (4.118 * (K[site] ^ (0.73))) * (Linf[site] * 100) ^(-0.33);
    // create projections for each site
    for( p in 1:nProject){
      siteProjections[ site, p] = Linf[site] * (1.0 - exp(-1.0 * K[site] * (ageProject[p] - t0[site])));
    }
  }

  // Estimate M using hyperparameters
  M_bar =  (4.118 * (K_bar ^ (0.73))) * (Linf_bar * 100) ^(-0.33);
  
  // simulate using equations
  hyperProjection = Linf_bar * (1.0 - exp(-1.0 * K_bar * (ageProject - t0_bar)));

}
