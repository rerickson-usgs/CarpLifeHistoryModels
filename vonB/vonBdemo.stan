data {
  int<lower = 0>           nFish; // number of fish
  int<lower = 0>           nSites; // number of sites
  int<lower = 0>           poolID[nFish] ; // Dummy variable to ID pool
  vector<lower = 0>[nFish] age; // Age of fish
  real<lower = 0>          length[ nFish]; // Length of fish
}
parameters {
  real<lower = 0> sigmaLength;
  vector[nSites]  Linf;
  vector[nSites]  t0;
  vector[nSites]  K;
}
model {
  vector[nFish] vonBplaceholder;
  for(fish in 1:nFish){
    vonBplaceholder[fish] = Linf[ poolID[fish] ] * (1 - exp( -K[poolID[fish]] * ( age[fish] - t0[poolID[fish]] )));
  }
  // priors 
  Linf ~ normal( 40, 10);
  K    ~ normal( 0, 1);
  t0   ~ normal( 0, 1);

  // estimation
  length ~ normal(  vonBplaceholder, sigmaLength);

}

