library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

n_iter  <- 10000
## Read in a format data
dat <- fread("./data_use.csv")

## Order pools
new_pool_order <-
    c("Pool 14",
      "Pool 16",
      "Pool 17",
      "Pool 18",
      "Pool 19",
      "Pool 20",
      "Pool 22",
      "Pool 24",
      "Pool 26",
      "Pool 27",
      "Alton",
      "LaGrange",
      "Peoria",
      "Starved Rock",
      "Marseilles",
      "Dresden Island",
      "JT Myers",
      "Newburgh",
      "Cannelton",
      "McAlpine",
      "Markland",
      "Meldahl",
      "RC Byrd"
      )

dat[ , Pool := factor(Pool,
                      levels = new_pool_order
                      )]

dat2 <- dat[ !is.na(TL) & !is.na(Age), ]


## Silver carp analysis
dat3_silver <- dat2[ Species == "Silver", ]
dat3_silver[ , Pool := droplevels(Pool)]
dat3_silver[ , PoolID := as.numeric(Pool)]
ageProjection = seq(0, 20, by = 1)
dat3_silver_pool_key <-
    dat3_silver[ , .(PoolID = mean(PoolID)), by = .( System, Pool)]

## placeholder for inputs

stanData_silver <- list(
    nFish  = dim(dat3_silver)[1],
    nSites = length(dat3_silver[, unique(PoolID)]),
    length = dat3_silver[ , TLm],
    poolID = dat3_silver[ , PoolID],
    age = dat3_silver[ , Age],
    hp_tau = 1.5,
    hp_sigma = 10,
    hp_omega = 2,
    p_mu_gamma = 0,
    p_mu_gammaSD = 2,
    nProject = length(ageProjection),
    ageProject = ageProjection
    )

## Model takes ~0.25 hrs to run.
stan_vonBNot0 <-
    stan_model(file = "vonBoNot0.stan")

stanOutO_silver  <-
    rstan::sampling(stan_vonBNot0,
                     data = stanData_silver, chains = 4, iter = n_iter,
                     control = list(adapt_delta = 0.8))
save(dat3_silver,
     stanOutO_silver,
     stanData_silver,
     dat3_silver_pool_key,
     file = "vonBfitNot0_silver.RData")


## Bighead carp bighead analysis
dat3_bighead <- dat2[ Species == "Bighead", ]
dat3_bighead[ , Pool := droplevels(Pool)]
dat3_bighead[ , PoolID := as.numeric(Pool)]
ageProjection = seq(0, 20, by = 1)
dat3_bighead_pool_key <-
    dat3_bighead[ , .(PoolID = mean(PoolID)), by = .( System, Pool)]

stanData_bighead <- list(
    nFish  = dim(dat3_bighead)[1],
    nSites = length(dat3_bighead[, unique(PoolID)]),
    length = dat3_bighead[ , TLm],
    poolID = dat3_bighead[ , PoolID],
    age = dat3_bighead[ , Age],
    hp_tau = 1.5,
    hp_sigma = 10,
    hp_omega = 2,
    p_mu_gamma = 0,
    p_mu_gammaSD = 2,
    nProject = length(ageProjection),
    ageProject = ageProjection
    )

## Model takes ~0.25 hrs to run.
stan_vonBNot0 <-
    stan_model(file = "vonBoNot0.stan")

stanOutO_bighead  <-
    rstan::sampling(stan_vonBNot0,
                     data = stanData_bighead, chains = 4, iter = n_iter,
                     control = list(adapt_delta = 0.8))
save(stanOutO_bighead,
     stanData_bighead,
     dat3_bighead_pool_key,
     file = "vonBfitNot0_bighead.RData")
