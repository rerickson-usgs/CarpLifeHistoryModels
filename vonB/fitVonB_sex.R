library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

n_iter  <- 600
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


## Silver carp silver analysis
dat3_silver_male <- dat2[ Species == "Silver" & Sex %in% c("Male", "Unkown"), ]
dat3_silver_male[ , Pool := droplevels(Pool)]
dat3_silver_male[ , PoolID := as.numeric(Pool)]
ageProjection = seq(0, 20, by = 1)
dat3_silver_male_pool_key <-
    dat3_silver_male[ , .(PoolID = mean(PoolID)), by = .( System, Pool)]

stanData_silver_male <- list(
    nFish  = dim(dat3_silver_male)[1],
    nSites = length(dat3_silver_male[, unique(PoolID)]),
    length = dat3_silver_male[ , TLm],
    poolID = dat3_silver_male[ , PoolID],
    age = dat3_silver_male[ , Age],
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

stanOutO_silver_male  <-
    rstan::sampling(stan_vonBNot0,
                     data = stanData_silver_male, chains = 4, iter = n_iter,
                     control = list(adapt_delta = 0.8))
save(stanOutO_silver_male,
     stanData_silver_male,
     dat3_silver_male_pool_key,
     file = "vonBfitNot0_silver_male.RData")

## Silver carp silver analysis
dat3_silver_female <- dat2[ Species == "Silver" & Sex %in% c("Female", "Unkown"), ]
dat3_silver_female[ , Pool := droplevels(Pool)]
dat3_silver_female[ , PoolID := as.numeric(Pool)]
ageProjection = seq(0, 20, by = 1)
dat3_silver_female_pool_key <-
    dat3_silver_female[ , .(PoolID = mean(PoolID)), by = .( System, Pool)]

stanData_silver_female <- list(
    nFish  = dim(dat3_silver_female)[1],
    nSites = length(dat3_silver_female[, unique(PoolID)]),
    length = dat3_silver_female[ , TLm],
    poolID = dat3_silver_female[ , PoolID],
    age = dat3_silver_female[ , Age],
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

stanOutO_silver_female  <-
    rstan::sampling(stan_vonBNot0,
                     data = stanData_silver_female, chains = 4, iter = n_iter,
                     control = list(adapt_delta = 0.8))
save(stanOutO_silver_female,
     stanData_silver_female,
     dat3_silver_female_pool_key,
     file = "vonBfitNot0_silver_female.RData")


## Bighead carp bighead analysis
dat3_bighead_male <- dat2[ Species == "Bighead" & Sex %in% c("Male", "Unkown"), ]
dat3_bighead_male[ , Pool := droplevels(Pool)]
dat3_bighead_male[ , PoolID := as.numeric(Pool)]
ageProjection = seq(0, 20, by = 1)
dat3_bighead_male_pool_key <-
    dat3_bighead_male[ , .(PoolID = mean(PoolID)), by = .( System, Pool)]

stanData_bighead_male <- list(
    nFish  = dim(dat3_bighead_male)[1],
    nSites = length(dat3_bighead_male[, unique(PoolID)]),
    length = dat3_bighead_male[ , TLm],
    poolID = dat3_bighead_male[ , PoolID],
    age = dat3_bighead_male[ , Age],
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

stanOutO_bighead_male  <-
    rstan::sampling(stan_vonBNot0,
                     data = stanData_bighead_male, chains = 4, iter = n_iter,
                     control = list(adapt_delta = 0.8))
save(stanOutO_bighead_male,
     stanData_bighead_male,
     dat3_bighead_male_pool_key,
     file = "vonBfitNot0_bighead_male.RData")

## Bighead carp bighead analysis
dat3_bighead_female <- dat2[ Species == "Bighead" & Sex %in% c("Female", "Unkown"), ]
dat3_bighead_female[ , Pool := droplevels(Pool)]
dat3_bighead_female[ , PoolID := as.numeric(Pool)]
ageProjection = seq(0, 20, by = 1)
dat3_bighead_female_pool_key <-
    dat3_bighead_female[ , .(PoolID = mean(PoolID)), by = .( System, Pool)]

stanData_bighead_female <- list(
    nFish  = dim(dat3_bighead_female)[1],
    nSites = length(dat3_bighead_female[, unique(PoolID)]),
    length = dat3_bighead_female[ , TLm],
    poolID = dat3_bighead_female[ , PoolID],
    age = dat3_bighead_female[ , Age],
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

stanOutO_bighead_female  <-
    rstan::sampling(stan_vonBNot0,
                     data = stanData_bighead_female, chains = 4, iter = n_iter,
                     control = list(adapt_delta = 0.8))
save(stanOutO_bighead_female,
     stanData_bighead_female,
     dat3_bighead_female_pool_key,
     file = "vonBfitNot0_bighead_female.RData")
