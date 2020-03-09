library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

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
dat3_silver <- dat2[ Species == "Silver", ]

dat3_silver[ , Pool := droplevels(Pool)]
dat3_silver[ , PoolID := as.numeric(Pool)]

ageProjection = seq(0, 20, by = 1)

dat3_silver[ , .(PoolID = mean(PoolID)), by = .(Pool)]

dat3_silver_pool_key <-
    dat3_silver[ , .(PoolID = mean(PoolID)), by = .(Pool, Pool)]

stanData_silver <- list(
    nFish  = dim(dat3_silver)[1],
    nSites = length(dat3_silver[, unique(PoolID)]),
    length = dat3_silver[ , TLm],
    poolID = dat3_silver[ , PoolID],
    age = dat3_silver[ , Age3],
    hp_tau = 1.5,
    hp_sigma = 10,
    hp_omega = 2,
    p_mu_gamma = 0,
    p_mu_gammaSD = 2,
    nProject = length(ageProjection),
    ageProject = ageProjection
    )

## Model takes ~0.25 hrs to run.
if(rerunStan){
    stanOutO_silver <- stan(file = "vonBoNot0.stan",
                          data = stanData_silver, chains = 4, iter = 6000,
                          control = list(adapt_delta = 0.8))
    save(stanOutO_silver, file = "vonBfitNot0_silver.RData")
} else {
    load("vonBfitNot0_silver.RData")
}

## stanOutO_SVCP
stanOutOsummary_SVCP <-
    summary(stanOutO_SVCP, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutOsummary_SVCP[[1]][grepl("M", rownames(summary(stanOutO_SVCP)[[1]])), ]

stanOutOsummary_SVCP[[1]][grepl("bar", rownames(summary(stanOutO_SVCP)[[1]])), ]
stanOutOsummary_SVCP[[1]][grepl("K", rownames(summary(stanOutO_SVCP)[[1]])), ]
stanOutOsummary_SVCP[[1]][grepl("Linf", rownames(summary(stanOutO_SVCP)[[1]])),]


plot(stanOutO_SVCP, pars =c("Linf_bar", "K_bar", "Linf"))

## Bighead carp analysis
dat3_BHCP <- dat2[ Species == "BHCP", ]
dat3_BHCP[ , Pool := droplevels(Pool)]
dat3_BHCP[ , PoolID := as.numeric(Pool)]

fwrite(file ="BHCP_vonBkey.csv",
       x = dat3_BHCP[ , .(PoolID = mean(PoolID)) , by = Pool])

stanData_BHCP <- list(
    nFish  = dim(dat3_BHCP)[1],
    nSites = length(dat3_BHCP[, unique(PoolID)]),
    length = dat3_BHCP[ , TLm],
    poolID = dat3_BHCP[ , PoolID],
    age = dat3_BHCP[ , Age3],
    hp_tau = 1.5,
    hp_sigma = 10,
    hp_omega = 2,
    p_mu_gamma = 0,
    p_mu_gammaSD = 2,
    nProject = length(ageProjection),
    ageProject = ageProjection
    )

## Model takes ~0.25 hrs to run.
if(rerunStan){
    stanOutO_BHCP <- stan(file = "vonBoNot0.stan",
                          data = stanData_BHCP, chains = 4, iter = 6000,
                          control = list(adapt_delta = 0.8))
    save(stanOutO_BHCP, file = "vonBfitNot0_BHCP.RData")
} else {
    load("vonBfitNot0_BHCP.RData")
}


stanOutOsummary_BHCP <-
    summary(stanOutO_BHCP, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))

stanOutOsummary_BHCP[[1]][grepl("M", rownames(summary(stanOutO_BHCP)[[1]])), ]

stanOutOsummary_BHCP[[1]][grepl("bar", rownames(summary(stanOutO_BHCP)[[1]])), ]
stanOutOsummary_BHCP[[1]][grepl("K", rownames(summary(stanOutO_BHCP)[[1]])), ]
stanOutOsummary_BHCP[[1]][grepl("Linf", rownames(summary(stanOutO_BHCP)[[1]])),]


plot(stanOutO_BHCP, pars =c("Linf_bar", "K_bar", "Linf"))
