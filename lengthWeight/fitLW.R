## Load required libraires
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

n_iter  <- 2000

## Read in a format data
dat <- fread("./data_use.csv")
dat[ , Sampdate :=ymd(Sampdate)]

dat2 <- dat[ !is.na(TL) & !is.na(WT), ]

## Format data for silver carp
dat3_silver <- dat2[ Species == "Silver", ]
dat3_silver[ , Pool := factor(Pool)]
dat3_silver[ , PoolID := as.numeric(Pool)]

## Creat group-level predictors
dat3_silver[ , Pool := factor(Pool)]
dat3_silver[ , PoolID := as.numeric(Pool)]

dat3_silver_pool_key <-
    dat3_silver[ , .(PoolID = mean(PoolID)), by = Pool]

## Log10 transorm data and center it
dat3_silver[ , TLmL10 := log10(TLm)] # may try centering here
dat3_silver[ , WTkgL10 := log10(WTkg)]

## u_silver
u_silver  = matrix(rep(1, length(dat3_silver[, unique(PoolID)])), ncol = 1)

## run model with correlated structure
x_silver  = dat3_silver[ , model.matrix( ~ TLmL10)]
head(x_silver)

xProject_silver_raw <- seq(0.001, 1.5, length.out = 100)
xProject_silver <- log10(xProject_silver_raw)

## Run model without sex
stanData_silver <- list(
    N  = dim(dat3_silver)[1], # Num obs
    J  = length(dat3_silver[, unique(PoolID)]), # num groups
    L  = 1, # num group predictors
    y  = dat3_silver[ , WTkg], # observations
    jj = dat3_silver[ , PoolID], # groups for each indivdual
    x  = x_silver, # individual predictor matrix
    u  = u_silver, # group predictors
    K  = ncol(x_silver), # num individual predictors
    xProject = xProject_silver,
    nProject = length(xProject_silver)
)

lw_model <- stan_model(file = "lengthWeight.stan")

stanOut_silver <- sampling(lw_model,
                           data = stanData_silver,
                           chains = 4, iter = n_iter,
                           control = list(adapt_delta = 0.8))
save(stanOut_silver, dat3_silver_pool_key, stanData_silver,
     file = "lengthWeight_silver.RData")


## Format data for bighead carp
dat3_bighead <- dat2[ Species == "Bighead", ]
dat3_bighead[ , Pool := factor(Pool)]
dat3_bighead[ , PoolID := as.numeric(Pool)]

## Creat group-level predictors
dat3_bighead[ , Pool := factor(Pool)]
dat3_bighead[ , PoolID := as.numeric(Pool)]

dat3_bighead_pool_key <-
    dat3_bighead[ , .(PoolID = mean(PoolID)), by = Pool]

## Log10 transorm data and center it
dat3_bighead[ , TLmL10 := log10(TLm)] # may try centering here
dat3_bighead[ , WTkgL10 := log10(WTkg)]

## u_bighead
u_bighead  = matrix(rep(1, length(dat3_bighead[, unique(PoolID)])), ncol = 1)

## run model with correlated structure
x_bighead  = dat3_bighead[ , model.matrix( ~ TLmL10)]
head(x_bighead)

xProject_bighead_raw <- seq(0.001, 1.5, length.out = 100)
xProject_bighead <- log10(xProject_bighead_raw)

## Run model without sex
stanData_bighead <- list(
    N  = dim(dat3_bighead)[1], # Num obs
    J  = length(dat3_bighead[, unique(PoolID)]), # num groups
    L  = 1, # num group predictors
    y  = dat3_bighead[ , WTkg], # observations
    jj = dat3_bighead[ , PoolID], # groups for each indivdual
    x  = x_bighead, # individual predictor matrix
    u  = u_bighead, # group predictors
    K  = ncol(x_bighead), # num individual predictors
    xProject = xProject_bighead,
    nProject = length(xProject_bighead)
)

lw_model <- stan_model(file = "lengthWeight.stan")

stanOut_bighead <- sampling(lw_model,
                           data = stanData_bighead,
                           chains = 4, iter = n_iter,
                           control = list(adapt_delta = 0.8))
save(stanOut_bighead, dat3_bighead_pool_key, stanData_bighead,
     file = "lengthWeight_bighead.RData")

