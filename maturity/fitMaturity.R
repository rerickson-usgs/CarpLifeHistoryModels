## Load required librarie
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(rstan) # used to fit Bayesian model

n_iter <- 10000
rstan_options(auto_write = TRUE)
## setup to use multiple cores and other local options
## options(mc.cores = parallel::detectCores())
## Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

## Read in a format data
dat <- fread("../Explore_data/data_use.csv")
dat[ , Sampdate :=ymd(Sampdate)]

dat2 <- dat[ !is.na(Maturity) & !is.na(TLm), ]

## Examine by pool to see if we need to pool or can estimate across pools
pools_use_mat_silver <-
    dat2[ , .N, by = .( Pool, Maturity, Species)][
        Species =="Silver" & Maturity == 0, ][, unique(Pool)]

pools_use_mat_bighead <-
    dat2[ , .N, by = .( Pool, Maturity, Species)][
        Species =="Bighead"  & Maturity == 0, ][, unique(Pool)]


dat3_silver <- dat2[ Species == "Silver" & Pool %in% pools_use_mat_silver, ]
dat3_bighead <- dat2[ Species == "Bighead" & Pool %in% pools_use_mat_bighead, ]


## Get things ready for silver carp
## Figure out interaction
## X = data.frame(x = 1:6, site = rep(letters[1:2], each = 3))
## X
## model.matrix( ~ site - 1, X)
## mm_test <- model.matrix( ~ site - 1 + x, X)
## no_x = mm_test[ , colnames(mm_test)!= "x"]
## x = mm_test[ , colnames(mm_test)== "x"]
## no_x * x

## Create predictor variables
X_1_silver  <- model.matrix( ~ Pool -1, dat3_silver)
X_2_silver <- X_1_silver * dat3_silver[ , TLm]

y_silver <-  dat3_silver[ , Maturity]

## Create projection data
data_project_silver <- seq(dat3_silver[, range(TLm)[1]],
                           dat3_silver[, range(TLm)[2]], by = 0.001)


stanData_silver <- list(
    N  = nrow(dat3_silver),
    N_pools = length(pools_use_mat_silver),
    X_1 = X_1_silver,
    X_2 = X_2_silver,
    y = y_silver,
    N_project = length(data_project_silver),
    x_project = data_project_silver
)

mat_model <-
    stan_model(file = "maturity.stan")

stanOut_silver <- rstan::sampling(mat_model,
                              data = stanData_silver,
                              chains = 4, iter = n_iter,
                              control = list(adapt_delta = 0.8))

save(file = "mat_stan_silver.Rda",
     stanOut_silver,
     stanData_silver)


## Get things ready for bighead carp

## Create predictor variables
X_1_bighead  <- model.matrix( ~ Pool -1, dat3_bighead)
X_2_bighead <- X_1_bighead * dat3_bighead[ , TLm]

y_bighead <-  dat3_bighead[ , Maturity]

## Create projection data
data_project_bighead <- seq(dat3_bighead[, range(TLm)[1]],
                           dat3_bighead[, range(TLm)[2]], by = 0.001)


stanData_bighead <- list(
    N  = nrow(dat3_bighead),
    N_pools = length(pools_use_mat_bighead),
    X_1 = X_1_bighead,
    X_2 = X_2_bighead,
    y = y_bighead,
    N_project = length(data_project_bighead),
    x_project = data_project_bighead
)

stanOut_bighead <- rstan::sampling(mat_model,
                              data = stanData_bighead,
                              chains = 4, iter = n_iter,
                              control = list(adapt_delta = 0.8))

save(file = "mat_stan_bighead.Rda",
     stanOut_bighead,
     stanData_bighead)
