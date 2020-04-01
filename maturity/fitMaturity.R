## Load required librarie
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(rstan) # used to fit Bayesian model

n_iter <- 10000
#rstan_options(auto_write = TRUE)
## setup to use multiple cores and other local options
options(mc.cores = parallel::detectCores())
## Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

## Read in a format data
dat <- fread("./data_use.csv")
dat[ , Sampdate :=ymd(Sampdate)]

dat2 <- dat[ !is.na(Maturity) & !is.na(TLm), ]

## Extract pools to use
pools_use_mat_silver <-
    dat2[ , .N, by = .( Pool, Maturity, Species)][
        Species =="Silver" & Maturity == 0, ][, unique(Pool)]

pools_use_mat_bighead <-
    dat2[ , .N, by = .( Pool, Maturity, Species)][
        Species =="Bighead"  & Maturity == 0, ][, unique(Pool)]


dat3_silver <- dat2[ Species == "Silver" & Pool %in% pools_use_mat_silver, ]
dat3_bighead <- dat2[ Species == "Bighead" & Pool %in% pools_use_mat_bighead, ]

dat3_silver[ , PoolID := as.numeric(as.factor(Pool))]

dat3_silver_pool_key <-
    dat3_silver[ , .(PoolID = mean(PoolID)), by = .(System, Pool)]
dat_3_pool_key


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
## X_1_silver  <- model.matrix( ~ Pool -1, dat3_silver)
## X_2_silver <- X_1_silver * dat3_silver[ , TLm]
## head(X_1_silver)
## head(X_2_silver)
X_silver  <- model.matrix( ~ TLm, dat3_silver)
head(X_silver)


## Function inputs
## Ind level inputs
y <-  dat3_silver[ , Maturity]
jj <- dat3_silver[ , PoolID]


## Predictor matrix on individual level
ind_dat <- dat3_silver
ind_formula <- ~ TLm

## Group inputs
group_dat <- dat3_silver_pool_key
group_formula <- ~1

## Projection inputs
n_projection <- 10
min_projection <- dat3_silver[, min(TLm)]
max_projection <- dat3_silver[, max(TLm)]


## inside function
X <- model.matrix( ind_formula, data = ind_dat)
U <- model.matrix( group_formula, data = group_dat)

N <- nrow(X)
K <- ncol(X)
N_groups <- nrow(U)
X_project <-
    matrix(
        c(rep(1, n_projection),
          seq(min_projection,
              max_projection, length.out = n_projection)),
        nrow = 2, byrow = TRUE
    )

standata <- list(N = N, K = K,
                 N_groups = N_groups, L = L,
                 jj = jj, X = X, y = y, U = U,
                 N_project = N_project, X_project = X_project)



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
    stan_model(file = "../../Parameter_package/fishStan/inst/stan/hierarchicalLogistical.stan")

stanOut_silver <- rstan::sampling(mat_model,
                              data = standata,
                              chains = 4, iter = 600,
                              control = list(adapt_delta = 0.8))

print(stanOut_silver, pars = 'gamma')

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
