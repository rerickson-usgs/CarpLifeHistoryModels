## Load required libraires
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting 
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

## Read in a format data
dat <- fread("../DemographicsData.csv")

dat[ , Sampdate :=ymd(Sampdate)] 
dat[ , unique(Species)]
dat[ , unique(Pool)]


## Convert metric units for stability
dat[ , TLm  := TL/1000]
dat[ , WTkg := WT/1000]

dat[ , TLmL10  := log10(TLm)]
dat[ , WTkgL10 := log10(WTkg)]

## Plot by pools
dat2 <- dat[ !is.na(TL) & !is.na(WT), ]
dat2[ , PoolID := as.numeric(Pool)]

## Extract out silver carp SVCP
dat3_SVCP<- dat2[ Species == "SVCP", ]
dat3_SVCP[ , Pool := factor(Pool)]
dat3_SVCP[ , PoolID := as.numeric(Pool)]

## Creat group-level predictors
dat3_SVCP[ , Pool := factor(Pool)]
dat3_SVCP[ , PoolID := as.numeric(Pool)]

## run model with correlated structure
x_SVCP  = dat3_SVCP[ , model.matrix( ~ TLmL10)]

groupPredictKey_SVCP <-
    dat3_SVCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]


u_SVCP  = matrix(rep(1, length(dat3_SVCP[, unique(PoolID)])), ncol = 1)

xProject_SVCP <- seq(dat2[ , range(TLmL10)][1], dat2[ , range(TLmL10)][2], length.out = 100)

## xProject_SVCP                           
stanData_SVCP <- list(
    N  = dim(dat3_SVCP)[1], # Num obs
    J  = length(dat3_SVCP[, unique(PoolID)]), # num groups 
    L  = 1, # num group predictors 
    y  = dat3_SVCP[ , WTkgL10], # observations 
    jj = dat3_SVCP[ , PoolID], # groups for each indivdual 
    x  = x_SVCP, # individual predictor matrix 
    u  = u_SVCP, # group predictors 
    K  = ncol(x_SVCP), # num individual predictors
    xProject = xProject_SVCP,
    nProject = length(xProject_SVCP)
)

## ## Only run if needed
stanOut_SVCP <- stan(file = "lengthWeight.stan",
                     data = stanData_SVCP,
                     chains = 4, iter = 6000,
                     control = list(adapt_delta = 0.8))
save(stanOut_SVCP, file = "lengthWeight3rd_SVCP.RData")
load("lengthWeight3rd_SVCP.RData")
stanOut_SVCP
gc()
##########################################################
##########################################################
stanOutsummary_SVCP <- summary(stanOut_SVCP,
                               probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
## stanOutsummary_SVCP[[1]][grepl("beta", rownames(summary(stanOut_SVCP)[[1]])), ]
## stanOutsummary_SVCP[[1]][grepl("gamma", rownames(summary(stanOut_SVCP)[[1]])), ]
## stanOutsummary_SVCP[[1]][grepl("tau", rownames(summary(stanOut_SVCP)[[1]])), ]
## stanOutsummary_SVCP[[1]][grepl("Omega", rownames(summary(stanOut_SVCP)[[1]])), ]



## bighead carp model
dat2[ Species == "BHCP", .N, by = Pool][ order(N, decreasing = TRUE), ]
dat3_BHCP <- dat2[ Species == "BHCP", ]
dat3_BHCP[ , Pool := factor(Pool)]
dat3_BHCP[ , PoolID := as.numeric(Pool)]
dat3_BHCP[ , .N, by = .(Pool, Species)]

## Creat group-level predictors
dat3_BHCP[ , Pool := factor(Pool)]
dat3_BHCP[ , PoolID := as.numeric(Pool)]


## run model with correlated structure
x_BHCP  = dat3_BHCP[ , model.matrix( ~ TLmL10)]

groupPredictKey_BHCP <-
    dat3_BHCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]

u_BHCP  = matrix(rep(1, length(dat3_BHCP[, unique(PoolID)])), ncol = 1)


xProject_BHCP <-xProject_SVCP

stanData_BHCP <- list(
    N  = dim(dat3_BHCP)[1], # Num obs
    J  = length(dat3_BHCP[, unique(PoolID)]), # num groups 
    L  = 1, # num group predictors 
    y  = dat3_BHCP[ , WTkgL10], # observations 
    jj = dat3_BHCP[ , PoolID], # groups for each indivdual 
    x  = x_BHCP, # individual predictor matrix 
    u  = u_BHCP, # group predictors 
    K  = ncol(x_BHCP), # num individual predictors
    xProject = xProject_BHCP,
    nProject = length(xProject_BHCP)
)

## ## Only run if needed
stanOut_BHCP <- stan(file = "lengthWeight.stan",
                     data = stanData_BHCP,
                     chains = 4, iter = 6000,
                     control = list(adapt_delta = 0.8))
save(stanOut_BHCP, file = "lengthWeight3rd_BHCP.RData")
## load("lengthWeight3rd_BHCP.RData")
## stanOut_BHCP

##########################################################
##########################################################
stanOutsummary_BHCP <- summary(stanOut_BHCP,
                               probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
## stanOutsummary_BHCP[[1]][grepl("beta", rownames(summary(stanOut_BHCP)[[1]])), ]
## stanOutsummary_BHCP[[1]][grepl("gamma", rownames(summary(stanOut_BHCP)[[1]])), ]
## stanOutsummary_BHCP[[1]][grepl("tau", rownames(summary(stanOut_BHCP)[[1]])), ]
## stanOutsummary_BHCP[[1]][grepl("Omega", rownames(summary(stanOut_BHCP)[[1]])), ]

