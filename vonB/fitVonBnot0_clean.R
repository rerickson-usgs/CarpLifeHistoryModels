library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting 
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

rerunStan = TRUE

## Read in a format data
dat <- fread("../DemographicsData.csv")
dat[ , Sampdate :=ymd(Sampdate)]
dat[ , month := month(Sampdate)]
dat[ is.na(month), month := 5]

dat2 <- dat[ !is.na(TL) & !is.na(Age), ]
dat2[ , Age2 := floor(Age)]
dat2[ , Age3 := Age2 + (month-5)/12]
dat2[ , Pool :=factor(Pool)]
dat2[ , TLm := TL/1000]


## Silver carp SVCP analysis
dat3_SVCP <- dat2[ Species == "SVCP", ] 
dat3_SVCP[ , Pool := factor(Pool)]
dat3_SVCP[ , PoolID := as.numeric(Pool)]

ageProjection = seq(0, 20, by = 1)
fwrite(file ="SVCP_vonBkey.csv",
       x = dat3_SVCP[ , .(PoolID = mean(PoolID)) , by = Pool])

stanData_SVCP <- list(
    nFish  = dim(dat3_SVCP)[1],
    nSites = length(dat3_SVCP[, unique(PoolID)]),
    length = dat3_SVCP[ , TLm], 
    poolID = dat3_SVCP[ , PoolID],
    age = dat3_SVCP[ , Age3],
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
    stanOutO_SVCP <- stan(file = "vonBoNot0.stan",
                          data = stanData_SVCP, chains = 4, iter = 6000,
                          control = list(adapt_delta = 0.8))
    save(stanOutO_SVCP, file = "vonBfitNot0_SVCP.RData")
} else {
    load("vonBfitNot0_SVCP.RData")
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
dat3_BHCP[ , Pool := factor(Pool)]
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
