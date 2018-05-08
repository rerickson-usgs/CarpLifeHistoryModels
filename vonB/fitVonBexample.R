library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

## Simulated data from http://rpubs.com/lacs/1123
age <- 1:12
lt <- c(10, 19, 24, 28, 32, 34, 38, 40, 41, 42, 42, 43)
dat <- data.frame(age, lt, poolID = 1L)


## Known pars:
## 45.78855  0.23959 -0.08253

stanDataDemo <- list(
    nFish = dim(dat)[1],
    nSites = 1,
    length = dat$lt,
    age = dat$age,
    poolID = dat$poolID)


stanDemo <- stan(file = "vonBdemo.stan", data = stanDataDemo, chains = 4,
                 iter = 10000)

stanDemo
pairs(stanDemo)
traceplot(stanDemo)


