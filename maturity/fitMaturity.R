## Load required librarie
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(rstan) # used to fit Bayesian model

## setup to use multiple cores
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

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
                              chains = 4, iter = 600,
                              control = list(adapt_delta = 0.8))

save(file = "mat_stan_silver.Rda",
     stanOut_silver,
     stanData_silver)


## ### Fit Model to BHCP
## dataInBHCP <- seq(dat3BHCP[, range(TLm)[1]], dat3BHCP[, range(TLm)[2]], by = 0.001)

## stanDataBHCP <- list(
##     N  = dim(dat3BHCP)[1],
##     x = dat3BHCP[ , TLm],
##     y = dat3BHCP[ , M2],
##     xProject = dataInBHCP,
##     nProject = length(dataInBHCP)
## )

## names(stanDataBHCP)

## stanOutBHCP <- stan(file = "maturity.stan", data = stanDataBHCP, chains = 4, iter = 6000,
##                     control = list(adapt_delta = 0.8))
## save(file = "logisticRegressionBHCP.Rda", x = stanOutBHCP)
## ## load("logisticRegressionBHCP.Rda")

## print(stanOutBHCP, pars = c("alpha", "beta", "lp__"))
## plot(stanOutBHCP, pars = c("alpha", "beta"))

## traceplot(stanOutBHCP, pars = c("alpha", "beta", "lp__"))
## traceplot(stanOutBHCP, inc_warmup = TRUE, pars = c("alpha", "beta"))

## pairs(stanOutBHCP, pars = c("alpha", "beta", "lp__"))

## summaryOutBHCP <- summary(stanOutBHCP)$summary
## summary(summaryOutBHCP[, "Rhat"])

## parOutBHCP <- data.frame(summary(stanOutBHCP, pars = c("alpha", "beta"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
## parOutBHCP$parameter <- rownames(parOutBHCP)
## parOutBHCPDT <- data.table(parOutBHCP)
## setnames(parOutBHCPDT, "X2.5.", "l95")
## setnames(parOutBHCPDT, "X97.5.", "u95")
## setnames(parOutBHCPDT, "X10.", "l90")
## setnames(parOutBHCPDT, "X90.", "u90")
## parOutBHCPDT

## parEstBHCP <- ggplot(parOutBHCPDT, aes(x = parameter, y = mean)) +
##     geom_linerange(aes(ymin = l95, ymax = u95)) +
##     geom_linerange(aes(ymin = l90, ymax = u90), size = 1.25) +
##     geom_point(size = 1.6) +
##     coord_flip() +
##     ylab("Parameter estimate") +
##     xlab("Parameter") +
##     theme_minimal()
## parEstBHCP

## ggsave("parEstBHCP.pdf", parEstBHCP, width = 4, height = 2)


## ### Predicted distribution

## predOutBHCP <- data.frame(summary(stanOutBHCP, pars = c("yProject"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
## predOutBHCP$parameter <- rownames(predOutBHCP)
## predOutBHCPDT <- data.table(predOutBHCP)
## setnames(predOutBHCPDT, "X2.5.", "l95")
## setnames(predOutBHCPDT, "X97.5.", "u95")
## setnames(predOutBHCPDT, "X10.", "l90")
## setnames(predOutBHCPDT, "X90.", "u90")
## predOutBHCPDT[ , index := as.numeric(gsub("yProject\\[(\\d{1,3})\\]", "\\1", parameter))]
## setkey(predOutBHCPDT, "index")
## dataInBHCPDT <- data.table(length = dataInBHCP, index = 1:length(dataInBHCP))
## setkey(dataInBHCPDT, "index")

## predOutBHCPDT <- dataInBHCPDT[predOutBHCPDT]

## predEstBHCP <-
##     ggplot(predOutBHCPDT, aes(x = length, y = mean)) +
##     geom_ribbon(aes(ymin = l95, ymax = u95), fill = 'blue', alpha = 0.50)+
##     geom_ribbon(aes(ymin = l90, ymax = u90), fill = 'blue', alpha = 0.50) +
##     geom_line(size = 1.6) +
##     ylab("Probability of being mature") +
##     xlab("Length (m)") +
##     theme_minimal()  +
##     geom_jitter(data = dat3BHCP, aes(x = TLm, y = M2), width = 0, height = 0.005)
## predEstBHCP
## ggsave("maturityPredBHCP.pdf", predEstBHCP, width = 6, height = 4)
