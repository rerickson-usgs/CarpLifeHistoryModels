## Load required librarie
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting 
library(rstan) # used to fit Bayesian model
## setup to use multiple cores
options(mc.cores = parallel::detectCores())

## Read in a format data
dat <- fread("../DemographicsData.csv")
dat[ , Sampdate :=ymd(Sampdate)] 

dat2 <- dat[ Maturity != "NA",] 
dat2[ , Maturity := factor(Maturity)]
dat2[ , M2 := as.numeric(Maturity) - 1]
dat2[ , TLm := TL / 1000]
dat2[ , WTkg := WT/1000]
dat2[ , Month := month(Sampdate)]
dat2[ , SpeciesFull := factor(Species, levels = c("BHCP", "SVCP"), labels = c("Bighead", "Silver"))]


## Examine by pool to see if we need to pool or can estimate across pools
dat2[ , .N, by = .( Maturity, Pool)][ order(Pool)]

dat2[ Species == "SVCP", .N, by = Pool][ order(N, decreasing = TRUE), ]

dat3SVCP <- dat2[ Species == "SVCP", ]

dat3BHCP <- dat2[ Species == "BHCP", ]

## Plot by pools
ggplot(dat2[ Species %in% c( "SVCP", "BHCP"), ], aes(x = TLm, y = M2)) +
    geom_smooth(method = 'glm',  method.args = list(family = "binomial")) + 
    geom_point(alpha = 0.25) +
    facet_grid( Species~Pool) +
    xlab("Length (m)") +
    ylab("Maturity") + theme_minimal()

ggplot(dat2[ Species %in% c( "SVCP", "BHCP"), ], aes(x = TLm, y = M2)) +
    geom_smooth(method = 'glm',  method.args = list(family = "binomial")) + 
    geom_point(alpha = 0.25) +
    facet_grid( System ~ Species) +
    xlab("Length (m)") +
    ylab("Maturity") + theme_minimal()

### Fit Model to SVCP
dataInSVCP <- seq(dat3SVCP[, range(TLm)[1]], dat3SVCP[, range(TLm)[2]], by = 0.001)

stanDataSVCP <- list(
    N  = dim(dat3SVCP)[1],
    x = dat3SVCP[ , TLm], 
    y = dat3SVCP[ , M2],
    xProject = dataInSVCP,
    nProject = length(dataInSVCP)
)

names(stanDataSVCP)
stanOutSVCP <- stan(file = "maturity.stan", data = stanDataSVCP, chains = 4, iter = 6000,
                    control = list(adapt_delta = 0.8))
save(file = "logisticRegressionSVCP.Rda", x = stanOutSVCP)

## load("logisticRegressionSVCP.Rda")

print(stanOutSVCP, pars = c("alpha", "beta", "lp__"))
plot(stanOutSVCP, pars = c("alpha", "beta"))

traceplot(stanOutSVCP, pars = c("alpha", "beta", "lp__"))
traceplot(stanOutSVCP, inc_warmup = TRUE, pars = c("alpha", "beta"))

pairs(stanOutSVCP, pars = c("alpha", "beta", "lp__"))

summaryOutSVCP <- summary(stanOutSVCP)$summary
summary(summaryOutSVCP[, "Rhat"])

parOutSVCP <- data.frame(summary(stanOutSVCP, pars = c("alpha", "beta"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
parOutSVCP$parameter <- rownames(parOutSVCP)
parOutSVCPDT <- data.table(parOutSVCP)
setnames(parOutSVCPDT, "X2.5.", "l95")
setnames(parOutSVCPDT, "X97.5.", "u95")
setnames(parOutSVCPDT, "X10.", "l90")
setnames(parOutSVCPDT, "X90.", "u90")
parOutSVCPDT

parEstSVCP <- ggplot(parOutSVCPDT, aes(x = parameter, y = mean)) +
    geom_linerange(aes(ymin = l95, ymax = u95)) +
    geom_linerange(aes(ymin = l90, ymax = u90), size = 1.25) +
    geom_point(size = 1.6) +
    coord_flip() +
    ylab("Parameter estimate") +
    xlab("Parameter") +
    theme_minimal()
parEstSVCP

ggsave("parEstSVCP.pdf", parEstSVCP, width = 4, height = 2)


### Predicted distribution
predOutSVCP <- data.frame(summary(stanOutSVCP, pars = c("yProject"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
predOutSVCP$parameter <- rownames(predOutSVCP)
predOutSVCPDT <- data.table(predOutSVCP)
setnames(predOutSVCPDT, "X2.5.", "l95")
setnames(predOutSVCPDT, "X97.5.", "u95")
setnames(predOutSVCPDT, "X10.", "l90")
setnames(predOutSVCPDT, "X90.", "u90")
predOutSVCPDT[ , index := as.numeric(gsub("yProject\\[(\\d{1,3})\\]", "\\1", parameter))]
setkey(predOutSVCPDT, "index")
dataInSVCPDT <- data.table(length = dataInSVCP, index = 1:length(dataInSVCP))
setkey(dataInSVCPDT, "index")
predOutSVCPDT <- dataInSVCPDT[predOutSVCPDT]

predEstSVCP <-
    ggplot(predOutSVCPDT, aes(x = length, y = mean)) +
    geom_ribbon(aes(ymin = l95, ymax = u95), fill = 'blue', alpha = 0.50)+ 
    geom_ribbon(aes(ymin = l90, ymax = u90), fill = 'blue', alpha = 0.50) +
    geom_line(size = 1.6) +
    ylab("Probability of being mature") +
    xlab("Length (m)") +
    theme_minimal()  +
    geom_jitter(data = dat3SVCP, aes(x = TLm, y = M2), width = 0, height = 0.005) 
predEstSVCP
ggsave("maturityPredSVCP.pdf", predEstSVCP, width = 6, height = 4)


### Fit Model to BHCP
dataInBHCP <- seq(dat3BHCP[, range(TLm)[1]], dat3BHCP[, range(TLm)[2]], by = 0.001)

stanDataBHCP <- list(
    N  = dim(dat3BHCP)[1],
    x = dat3BHCP[ , TLm], 
    y = dat3BHCP[ , M2],
    xProject = dataInBHCP,
    nProject = length(dataInBHCP)
)

names(stanDataBHCP)

stanOutBHCP <- stan(file = "maturity.stan", data = stanDataBHCP, chains = 4, iter = 6000,
                    control = list(adapt_delta = 0.8))
save(file = "logisticRegressionBHCP.Rda", x = stanOutBHCP)
## load("logisticRegressionBHCP.Rda")

print(stanOutBHCP, pars = c("alpha", "beta", "lp__"))
plot(stanOutBHCP, pars = c("alpha", "beta"))

traceplot(stanOutBHCP, pars = c("alpha", "beta", "lp__"))
traceplot(stanOutBHCP, inc_warmup = TRUE, pars = c("alpha", "beta"))

pairs(stanOutBHCP, pars = c("alpha", "beta", "lp__"))

summaryOutBHCP <- summary(stanOutBHCP)$summary
summary(summaryOutBHCP[, "Rhat"])

parOutBHCP <- data.frame(summary(stanOutBHCP, pars = c("alpha", "beta"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
parOutBHCP$parameter <- rownames(parOutBHCP)
parOutBHCPDT <- data.table(parOutBHCP)
setnames(parOutBHCPDT, "X2.5.", "l95")
setnames(parOutBHCPDT, "X97.5.", "u95")
setnames(parOutBHCPDT, "X10.", "l90")
setnames(parOutBHCPDT, "X90.", "u90")
parOutBHCPDT

parEstBHCP <- ggplot(parOutBHCPDT, aes(x = parameter, y = mean)) +
    geom_linerange(aes(ymin = l95, ymax = u95)) +
    geom_linerange(aes(ymin = l90, ymax = u90), size = 1.25) +
    geom_point(size = 1.6) +
    coord_flip() +
    ylab("Parameter estimate") +
    xlab("Parameter") +
    theme_minimal()
parEstBHCP

ggsave("parEstBHCP.pdf", parEstBHCP, width = 4, height = 2)


### Predicted distribution

predOutBHCP <- data.frame(summary(stanOutBHCP, pars = c("yProject"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
predOutBHCP$parameter <- rownames(predOutBHCP)
predOutBHCPDT <- data.table(predOutBHCP)
setnames(predOutBHCPDT, "X2.5.", "l95")
setnames(predOutBHCPDT, "X97.5.", "u95")
setnames(predOutBHCPDT, "X10.", "l90")
setnames(predOutBHCPDT, "X90.", "u90")
predOutBHCPDT[ , index := as.numeric(gsub("yProject\\[(\\d{1,3})\\]", "\\1", parameter))]
setkey(predOutBHCPDT, "index")
dataInBHCPDT <- data.table(length = dataInBHCP, index = 1:length(dataInBHCP))
setkey(dataInBHCPDT, "index")

predOutBHCPDT <- dataInBHCPDT[predOutBHCPDT]

predEstBHCP <-
    ggplot(predOutBHCPDT, aes(x = length, y = mean)) +
    geom_ribbon(aes(ymin = l95, ymax = u95), fill = 'blue', alpha = 0.50)+ 
    geom_ribbon(aes(ymin = l90, ymax = u90), fill = 'blue', alpha = 0.50) +
    geom_line(size = 1.6) +
    ylab("Probability of being mature") +
    xlab("Length (m)") +
    theme_minimal()  +
    geom_jitter(data = dat3BHCP, aes(x = TLm, y = M2), width = 0, height = 0.005) 
predEstBHCP
ggsave("maturityPredBHCP.pdf", predEstBHCP, width = 6, height = 4)
