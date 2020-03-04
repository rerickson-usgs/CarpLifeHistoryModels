
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(rstan) # used to fit Bayesian model

## Load previously run sims

load("mat_stan_silver.Rda")

## Examine outputs
print(stanOut_silver)
plot(stanOut_silver, pars = c("alpha", "alpha_hyper"))
plot(stanOut_silver, pars = c("beta", "beta_hyper"))



print(stanOut_silver)


traceplot(stanOut_silver, pars = c("alpha", "beta", "lp__"))
traceplot(stanOut_silver, inc_warmup = TRUE, pars = c("alpha", "beta"))

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
