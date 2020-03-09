library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

load("vonBfitNot0_silver_female.RData")
ls()



## stanOutO_SVCP
stanOutOsummary_SVCP_female <-
    summary(stanOutO_silver_female, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))

plot(stanOutO_silver_female, pars =c("Linf_bar", "K_bar", "Linf"))
print(stanOutO_silver_female, pars =c("Linf_bar", "K_bar", "Linf"))


load("vonBfitNot0_silver_male.RData")
x11()
plot(stanOutO_silver_male, pars =c("Linf_bar", "K_bar", "Linf"))
print(stanOutO_silver_male, pars =c("Linf_bar", "K_bar", "Linf"))


print(stanOutO_silver_female, pars =c( "K_bar", "Linf_bar"))
print(stanOutO_silver_male, pars =c( "K_bar", "Linf_bar"))

plot(stanOutO_silver_female, pars =c("Linf_bar", "K_bar")) +
    xlim(c(0.3, 1.1))
x11()
plot(stanOutO_silver_male, pars =c("Linf_bar", "K_bar")) +
    xlim(c(0.3, 1.1))
