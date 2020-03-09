## Load required libraires
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())


load("lengthWeight_silver_sex.RData")
print(stanOut_silver, pars = "beta")
plot(stanOut_silver, pars = "beta")

##########################################################
##########################################################
## beta are individual-level parameters
## sigma corresponds to this level
## gamma are hyper-parameter for beta
## tau corresponds to this level
## omega is correlation matrix with tau coefficients

names_to_keep  <- colnames(stanData_silver$x)[grepl("Sex", colnames(stanData_silver$x))]
names_to_keep

stanOutsummary_SVCP <- summary(stanOut_silver,
                               probs = c(0.025, 0.1, 0.50, 0.9, 0.975))$summary
row_names  <- rownames(stanOutsummary_SVCP)

sex_est  <-  stanOutsummary_SVCP[ grepl("beta\\[\\d+,[1-7]\\]", row_names), ]
sex_par <- data.table(sex_est)
sex_par[ , par := rownames(sex_est)]
sex_par[ , coef_id := as.character(gsub("(beta)\\[(\\d+),([1-7])\\]", "\\3", par))]
sex_par
## key for sex pars



sex_coef_key <-
    data.table(
        coef_id = as.character(1:length(names_to_keep)),
        coef_name = names_to_keep
    )

setkey(sex_par, "coef_id")
setkey(sex_coef_key, "coef_id")
sex_par  <- sex_par[sex_coef_key]

sex_par[ grep("TLm", coef_name), ]

ggplot(sex_par,
       aes(x = coef_name, y = mean, group = par,
           ymin = `10%`,
           ymax = `90%`)) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_linerange(position = position_dodge(width = 0.3))
