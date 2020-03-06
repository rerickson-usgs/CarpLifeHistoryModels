## Load required libraires
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())


load("lengthWeight_bighead_sex.RData")
print(stanOut_bighead, pars = "beta")
plot(stanOut_bighead, pars = "beta")

##########################################################
##########################################################
## beta are individual-level parameters
## sigma corresponds to this level
## gamma are hyper-parameter for beta
## tau corresponds to this level
## omega is correlation matrix with tau coefficients

names_to_keep  <- colnames(stanData_bighead$x)[grepl("Sex", colnames(stanData_bighead$x))]
names_to_keep

stanOutsummary_SVCP <- summary(stanOut_bighead,
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

gg_plot_with_sex_bighead <-
    ggplot(sex_par,
       aes(x = coef_name, y = mean, group = par,
           ymin = `10%`,
           ymax = `90%`)) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_linerange(position = position_dodge(width = 0.3))
gg_plot_with_sex_bighead
ggsave("./figures/gg_plot_with_sex_bighead.jpg", gg_plot_with_sex_bighead, width = 6, height = 6)
ggsave("./figures/gg_plot_with_sex_bighead.pdf", gg_plot_with_sex_bighead, width = 6, height = 6)

## stanOutsummary_SVCP[[1]][grepl("beta", rownames(summary(stanOut_SVCP)[[1]])), ]
## stanOutsummary_SVCP[[1]][grepl("gamma", rownames(summary(stanOut_SVCP)[[1]])), ]
## stanOutsummary_SVCP[[1]][grepl("tau", rownames(summary(stanOut_SVCP)[[1]])), ]
## stanOutsummary_SVCP[[1]][grepl("Omega", rownames(summary(stanOut_SVCP)[[1]])), ]


