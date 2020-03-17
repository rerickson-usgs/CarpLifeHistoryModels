library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(ggthemes) # used to examine outputs
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

male_out <- data.frame(summary(stanOutO_silver_male, pars =c("Linf_bar", "K_bar"), probs= c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
male_out$sex <- "male"
male_out$parameter <- row.names(male_out)
male_out


female_out <- data.frame(summary(stanOutO_silver_female, pars =c("Linf_bar", "K_bar"), probs = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
female_out$sex <- "female"
female_out$parameter <- row.names(female_out)
female_out

all_out <-
    rbind(male_out,
          female_out)
all_out


pool_predictions_silver <-
    pool_predictions_silver[dat3_silver_pool_key[ !grep("Hyper", System), ]]

age_key <-
    data.table(AgeID = as.character(1:(length(stanData_silver$ageProject))),
               Age = stanData_silver$ageProject)

setkey(pool_predictions_silver, "AgeID")
setkey(age_key, "AgeID")
pool_predictions_silver <- pool_predictions_silver[age_key]

setkey(pool_predictions_silver, "Pool")
setkey(min_max_pool, "Pool")

pool_predictions_silver_2 <- pool_predictions_silver[min_max_pool]
pool_predictions_silver_3 <-
    pool_predictions_silver_2[ Age <= maxAge +1 &
                               Age >= minAge -1, ]


ggplot(all_out, aes(x =parameter, y = mean, color = sex)) +
    coord_flip() +
    scale_color_colorblind() +
    geom_linerange(aes(ymin = `X2.5.`, ymax = `X97.5.`),
                   position = position_dodge(0.1),
                   size = 1.0) +
    geom_linerange(aes(ymin = `X10.`, ymax = `X90.`),
                   position = position_dodge(0.1),
                   size = 1.5) +
    geom_point(position = position_dodge(0.1), size = 2)
