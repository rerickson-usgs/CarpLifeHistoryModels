## Load required libraires
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(ggthemes) # used for plotting
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())


load("lengthWeight_silver.RData")

##########################################################
##########################################################
## beta are individual-level parameters
## sigma corresponds to this level
## gamma are hyper-parameter for beta
## tau corresponds to this level
## omega is correlation matrix with tau coefficients

print(stanOut_silver, pars = c("beta", "gamma"))
plot(stanOut_silver, pars = "beta")


## Extract out
summary_silver <-
    data.frame(summary(stanOut_silver,
                       probs = c(0.025, 0.1, 0.50, 0.9, 0.975))$summary)
summary_silver$parameter <- rownames(summary_silver)
summary_silver <- data.table(summary_silver)

correct_pool_order  <- c(levels(dat3_silver_pool_key$Pool), "Hyper-parameter")

## Plot intercepts
intercepts_silver  <-
    summary_silver[
        grepl("(beta|gamma)\\[(\\d+),(\\d+)\\]", parameter), ]
par_gsub <- "(beta|gamma)\\[(\\d+),(\\d+)\\]"
intercepts_silver[ , PoolID := as.numeric(gsub(par_gsub, "\\2", parameter))]
intercepts_silver[ , par := gsub(par_gsub, "\\1", parameter)]
intercepts_silver[ , parType := gsub(par_gsub, "\\3", parameter)]
setkey(intercepts_silver, "PoolID")
setkey(dat3_silver_pool_key, "PoolID")
intercepts_silver <- intercepts_silver[ dat3_silver_pool_key]
setnames(intercepts_silver, "X2.5.",  "l95")
setnames(intercepts_silver, "X97.5.", "u95")
setnames(intercepts_silver, "X10.",  "l80")
setnames(intercepts_silver, "X90.",  "u80")
intercepts_silver[ , Pool := as.character(Pool)]
intercepts_silver[ , Pool := ifelse(grepl("gamma", par), "Hyper-parameter", Pool)]
intercepts_silver[ , System := ifelse(grepl("gamma", par), "Hyper-parameter", System)]
intercepts_silver[ , Pool := factor(Pool, levels = correct_pool_order)]
intercepts_silver[ , parType := factor(parType, levels = c("1", "2"),
                                       labels = c('Pool coefficients ("intercpts")', 'Growth coefficients ("slopes")'))]

lw_coef <-
    ggplot(intercepts_silver, aes(x = Pool, y = mean)) +
    facet_grid( System ~ parType, scales = 'free') +
    coord_flip() +
    geom_linerange(aes(ymin = l95, ymax = u95), size = 1) +
    geom_linerange(aes(ymin = l80, ymax = u80), size = 1.5) +
    geom_point(size = 2.5) +
    theme_bw() +
    theme(strip.background = element_blank())
lw_coef
ggsave("./figures/lw_coef_silver.pdf", lw_coef, width = 6, height = 6)
ggsave("./figures/lw_coef_silver.jpg", lw_coef, width = 6, height = 6)

## Extractand plot models for each pool
pred_gsub <- "(yProject)\\[(\\d+),(\\d+)\\]"
predictions_silver  <- summary_silver[ grepl("yProject", parameter), ]
predictions_silver[ , PoolID := as.numeric(gsub(pred_gsub, "\\2", parameter))]
predictions_silver[ , LenID := as.numeric(gsub(pred_gsub, "\\3", parameter))]

setkey(predictions_silver, "PoolID")
setkey(dat3_silver_pool_key, "PoolID")
predictions_silver <- predictions_silver[ dat3_silver_pool_key]
setnames(predictions_silver, "X2.5.",  "l95")
setnames(predictions_silver, "X97.5.", "u95")
setnames(predictions_silver, "X10.",  "l80")
setnames(predictions_silver, "X90.",  "u80")

length_key <- data.table(LenID = 1:length(stanData_silver$xProject),
                         TLmLog10 = stanData_silver$xProject)
setkey(length_key, "LenID")
setkey(predictions_silver, "LenID")

predictions_silver  <- predictions_silver[ length_key]

## Read in a format data
dat <- fread("./data_use.csv")
dat[ , Sampdate :=ymd(Sampdate)]
dat2 <- dat[ !is.na(TL) & !is.na(WT), ]
## Format data for silver carp
dat3_silver <- dat2[ Species == "Silver", ]
dat3_silver[ , Pool := factor(Pool)]
dat3_silver[ , PoolID := as.numeric(Pool)]
## Creat group-level predictors
dat3_silver[ , Pool := factor(Pool)]
dat3_silver[ , PoolID := as.numeric(Pool)]
dat3_silver[ , TLmL10 := log10(TLm)] # may try centering here
dat3_silver[ , WTkgL10 := log10(WTkg)]
pool_min_max  <- dat3_silver[ , .(minL_pool = min(TLmL10), maxL_pool = max(TLmL10)),
                             by = .(Pool) ]

setkey(predictions_silver, "Pool")
setkey(pool_min_max, "Pool")
predictions_silver_2  <- predictions_silver[ pool_min_max]
predictions_silver_3 <-
    predictions_silver_2[ TLmLog10 > minL_pool &
                          TLmLog10 < maxL_pool, ]

lw_data_silver  <-
    ggplot(predictions_silver_3, aes(x = 10^TLmLog10, y = 10^mean)) +
    ## facet_grid( ~ System + Pool) +
    facet_wrap( System ~ Pool) +
    ## geom_ribbon(aes(ymin = 10^l95, ymax = 10^u95), fill = 'blue', alpha = 0.50)+
    ## geom_ribbon(aes(ymin = 10^l80, ymax = 10^u80), fill = 'blue', alpha = 0.50) +
    geom_point(data = dat3_silver, aes(x = TLm, y = WTkg), alpha = 1, color = 'navyblue') +
    geom_ribbon(aes(ymin = 10^l95, ymax = 10^u95, fill = System), alpha = 0.50)+
    geom_ribbon(aes(ymin = 10^l80, ymax = 10^u80, fill = System),  alpha = 0.50) +
    geom_line(size = 1.6, aes(color = System)) +
    theme_bw() +
    scale_color_colorblind() +
    theme(strip.background = element_blank()) +
    ylab("Weight (kg)") +
    xlab("Total length (m)")

print(lw_data_silver)
ggsave("./figures/lw_data_silver.jpg", lw_data_silver, width = 8, height = 6)
ggsave("./figures/lw_data_silver.pdf", lw_data_silver, width = 8, height = 6)


## Extract out and plot hyper predictions
hyper_gsub <- "(yHyper)\\[(\\d+)\\]"

hyper_silver  <- summary_silver[ grepl("yHyper", parameter), ]
hyper_silver[ , LenID := as.numeric(gsub(hyper_gsub, "\\2", parameter))]
hyper_silver

setnames(hyper_silver, "X2.5.",  "l95")
setnames(hyper_silver, "X97.5.", "u95")
setnames(hyper_silver, "X10.",  "l80")
setnames(hyper_silver, "X90.",  "u80")

length_key <- data.table(LenID = 1:length(stanData_silver$xProject),
                         TLmLog10 = stanData_silver$xProject)
setkey(length_key, "LenID")
setkey(hyper_silver, "LenID")

hyper_silver  <- hyper_silver[ length_key]

head(hyper_silver)

predictions_silver_3

lw_hyper_silver  <-
    ggplot(hyper_silver, aes(x = 10^TLmLog10, y = 10^mean)) +
    geom_ribbon(aes(ymin = 10^l95, ymax = 10^u95), fill = 'navyblue', alpha = 0.50) +
    geom_ribbon(aes(ymin = 10^l80, ymax = 10^u80), fill = 'navyblue', alpha = 0.750) +
    theme_bw() +
    geom_line(data = predictions_silver_2,
              aes(x = 10^TLmLog10, y = 10^mean,
                  color = System, group = Pool), size = 1.6) +
    geom_line(size = 3, color = 'navyblue', linetype = 'longdash') +
    theme_bw() +
    scale_color_colorblind("Pool") +
    xlab("Total length (m)") +
    ylab("Weight (kg)")
lw_hyper_silver
ggsave("lw_hyper_silver.jpg", lw_hyper_silver, width = 4, height = 4)
ggsave("lw_hyper_silver.pdf", lw_hyper_silver, width = 4, height = 4)







