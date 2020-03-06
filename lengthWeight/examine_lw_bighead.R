## Load required libraires
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(ggthemes) # used for plotting
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())


load("lengthWeight_bighead.RData")

##########################################################
##########################################################
## beta are individual-level parameters
## sigma corresponds to this level
## gamma are hyper-parameter for beta
## tau corresponds to this level
## omega is correlation matrix with tau coefficients

print(stanOut_bighead, pars = c("beta", "gamma"))
plot(stanOut_bighead, pars = "beta")


## Extract out
summary_bighead <-
    data.frame(summary(stanOut_bighead,
                       probs = c(0.025, 0.1, 0.50, 0.9, 0.975))$summary)
summary_bighead$parameter <- rownames(summary_bighead)
summary_bighead <- data.table(summary_bighead)

correct_pool_order  <- c(levels(dat3_bighead_pool_key$Pool), "Hyper-parameter")

## Plot intercepts
intercepts_bighead  <-
    summary_bighead[
        grepl("(beta|gamma)\\[(\\d+),(\\d+)\\]", parameter), ]
par_gsub <- "(beta|gamma)\\[(\\d+),(\\d+)\\]"
intercepts_bighead[ , PoolID := as.numeric(gsub(par_gsub, "\\2", parameter))]
intercepts_bighead[ , par := gsub(par_gsub, "\\1", parameter)]
intercepts_bighead[ , parType := gsub(par_gsub, "\\3", parameter)]
setkey(intercepts_bighead, "PoolID")
setkey(dat3_bighead_pool_key, "PoolID")
intercepts_bighead <- intercepts_bighead[ dat3_bighead_pool_key]
setnames(intercepts_bighead, "X2.5.",  "l95")
setnames(intercepts_bighead, "X97.5.", "u95")
setnames(intercepts_bighead, "X10.",  "l80")
setnames(intercepts_bighead, "X90.",  "u80")
intercepts_bighead[ , Pool := as.character(Pool)]
intercepts_bighead[ , Pool := ifelse(grepl("gamma", par), "Hyper-parameter", Pool)]
intercepts_bighead[ , System := ifelse(grepl("gamma", par), "Hyper-parameter", System)]
intercepts_bighead[ , Pool := factor(Pool, levels = correct_pool_order)]
intercepts_bighead[ , parType := factor(parType, levels = c("1", "2"),
                                       labels = c('Pool coefficients ("intercpts")', 'Growth coefficients ("slopes")'))]

lw_coef <-
    ggplot(intercepts_bighead, aes(x = Pool, y = mean)) +
    facet_grid( System ~ parType, scales = 'free') +
    coord_flip() +
    geom_linerange(aes(ymin = l95, ymax = u95), size = 1) +
    geom_linerange(aes(ymin = l80, ymax = u80), size = 1.5) +
    geom_point(size = 2.5) +
    theme_bw() +
    theme(strip.background = element_blank())
lw_coef
ggsave("./figures/lw_coef_bighead.pdf", lw_coef, width = 6, height = 6)
ggsave("./figures/lw_coef_bighead.jpg", lw_coef, width = 6, height = 6)

## Extractand plot models for each pool
pred_gsub <- "(yProject)\\[(\\d+),(\\d+)\\]"
predictions_bighead  <- summary_bighead[ grepl("yProject", parameter), ]
predictions_bighead[ , PoolID := as.numeric(gsub(pred_gsub, "\\2", parameter))]
predictions_bighead[ , LenID := as.numeric(gsub(pred_gsub, "\\3", parameter))]

setkey(predictions_bighead, "PoolID")
setkey(dat3_bighead_pool_key, "PoolID")
predictions_bighead <- predictions_bighead[ dat3_bighead_pool_key]
setnames(predictions_bighead, "X2.5.",  "l95")
setnames(predictions_bighead, "X97.5.", "u95")
setnames(predictions_bighead, "X10.",  "l80")
setnames(predictions_bighead, "X90.",  "u80")

length_key <- data.table(LenID = 1:length(stanData_bighead$xProject),
                         TLmLog10 = stanData_bighead$xProject)
setkey(length_key, "LenID")
setkey(predictions_bighead, "LenID")

predictions_bighead  <- predictions_bighead[ length_key]

## Read in a format data
dat <- fread("./data_use.csv")
dat[ , Sampdate :=ymd(Sampdate)]
dat2 <- dat[ !is.na(TL) & !is.na(WT), ]
## Format data for bighead carp
dat3_bighead <- dat2[ Species == "Bighead", ]
dat3_bighead[ , Pool := factor(Pool)]
dat3_bighead[ , PoolID := as.numeric(Pool)]
## Creat group-level predictors
dat3_bighead[ , Pool := factor(Pool)]
dat3_bighead[ , PoolID := as.numeric(Pool)]
dat3_bighead[ , TLmL10 := log10(TLm)] # may try centering here
dat3_bighead[ , WTkgL10 := log10(WTkg)]
pool_min_max  <- dat3_bighead[ , .(minL_pool = min(TLmL10), maxL_pool = max(TLmL10)),
                             by = .(Pool) ]

setkey(predictions_bighead, "Pool")
setkey(pool_min_max, "Pool")
predictions_bighead_2  <- predictions_bighead[ pool_min_max]
predictions_bighead_3 <-
    predictions_bighead_2[ TLmLog10 > minL_pool &
                          TLmLog10 < maxL_pool, ]

lw_data_bighead  <-
    ggplot(predictions_bighead_3, aes(x = 10^TLmLog10, y = 10^mean)) +
    geom_line(size = 1.6) +
    facet_grid( ~ Pool + System) +
    geom_ribbon(aes(ymin = 10^l95, ymax = 10^u95), fill = 'blue', alpha = 0.50)+
    geom_ribbon(aes(ymin = 10^l80, ymax = 10^u80), fill = 'blue', alpha = 0.50) +
    geom_point(data = dat3_bighead, aes(x = TLm, y = WTkg)) +
    theme_bw() +
    theme(strip.background = element_blank()) +
    ylab("Weight (kg)") +
    xlab("Total length (m)")

print(lw_data_bighead)
ggsave("./figures/lw_data_bighead.jpg", lw_data_bighead, width = 10, height = 4)
ggsave("./figures/lw_data_bighead.pdf", lw_data_bighead, width = 10, height = 4)


## Extract out and plot hyper predictions
hyper_gsub <- "(yHyper)\\[(\\d+)\\]"

hyper_bighead  <- summary_bighead[ grepl("yHyper", parameter), ]
hyper_bighead[ , LenID := as.numeric(gsub(hyper_gsub, "\\2", parameter))]
hyper_bighead

setnames(hyper_bighead, "X2.5.",  "l95")
setnames(hyper_bighead, "X97.5.", "u95")
setnames(hyper_bighead, "X10.",  "l80")
setnames(hyper_bighead, "X90.",  "u80")

length_key <- data.table(LenID = 1:length(stanData_bighead$xProject),
                         TLmLog10 = stanData_bighead$xProject)
setkey(length_key, "LenID")
setkey(hyper_bighead, "LenID")

hyper_bighead  <- hyper_bighead[ length_key]

head(hyper_bighead)

predictions_bighead_3

lw_hyper_bighead  <-
    ggplot(hyper_bighead, aes(x = 10^TLmLog10, y = 10^mean)) +
    geom_ribbon(aes(ymin = 10^l95, ymax = 10^u95), fill = 'skyblue', alpha = 0.50) +
    geom_ribbon(aes(ymin = 10^l80, ymax = 10^u80), fill = 'skyblue', alpha = 0.750) +
    theme_bw() +
    geom_line(data = predictions_bighead_2,
              aes(x = 10^TLmLog10, y = 10^mean,
                  color = System, group = Pool), size = 1.6) +
    geom_line(size = 3, color = 'navyblue', linetype = 'solid') +
    theme_bw() +
    scale_color_colorblind("Pool") +
    xlab("Total length (m)") +
    ylab("Weight (kg)")
lw_hyper_bighead
ggsave("./figures/lw_hyper_bighead.jpg", lw_hyper_bighead, width = 4, height = 4)
ggsave("./figures/lw_hyper_bighead.pdf", lw_hyper_bighead, width = 4, height = 4)




