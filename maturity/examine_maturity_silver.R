
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(ggthemes) # used for plotting
library(rstan) # used to fit Bayesian model
## options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

## Load previously run sims
load("mat_stan_silver.Rda")

## Examine outputs
par_save  <- c("alpha", "alpha_hyper", "beta", "beta_hyper")
print(stanOut_silver, pars = par_save)

plot(stanOut_silver, pars = c("alpha", "alpha_hyper"))
plot(stanOut_silver, pars = c("beta", "beta_hyper"))
traceplot(stanOut_silver, pars = c("alpha", "beta", "lp__"))

## Format data for plotting
pool_names <-
    c(gsub("(^Pool)(\\w+)", "\\2", colnames(stanData_silver$X_1)), "hyper")
pool_basin <-
    c(rep("Illinois", 3),
      rep("Mississippi", 2),
      "hyper")

pool_key <-
    data.table(pool_names = pool_names,
               par_num = c(1:(length(pool_names) - 1), "hyper"),
               pool_basin = pool_basin
               )

summaryOut_silver <-
    summary(stanOut_silver, pars = par_save)$summary
summary(summaryOut_silver[, "Rhat"])

parOut_silver <-
    data.frame(summary(stanOut_silver,
                       pars = par_save,
                       prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)

parOut_silver$parameter <- rownames(parOut_silver)
parOut_silverDT <- data.table(parOut_silver)
setnames(parOut_silverDT, "X2.5.", "l95")
setnames(parOut_silverDT, "X97.5.", "u95")
setnames(parOut_silverDT, "X10.", "l90")
setnames(parOut_silverDT, "X90.", "u90")

parOut_silverDT[ , par_num := gsub("alpha|beta|\\[|\\]|_", "", parameter)]
parOut_silverDT[ , par := gsub("\\[\\d+\\]|_hyper", "", parameter)]

setkey(parOut_silverDT, "par_num")
setkey(pool_key, "par_num")

parOut_silverDT

parOut_silverDT <- parOut_silverDT[ pool_key]
parOut_silverDT[ , par := factor(par,
                                 levels = c("alpha", "beta"),
                                 labels = c("intercept coefficient", "slope coefficient"))
                ]


parOut_silverDT

parEst_silver <-
    ggplot(parOut_silverDT, aes(x = pool_names, y = mean)) +
    geom_linerange(aes(ymin = l95, ymax = u95)) +
    geom_linerange(aes(ymin = l90, ymax = u90), size = 1.25) +
    geom_point(size = 1.6) +
    coord_flip() +
    ylab("Parameter estimate") +
    xlab("Parameter") +
    theme_bw() +
    facet_grid( pool_basin ~ par, scales = 'free') +
    theme(strip.background = element_blank())

parEst_silver
ggsave("parEst_silver.pdf", parEst_silver, width = 4, height = 2)
ggsave("parEst_silver.jpg", parEst_silver, width = 4, height = 2)

### Predicted distribution
pool_predictions_silver <-
    data.frame(summary(stanOut_silver, pars = c("y_project_pools"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
pool_predictions_silver$parameter  <-
    rownames(pool_predictions_silver)

pool_predictions_silverDT <- data.table(pool_predictions_silver)
setnames(pool_predictions_silverDT, "X2.5.", "l95")
setnames(pool_predictions_silverDT, "X97.5.", "u95")
setnames(pool_predictions_silverDT, "X10.", "l90")
setnames(pool_predictions_silverDT, "X90.", "u90")

pool_key
pool_predictions_silverDT
gsub_pat <- "(y_project_pools)\\[(\\d+),(\\d+)\\]"

pool_predictions_silverDT[ , par_num := gsub(gsub_pat, "\\3", parameter)]
pool_predictions_silverDT[ , len_idx := as.integer(gsub(gsub_pat, "\\2", parameter))]

setkey(pool_predictions_silverDT, "par_num")
pool_predictions_silverDT <- pool_predictions_silverDT[ pool_key[ !grep("hyper", par_num),]]


length_key  <- data.table(len_idx = 1:length(stanData_silver$x_project),
                          length = (stanData_silver$x_project))

setkey(length_key, "len_idx")
setkey(pool_predictions_silverDT, "len_idx")

pool_predictions_silverDT <- pool_predictions_silverDT[ length_key]

## Load and extract raw data
y_plot  <- data.table(cbind(stanData_silver$X_1, y = stanData_silver$y))
y_plot <- melt(y_plot, id.vars = "y", variable.name = "pool_names")
y_plot <- y_plot[ value != 0,]
y_plot[ , value := NULL]
y_plot[ , pool_names := gsub("^Pool", "", pool_names)]

x_plot  <- data.table(cbind(stanData_silver$X_2))
x_plot <- melt(x_plot, variable.name = "pool_names", value.name = "TLm")
x_plot <- x_plot[ TLm != 0,]
x_plot[ , pool_names := gsub("^Pool", "", pool_names)]

xy_plot <- cbind(x_plot, y = y_plot[ ,y])
setkey(xy_plot, "pool_names")
setkey(pool_key, "pool_names")

xy_plot <- xy_plot[pool_key[ !grepl("hyper", pool_names),]]

predEst_silver <-
    ggplot(pool_predictions_silverDT, aes(x = length, y = mean)) +
    geom_ribbon(aes(ymin = l95, ymax = u95), fill = 'blue', alpha = 0.50)+
    geom_ribbon(aes(ymin = l90, ymax = u90), fill = 'blue', alpha = 0.50) +
    geom_line(size = 1.6) +
    ylab("Probability of being mature") +
    xlab("Total length (m)") +
    theme_bw() +
    theme(strip.background = element_blank()) +
    facet_grid( . ~ pool_basin + pool_names) +
    geom_jitter(data = xy_plot, aes(x = TLm, y = y), width = 0, height = 0.005)
predEst_silver
ggsave("maturity_pools_silver.pdf", predEst_silver, width = 6, height = 4)
ggsave("maturity_pools_silver.jpg", predEst_silver, width = 6, height = 4)


## Extract out and plot hyper parameter
hyper_predictions_silver <-
    data.frame(summary(stanOut_silver, pars = c("y_project_hyper"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
hyper_predictions_silver$parameter  <-
    rownames(hyper_predictions_silver)

hyper_predictions_silverDT <- data.table(hyper_predictions_silver)
setnames(hyper_predictions_silverDT, "X2.5.", "l95")
setnames(hyper_predictions_silverDT, "X97.5.", "u95")
setnames(hyper_predictions_silverDT, "X10.", "l90")
setnames(hyper_predictions_silverDT, "X90.", "u90")

gsub_pat_hyper <- "(y_project_hyper)\\[(\\d+)\\]"

hyper_predictions_silverDT[ , len_idx := as.integer(gsub(gsub_pat_hyper, "\\2", parameter))]
hyper_predictions_silverDT

length_key  <- data.table(len_idx = 1:length(stanData_silver$x_project),
                          length = (stanData_silver$x_project))

setkey(length_key, "len_idx")
setkey(hyper_predictions_silverDT, "len_idx")

hyper_predictions_silverDT <- hyper_predictions_silverDT[ length_key]

hyper_Est_silver <-
    ggplot(hyper_predictions_silverDT, aes(x = length, y = mean)) +
    geom_ribbon(aes(ymin = l95, ymax = u95), fill = 'skyblue', alpha = 0.50)+
    geom_ribbon(aes(ymin = l90, ymax = u90), fill = 'skyblue', alpha = 0.750) +
    geom_line(size = 2.5, color = 'navyblue', linetype = 'dashed') +
    ylab("Probability of being mature") +
    xlab("Total length (m)") +
    theme_bw() +
    geom_line(data = pool_predictions_silverDT,
              aes(x = length, y = mean,
                  color =pool_names), size = 1.6) +
    theme_bw() +
    scale_color_colorblind("Pool")



hyper_Est_silver
ggsave("maturity_hyper_silver.pdf", hyper_Est_silver, width = 6, height = 4)
ggsave("maturity_hyper_silver.jpg", hyper_Est_silver, width = 6, height = 4)
