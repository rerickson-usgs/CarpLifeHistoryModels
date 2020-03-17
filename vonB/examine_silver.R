library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting
library(ggthemes)
library(rstan) # used to fit Bayesian model

options(mc.cores = parallel::detectCores())

load("vonBfitNot0_silver.RData")
ls()

## Examine model outputs directly
plot(stanOutO_silver, pars =c("Linf_bar", "K_bar", "Linf"))
print(stanOutO_silver, pars =c("Linf_bar", "K_bar", "Linf"))

plot(stanOutO_silver, pars =c("Linf_bar", "K_bar", "M_bar"))
plot(stanOutO_silver, pars =c("K", "K_bar"))
plot(stanOutO_silver, pars =c("Linf", "Linf_bar"))
plot(stanOutO_silver, pars =c("M", "M_bar"))

print(stanOutO_silver, pars =c("K", "K_bar"))
print(stanOutO_silver, pars =c("Linf", "Linf_bar"))
print(stanOutO_silver, pars =c("M", "M_bar"))
print(stanOutO_silver, pars =c("lp__"))


## merge in parameter details
stan_summary_silver <-
    data.frame(summary(stanOutO_silver, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))$summary)
stan_summary_silver$parameter  <- rownames(stan_summary_silver)
stan_summary_silver <- data.table(stan_summary_silver)

setnames(stan_summary_silver, "X2.5.",  "l95")
setnames(stan_summary_silver, "X97.5.", "u95")
setnames(stan_summary_silver, "X10.",  "l80")
setnames(stan_summary_silver, "X90.",  "u80")
stan_summary_silver



## Extract out L and K parameters
gsub_pat  <- "(K|Linf|M)(\\[\\d+\\]|_bar)"
coef_silver  <- stan_summary_silver[ grepl("K|Linf|M", parameter), ]
coef_silver[ , PoolID := gsub(gsub_pat, "\\2", parameter)]
coef_silver[ , PoolID := gsub("\\[|\\]", "", PoolID)]
coef_silver[ , parType := gsub(gsub_pat, "\\1", parameter)]

dat3_silver_pool_key  <-
    rbind(dat3_silver_pool_key,
          data.table(System = "Hyper-parameter",
                     Pool = "Hyper-parameter",
                     PoolID = "_bar")
          )

setkey(dat3_silver_pool_key, "PoolID")
setkey(coef_silver, "PoolID")

coef_silver <- coef_silver[ dat3_silver_pool_key]

coef_silver_plot <-
    ggplot(coef_silver,
           aes(x = Pool, y = mean)) +
    facet_grid( System ~ parType, scales = 'free') +
    coord_flip() +
    geom_linerange(aes(ymin = l95, ymax = u95), size = 1) +
    geom_linerange(aes(ymin = l80, ymax = u80), size = 1.5) +
    geom_point(size = 2.5) +
    theme_bw() +
    theme(strip.background = element_blank()) +
    xlab("Pool") +
    ylab(expression("Parameter estimates ("~y^-1~"for K, m for Linf,"~y^-1~"for M)"))

print(coef_silver_plot)
ggsave("./figures/coef_silver_plot.jpg", coef_silver_plot, width = 6, height = 6)
ggsave("./figures/coef_silver_plot.pdf", coef_silver_plot, width = 6, height = 6)

## Load data
dat <- fread("./data_use.csv")

## Order pools
new_pool_order <-
    c("Pool 14",
      "Pool 16",
      "Pool 17",
      "Pool 18",
      "Pool 19",
      "Pool 20",
      "Pool 22",
      "Pool 24",
      "Pool 26",
      "Pool 27",
      "Alton",
      "LaGrange",
      "Peoria",
      "Starved Rock",
      "Marseilles",
      "Dresden Island",
      "JT Myers",
      "Newburgh",
      "Cannelton",
      "McAlpine",
      "Markland",
      "Meldahl",
      "RC Byrd"
      )

dat[ , Pool := factor(Pool,
                      levels = new_pool_order
                      )]

dat2 <- dat[ !is.na(TL) & !is.na(Age), ]


## Silver carp analysis
dat3_silver <- dat2[ Species == "Silver", ]
dat3_silver[ , Pool := droplevels(Pool)]


## Get min and max
min_max_pool <- dat3_silver[ , .(minAge = min(Age), maxAge = max(Age)), by = Pool]

## Extract out prediction curves
pool_predictions_silver  <-
    stan_summary_silver[ grepl("siteProjections", parameter), ]

pool_gsub <- "siteProjections\\[(\\d+),(\\d+)\\]"
pool_predictions_silver[ , PoolID := gsub(pool_gsub, "\\1", parameter)]
pool_predictions_silver[ , AgeID := gsub(pool_gsub, "\\2", parameter)]

setkey(pool_predictions_silver, "PoolID")
setkey(dat3_silver_pool_key, "PoolID")

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

vonb_plot_silver <-
    ggplot(pool_predictions_silver_3,
           aes(x = Age, y = mean)) +
    facet_wrap( System ~ Pool) +
    geom_line(size = 1.6, aes(color = System)) +
    geom_point(data = dat3_silver, aes(x = Age, y = TLm), alpha = 1, color = 'navyblue') +
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = System), alpha = 0.50)+
    geom_ribbon(aes(ymin = l80, ymax = u80, fill = System),  alpha = 0.50) +
    theme_bw() +
    scale_fill_colorblind() +
    theme(strip.background = element_blank()) +
    ylab("Length (m)") +
    xlab("Age (y)")

print(vonb_plot_silver)
ggsave("./figures/vonb_plot_silver.jpg", vonb_plot_silver, width = 6, height = 6)
ggsave("./figures/vonb_plot_silver.pdf", vonb_plot_silver, width = 6, height = 6)

## Extract out hyper-parameter
hyper_predictions_silver  <-
    stan_summary_silver[ grepl("hyperProjection", parameter), ]

hyper_gsub  <-  "hyperProjection\\[(\\d+)\\]"
hyper_predictions_silver[ , AgeID := gsub(hyper_gsub, "\\1", parameter)]

setkey(hyper_predictions_silver, "AgeID")
hyper_predictions_silver <- hyper_predictions_silver[age_key]

hyper_plot_silver <-
    ggplot(hyper_predictions_silver,
           aes(x = Age, y = mean)) +
    geom_ribbon(aes(ymin = l95, ymax = u95), fill = 'navyblue', alpha = 0.50) +
    geom_ribbon(aes(ymin = l80, ymax = u80), fill = 'navyblue', alpha = 0.750) +
    theme_bw() +
    geom_line(data = pool_predictions_silver_2,
              aes(x = Age, y = mean,
                  color = System, group = Pool), size = 1.6) +
    geom_line(size = 3, color = 'navyblue', linetype = 'longdash') +
    theme_bw() +
    scale_color_colorblind("Pool") +
    ylab("Total length (m)") +
    xlab("Age (y)")
print(hyper_plot_silver)
ggsave("./figures/hyper_plot_silver.jpg", hyper_plot_silver, width = 6, height = 6)
ggsave("./figures/hyper_plot_silver.pdf", hyper_plot_silver, width = 6, height = 6)
