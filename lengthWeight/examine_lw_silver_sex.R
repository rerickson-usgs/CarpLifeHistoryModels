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


## stanOutsummary_SVCP[[1]][grepl("beta", rownames(summary(stanOut_SVCP)[[1]])), ]
## stanOutsummary_SVCP[[1]][grepl("gamma", rownames(summary(stanOut_SVCP)[[1]])), ]
## stanOutsummary_SVCP[[1]][grepl("tau", rownames(summary(stanOut_SVCP)[[1]])), ]
## stanOutsummary_SVCP[[1]][grepl("Omega", rownames(summary(stanOut_SVCP)[[1]])), ]


################
## Plot intercepts
intercepts_SVCP <- data.frame(stanOutsummary_SVCP[[1]][
    grepl("(beta|gamma)(\\[\\d+,1\\])",
          rownames(summary(stanOut_SVCP)[[1]])), ])
intercepts_SVCP$ID = gsub("\\,1]|\\[", "", rownames(intercepts_SVCP))
## intercepts_SVCP
groupPredictKey_SVCP

groupPredictKey_SVCP <-
    copy(dat3_SVCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),])
## groupPredictKey_SVCP
## dat3_SVCP[ , .(PoolID, Pool)]
groupPredictKey_SVCP[ , PoolID := paste0("beta", PoolID)]
groupPredictKey_SVCP <- copy(rbind(groupPredictKey_SVCP,
                                   data.table(Pool = "Hyper-parameter",
                                              PoolID = 'gamma1')))
## groupPredictKey_SVCP

interceptsDT_SVCP <- data.table(intercepts_SVCP)
setkey(interceptsDT_SVCP, "ID")
setkey(groupPredictKey_SVCP, "PoolID")
interceptsDT_SVCP <- interceptsDT_SVCP[groupPredictKey_SVCP]

setnames(interceptsDT_SVCP, "X2.5.",  "l95")
setnames(interceptsDT_SVCP, "X97.5.", "u95")

setnames(interceptsDT_SVCP, "X10.",  "l80")
setnames(interceptsDT_SVCP, "X90.",  "u80")

## interceptsDT_SVCP
ggIntercept_SVCP <-
    ggplot(interceptsDT_SVCP, aes(x = Pool, y = mean)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) +
    coord_flip() +
    xlab("Pool") +
    ylab(expression("Intercept estimate ("*log[10](weight)*")")) +
    theme_minimal()

## ggIntercept_SVCP
ggsave("intercept_SVCP.pdf", ggIntercept_SVCP, width = 4, height = 6)

################
## Plot slopes
slopes_SVCP <-
    data.frame(stanOutsummary_SVCP[[1]][grepl("(beta|gamma)(\\[\\d+,2\\])",
                                              rownames(summary(stanOut_SVCP)[[1]])),
                                        ])
slopes_SVCP$ID = gsub("\\,2]|\\[", "", rownames(slopes_SVCP))
## slopes_SVCP

groupPredictKey_SVCP <-
    dat3_SVCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
## groupPredictKey_SVCP
dat3_SVCP[ , .(PoolID, Pool)]

groupPredictKey_SVCP[ , PoolID := paste0("beta", PoolID)]
groupPredictKey_SVCP <-
    rbind(groupPredictKey_SVCP,
          data.table(Pool = "Hyper-parameter", PoolID = 'gamma1'))
## groupPredictKey_SVCP

slopesDT_SVCP <- data.table(slopes_SVCP)
setkey(slopesDT_SVCP, "ID")
setkey(groupPredictKey_SVCP, "PoolID")
slopesDT_SVCP <- slopesDT_SVCP[groupPredictKey_SVCP]

setnames(slopesDT_SVCP, "X2.5.",  "l95")
setnames(slopesDT_SVCP, "X97.5.", "u95")

setnames(slopesDT_SVCP, "X10.",  "l80")
setnames(slopesDT_SVCP, "X90.",  "u80")

## slopesDT_SVCP

ggSlope_SVCP <-
    ggplot(slopesDT_SVCP, aes(x = Pool, y = mean)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) +
    coord_flip() +
    xlab("Pool") +
    ylab(expression(atop("Length-weight slope",
                         "estimate ("*frac(log[10](weight),log[10](length))*")"))) +
    theme_minimal()

## ggSlope_SVCP
ggsave("slope_SVCP.pdf", ggSlope_SVCP, width = 4, height = 6)

## plot projections
siteProjections_SVCP <-
    data.frame(
        stanOutsummary_SVCP[[1]][grepl("yProject",
                                       rownames(summary(stanOut_SVCP)[[1]])), ])
siteProjections_SVCP$parameter <- rownames(siteProjections_SVCP)
siteProjectionsDT_SVCP <- data.table(siteProjections_SVCP)
siteProjectionsDT_SVCP[ ,
                       PoolID := gsub("yProject\\[(\\d+),(\\d+)\\]",
                                      "\\1", parameter)]
siteProjectionsDT_SVCP[ ,
                       lengthID :=
                           as.numeric(
                               gsub("yProject\\[(\\d+),(\\d+)\\]",
                                    "\\2", parameter))]

setnames( siteProjectionsDT_SVCP, "X2.5.",  "l95")
setnames( siteProjectionsDT_SVCP, "X97.5.", "u95")
setnames( siteProjectionsDT_SVCP, "X10.",  "l80")
setnames( siteProjectionsDT_SVCP, "X90.",  "u80")

## Merge in length
lengthDT_SVCP <- data.table(lengthID = 1:length(xProject_SVCP_raw),
                            length = xProject_SVCP_raw)
setkey(lengthDT_SVCP, "lengthID")
setkey(siteProjectionsDT_SVCP, "lengthID")
siteProjectionsDT_SVCP <- siteProjectionsDT_SVCP[lengthDT_SVCP]

## Merge files
siteProjectionsDT_SVCP[ , PoolID := paste0("beta", PoolID)]
setkey(siteProjectionsDT_SVCP, "PoolID")
setkey(groupPredictKey_SVCP, "PoolID")

siteProjectionsDT_SVCP <-
    siteProjectionsDT_SVCP[groupPredictKey_SVCP[ PoolID !="gamma1", ]]

poolMinMax_SVCP <-
    dat3_SVCP[ , .(poolMin =min(TLmL10, na.rm = TRUE),
                   poolMax = max(TLmL10, na.rm = TRUE)), by = Pool]
setkey(siteProjectionsDT_SVCP, "Pool")
setkey(poolMinMax_SVCP, "Pool")
siteProjectionsDT_SVCP <-
    siteProjectionsDT_SVCP[ poolMinMax_SVCP]

head(siteProjectionsDT_SVCP[ , length >= poolMin])

GGlwData_SVCP <- ggplot() +
    geom_point(data = dat3_SVCP, aes( x = TLmL10,  y = WTkgL10)) +
    geom_line(data = siteProjectionsDT_SVCP, aes(x = length, y = mean),
              color = 'blue', size = 1.1) +
    facet_wrap( ~ Pool , nrow = 2) +
    ## geom_ribbon(data = siteProjectionsDT,
    ##             aes(x = length,  ymin = l95, ymax = u95), color = 'orange', alpha = 0.5) +
    ylab(expression(log[10]*"(weight kg)")) +
    xlab(expression(log[10]*"(length m)")) +
    theme_minimal()
## GGlwData_SVCP
ggsave("lengthWeightData_SVCP.pdf", GGlwData_SVCP, width = 8, height = 4)

## exract out hyper-parameter
yHyper_SVCP <-
    data.frame(stanOutsummary_SVCP[[1]][grepl(
                                      "yHyper",
                                      rownames(summary(stanOut_SVCP)[[1]])), ])
yHyper_SVCP$parameter = rownames(yHyper_SVCP)
yHyperDT_SVCP <- data.table(yHyper_SVCP)
yHyperDT_SVCP[ ,
              lengthID := as.numeric(gsub("yHyper\\[(\\d+)\\]",
                                          "\\1", parameter))]
setnames( yHyperDT_SVCP, "X2.5.",  "l95")
setnames( yHyperDT_SVCP, "X97.5.", "u95")
setnames( yHyperDT_SVCP, "X10.",  "l80")
setnames( yHyperDT_SVCP, "X90.",  "u80")

## Merge in length
lengthDT_SVCP <- data.table(lengthID = 1:length(xProject_SVCP_raw),
                            length = xProject_SVCP_raw)
setkey(lengthDT_SVCP, "lengthID")
setkey(yHyperDT_SVCP, "lengthID")
yHyperDT_SVCP <-
    yHyperDT_SVCP[lengthDT_SVCP]


ggHyper_SVCP <-
    ggplot() +
    geom_line(data = siteProjectionsDT_SVCP,
              aes(x = length, y = mean, group = Pool), size = 1, alpha = 0.5, color = 'blue') +
    ylab(expression(log[10]*"(weight kg)")) +
    xlab(expression(log[10]*"(length m)")) +
    theme_minimal() +
    geom_ribbon(data = yHyperDT_SVCP,
                aes(x = length, ymin = l95, ymax = u95),
                fill = 'red', alpha = 0.75) +
    geom_line(data = yHyperDT_SVCP,
              aes(x = length, y = mean),
              color = 'black', size = 1)

## ggHyper_SVCP
ggsave("lengthWeightHyper_SVCP.pdf", ggHyper_SVCP, width = 6, height = 4)
