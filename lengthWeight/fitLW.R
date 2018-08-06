## Load required libraires
library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting 
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

## Read in a format data
dat <- fread("../Demographics_01.csv")

dat[ , Sampdate :=ymd(Sampdate)] 
dat[,  FL := as.numeric(FL)]

dat[ , unique(Species)]

## Explore data with plots and do some manipulation
ggplot(data  = dat[Species %in% c( "SVCP", "BHCP"), ],
       aes(x = TL, y = WT)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = 'lm') +
    facet_grid( ~ Species) +
    theme_minimal() +
    ylab("Weight") +
    xlab("Total length") +
    scale_y_log10() + 
    scale_x_log10()


## Plot by pools
dat2 <- dat[ !is.na(TL) & !is.na(WT), ]
dat2[ , PoolID := as.numeric(Pool)]
dat2[ , .N, by = Pool]

dat2[ , mean(WT, na.rm = TRUE), by = .(Pool)]


ggplot(dat2[ Species %in% c( "SVCP", "BHCP"), ], aes(x = TL/1000, y = WT/1000)) +
    geom_point(alpha = 0.25) +
    facet_grid( Species~Pool) +
    xlab("Total length (m)") +
    ylab("Weight (kg)") + theme_minimal()+
    scale_y_log10() +
    scale_x_log10()

## Get data from Stan
## Fit silver carp SVCP

dat2[ Species == "SVCP", .N, by = Pool][ order(N, decreasing = TRUE), ]

dat3_SVCP<- dat2[ Pool %in% c("LaGrange", "Alton", "Peoria",
                          "Marseilles", "Pool 26", "Pool 27") ,][ Species == "SVCP", ]

dat3_SVCP[ , Pool := factor(Pool)]
dat3_SVCP[ , PoolID := as.numeric(Pool)]
dat3_SVCP[ , .N, by = .(Pool, Species)]

## Convert to M from mm to stabilisze results

dat3_SVCP[ , summary(TL/1000)]
dat3_SVCP[ , TLm  := TL/1000]
dat3_SVCP[ , WTkg := WT/1000]

## Creat group-level predictors
dat3_SVCP[ , Pool := factor(Pool)]
dat3_SVCP[ , PoolID := as.numeric(Pool)]

## Log10 transorm data and center it 
dat3_SVCP[ , TLmL10 := log10(TLm) - mean(log10(TLm))]
dat3_SVCP[ , WTkgL10 := log10(WTkg)]

## build simple length-weight model
## with hardcoded regression 
s1_SVCP <- list(
    N  = dim(dat3_SVCP)[1],
    y  = dat3_SVCP[ , WTkgL10],
    x  = dat3_SVCP[ , TLmL10]
    )

## lwSimpleOut_SVCP <- stan(file = "lwSimple.stan",
##                     data = s1_SVCP,
##                     chains = 4, iter = 1000,
##                     control = list(adapt_delta = 0.8))
## lwSimpleOut_SVCP

## build length-weight model
## with matrix input
x_SVCP<- model.matrix( ~ TLmL10, data = dat3_SVCP)

s2_SVCP <- list(
    N  = dim(dat3_SVCP)[1],
    K  = ncol(x_SVCP),
    y  = dat3_SVCP[ , WTkgL10],
    x  = x_SVCP
    )

## lwSimMatOut_SVCP <- stan(file = "lwSimpleMatrix.stan",
##                     data = s2_SVCP,
##                     chains = 4, iter = 1000,
##                     control = list(adapt_delta = 0.8))
## lwSimMatOut_SVCP

## run model with correlated structure

x_SVCP  = dat3_SVCP[ , model.matrix( ~ TLmL10)]
x_SVCP[1, ]

groupPredictKey_SVCP <-
    dat3_SVCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey_SVCP
dat3_SVCP[ , .(PoolID, Pool)]

u_SVCP  = matrix(rep(1, length(dat3_SVCP[, unique(PoolID)])), ncol = 1)
u_SVCP

## output
dat3_SVCP[ , summary(WTkgL10)]
## input
dat3_SVCP[ , summary(TLmL10)]

xProject_SVCP <- seq(-1, 0.25, length.out = 10)
xProject_SVCP

stanData_SVCP <- list(
    N  = dim(dat3_SVCP)[1], # Num obs
    J  = length(dat3_SVCP[, unique(PoolID)]), # num groups 
    L  = 1, # num group predictors 
    y  = dat3_SVCP[ , WTkgL10], # observations 
    jj = dat3_SVCP[ , PoolID], # groups for each indivdual 
    x  = x_SVCP, # individual predictor matrix 
    u  = u_SVCP, # group predictors 
    K  = ncol(x_SVCP), # num individual predictors
    xProject = xProject_SVCP,
    nProject = length(xProject_SVCP)
)

## ## Only run if needed
## stanOut_SVCP <- stan(file = "lengthWeight.stan",
##                      data = stanData_SVCP,
##                      chains = 4, iter = 6000,
##                      control = list(adapt_delta = 0.8))
## save(stanOut_SVCP, file = "lengthWeight2_SVCP.RData")
load("lengthWeight2_SVCP.RData")
stanOut_SVCP

##########################################################
##########################################################
## beta are individual-level parameters
## sigma corresponds to this level
## gamma are hyper-parameter for beta
## tau corresponds to this level
## omega is correlation matrix with tau coefficients

stanOutsummary_SVCP <- summary(stanOut_SVCP,
                               probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutsummary_SVCP[[1]][grepl("beta", rownames(summary(stanOut_SVCP)[[1]])), ]
stanOutsummary_SVCP[[1]][grepl("gamma", rownames(summary(stanOut_SVCP)[[1]])), ]
stanOutsummary_SVCP[[1]][grepl("tau", rownames(summary(stanOut_SVCP)[[1]])), ]
stanOutsummary_SVCP[[1]][grepl("Omega", rownames(summary(stanOut_SVCP)[[1]])), ]


################
## Plot intercepts
intercepts_SVCP <- data.frame(stanOutsummary_SVCP[[1]][
    grepl("(beta|gamma)(\\[\\d,1\\])",
          rownames(summary(stanOut_SVCP)[[1]])), ])
intercepts_SVCP$ID = gsub("\\,1]|\\[", "", rownames(intercepts_SVCP))
intercepts_SVCP

groupPredictKey_SVCP <-
    dat3[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey_SVCP
dat3_SVCP[ , .(PoolID, Pool)]
groupPredictKey_SVCP[ , PoolID := paste0("beta", PoolID)]
groupPredictKey_SVCP <- rbind(groupPredictKey_SVCP,
                              data.table(Pool = "Hyper-parameter",
                                         PoolID = 'gamma1'))
groupPredictKey_SVCP

interceptsDT_SVCP <- data.table(intercepts_SVCP)
setkey(interceptsDT_SVCP, "ID")
setkey(groupPredictKey_SVCP, "PoolID")
interceptsDT_SVCP <- interceptsDT_SVCP[groupPredictKey_SVCP]

setnames(interceptsDT_SVCP, "X2.5.",  "l95")
setnames(interceptsDT_SVCP, "X97.5.", "u95")

setnames(interceptsDT_SVCP, "X10.",  "l80")
setnames(interceptsDT_SVCP, "X90.",  "u80")


library(ggplot2)

ggIntercept_SVCP <-
    ggplot(interceptsDT_SVCP, aes(x = Pool, y = mean)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) + 
    coord_flip() +
    xlab("Pool") +
    ylab(expression(atop("Length-weight intercept",
                         "estimate ("*log[10]*log[10]*" scale)"))) +
    theme_minimal()

ggIntercept_SVCP
ggsave("intercept_SVCP.pdf", ggIntercept_SVCP, width = 4, height = 6)

################
## Plot slopes
slopes_SVCP <-
    data.frame(stanOutsummary_SVCP[[1]][grepl("(beta|gamma)(\\[\\d,2\\])",
                                              rownames(summary(stanOut_SVCP)[[1]])),
                                        ])
slopes_SVCP$ID = gsub("\\,2]|\\[", "", rownames(slopes_SVCP))
slopes_SVCP

groupPredictKey_SVCP <-
    dat3_SVCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey_SVCP
dat3_SVCP[ , .(PoolID, Pool)]

groupPredictKey_SVCP[ , PoolID := paste0("beta", PoolID)]
groupPredictKey_SVCP <-
    rbind(groupPredictKey_SVCP,
          data.table(Pool = "Hyper-parameter", PoolID = 'gamma1'))
groupPredictKey_SVCP

slopesDT_SVCP <- data.table(slopes_SVCP)
setkey(slopesDT_SVCP, "ID")
setkey(groupPredictKey_SVCP, "PoolID")
slopesDT_SVCP <- slopesDT_SVCP[groupPredictKey_SVCP]

setnames(slopesDT_SVCP, "X2.5.",  "l95")
setnames(slopesDT_SVCP, "X97.5.", "u95")

setnames(slopesDT_SVCP, "X10.",  "l80")
setnames(slopesDT_SVCP, "X90.",  "u80")

slopesDT_SVCP

ggSlope_SVCP <-
    ggplot(slopesDT_SVCP, aes(x = Pool, y = mean)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) + 
    coord_flip() +
    xlab("Pool") +
    ylab(expression(atop("Length-weight slope",
                         "estimate ("*log[10]*log[10]*" scale)"))) +
    theme_minimal()

ggSlope_SVCP
ggsave("slope_SVCP.pdf", ggSlope_SVCP, width = 4, height = 6)

## plot projections 
siteProjections_SVCP <-
    data.frame(
        stanOutsummary_SVCP[[1]][grepl("yProject",
                                       rownames(summary(stanOut_SVCP)[[1]])), ])
siteProjections_SVCP$parameter <- rownames(siteProjections_SVCP)
siteProjectionsDT_SVCP <- data.table(siteProjections_SVCP)
siteProjectionsDT_SVCP[ ,
                       PoolID := gsub("yProject\\[(\\d{1,2}),(\\d{1,2})\\]",
                                      "\\1", parameter)]
siteProjectionsDT_SVCP[ ,
                       lengthID :=
                           as.numeric(
                               gsub("yProject\\[(\\d{1,2}),(\\d{1,2})\\]",
                                    "\\2", parameter))]

setnames( siteProjectionsDT_SVCP, "X2.5.",  "l95")
setnames( siteProjectionsDT_SVCP, "X97.5.", "u95")
setnames( siteProjectionsDT_SVCP, "X10.",  "l80")
setnames( siteProjectionsDT_SVCP, "X90.",  "u80")

## Merge in length
lengthDT_SVCP <- data.table(lengthID = 1:length(xProject_SVCP),
                            length = xProject_SVCP)
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
    facet_grid( ~ Pool) +
    ## geom_ribbon(data = siteProjectionsDT,
    ##             aes(x = length,  ymin = l95, ymax = u95), color = 'orange', alpha = 0.5) +
    ylab(expression(log[10]*"(weight kg)")) +
    xlab(expression(log[10]*"(length m)")) +
    theme_minimal()
GGlwData_SVCP
ggsave("lengthWeightData_SVCP.pdf", GGlwData_SVCP, width = 8, height = 4)

## exract out hyper parameter
yHyper_SVCP <-
    data.frame(stanOutsummary_SVCP[[1]][grepl(
                                      "yHyper",
                                      rownames(summary(stanOut_SVCP)[[1]])), ])
yHyper_SVCP$parameter = rownames(yHyper_SVCP)
yHyperDT_SVCP <- data.table(yHyper_SVCP)
yHyperDT_SVCP[ ,
              lengthID := as.numeric(gsub("yHyper\\[(\\d{1,2})\\]",
                                          "\\1", parameter))]
setnames( yHyperDT_SVCP, "X2.5.",  "l95")
setnames( yHyperDT_SVCP, "X97.5.", "u95")
setnames( yHyperDT_SVCP, "X10.",  "l80")
setnames( yHyperDT_SVCP, "X90.",  "u80")

## Merge in length
lengthDT_SVCP <- data.table(lengthID = 1:length(xProject_SVCP),
                            length = xProject_SVCP)
setkey(lengthDT_SVCP, "lengthID")
setkey(yHyperDT_SVCP, "lengthID")
yHyperDT_SVCP <-
    yHyperDT_SVCP[lengthDT_SVCP]


ggHyper_SVCP <-
    ggplot() +
    geom_line(data = siteProjectionsDT_SVCP,
              aes(x = length, y = mean, color = Pool), size = 1.1) +
    ylab(expression(log[10]*"(weight kg)")) +
    xlab(expression(log[10]*"(length m)")) +
    theme_minimal() +
    scale_color_manual( values = c("red", "blue", "seagreen",
                                   "orange", "skyblue", "navyblue")) +
    geom_ribbon(data = yHyperDT_SVCP,
                aes(x = length, ymin = l95, ymax = u95),
                fill = 'grey', alpha = 0.5) +
    geom_line(data = yHyperDT_SVCP,
              aes(x = length, y = mean),
              color = 'black', size = 1)

ggHyper_SVCP
ggsave("lengthWeightHyper_SVCP.pdf", ggHyper_SVCP, width = 6, height = 4) 


## bighead carp model

dat2[ Species == "BHCP", .N, by = Pool][ order(N, decreasing = TRUE), ]

dat3_BHCP<- dat2[ Pool %in% c("LaGrange", "Alton", "Peoria",
                          "Marseilles", "Pool 26", "Pool 27") ,][ Species == "BHCP", ]

dat3_BHCP[ , Pool := factor(Pool)]
dat3_BHCP[ , PoolID := as.numeric(Pool)]
dat3_BHCP[ , .N, by = .(Pool, Species)]

## Convert to M from mm to stabilisze results

dat3_BHCP[ , summary(TL/1000)]
dat3_BHCP[ , TLm  := TL/1000]
dat3_BHCP[ , WTkg := WT/1000]

## Creat group-level predictors
dat3_BHCP[ , Pool := factor(Pool)]
dat3_BHCP[ , PoolID := as.numeric(Pool)]

## Log10 transorm data and center it 
dat3_BHCP[ , TLmL10 := log10(TLm) - mean(log10(TLm))]
dat3_BHCP[ , WTkgL10 := log10(WTkg)]

## build simple length-weight model
## with hardcoded regression 
s1_BHCP <- list(
    N  = dim(dat3_BHCP)[1],
    y  = dat3_BHCP[ , WTkgL10],
    x  = dat3_BHCP[ , TLmL10]
    )

## lwSimpleOut_BHCP <- stan(file = "lwSimple.stan",
##                     data = s1_BHCP,
##                     chains = 4, iter = 1000,
##                     control = list(adapt_delta = 0.8))
## lwSimpleOut_BHCP

## build length-weight model
## with matrix input
x_BHCP<- model.matrix( ~ TLmL10, data = dat3_BHCP)

s2_BHCP <- list(
    N  = dim(dat3_BHCP)[1],
    K  = ncol(x_BHCP),
    y  = dat3_BHCP[ , WTkgL10],
    x  = x_BHCP
    )

## lwSimMatOut_BHCP <- stan(file = "lwSimpleMatrix.stan",
##                     data = s2_BHCP,
##                     chains = 4, iter = 1000,
##                     control = list(adapt_delta = 0.8))
## lwSimMatOut_BHCP

## run model with correlated structure

x_BHCP  = dat3_BHCP[ , model.matrix( ~ TLmL10)]
x_BHCP[1, ]

groupPredictKey_BHCP <-
    dat3_BHCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey_BHCP
dat3_BHCP[ , .(PoolID, Pool)]

u_BHCP  = matrix(rep(1, length(dat3_BHCP[, unique(PoolID)])), ncol = 1)
u_BHCP

## output
dat3_BHCP[ , summary(WTkgL10)]
## input
dat3_BHCP[ , summary(TLmL10)]

xProject_BHCP <- seq(-1, 0.25, length.out = 10)
xProject_BHCP

stanData_BHCP <- list(
    N  = dim(dat3_BHCP)[1], # Num obs
    J  = length(dat3_BHCP[, unique(PoolID)]), # num groups 
    L  = 1, # num group predictors 
    y  = dat3_BHCP[ , WTkgL10], # observations 
    jj = dat3_BHCP[ , PoolID], # groups for each indivdual 
    x  = x_BHCP, # individual predictor matrix 
    u  = u_BHCP, # group predictors 
    K  = ncol(x_BHCP), # num individual predictors
    xProject = xProject_BHCP,
    nProject = length(xProject_BHCP)
)

## ## Only run if needed
## stanOut_BHCP <- stan(file = "lengthWeight.stan",
##                      data = stanData_BHCP,
##                      chains = 4, iter = 6000,
##                      control = list(adapt_delta = 0.8))
## save(stanOut_BHCP, file = "lengthWeight2_BHCP.RData")
load("lengthWeight2_BHCP.RData")
stanOut_BHCP

##########################################################
##########################################################
## beta are individual-level parameters
## sigma corresponds to this level
## gamma are hyper-parameter for beta
## tau corresponds to this level
## omega is correlation matrix with tau coefficients

stanOutsummary_BHCP <- summary(stanOut_BHCP,
                               probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutsummary_BHCP[[1]][grepl("beta", rownames(summary(stanOut_BHCP)[[1]])), ]
stanOutsummary_BHCP[[1]][grepl("gamma", rownames(summary(stanOut_BHCP)[[1]])), ]
stanOutsummary_BHCP[[1]][grepl("tau", rownames(summary(stanOut_BHCP)[[1]])), ]
stanOutsummary_BHCP[[1]][grepl("Omega", rownames(summary(stanOut_BHCP)[[1]])), ]


################
## Plot intercepts
intercepts_BHCP <- data.frame(stanOutsummary_BHCP[[1]][
    grepl("(beta|gamma)(\\[\\d,1\\])",
          rownames(summary(stanOut_BHCP)[[1]])), ])
intercepts_BHCP$ID = gsub("\\,1]|\\[", "", rownames(intercepts_BHCP))
intercepts_BHCP

groupPredictKey_BHCP <-
    dat3[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey_BHCP
dat3_BHCP[ , .(PoolID, Pool)]
groupPredictKey_BHCP[ , PoolID := paste0("beta", PoolID)]
groupPredictKey_BHCP <- rbind(groupPredictKey_BHCP,
                              data.table(Pool = "Hyper-parameter",
                                         PoolID = 'gamma1'))
groupPredictKey_BHCP

interceptsDT_BHCP <- data.table(intercepts_BHCP)
setkey(interceptsDT_BHCP, "ID")
setkey(groupPredictKey_BHCP, "PoolID")
interceptsDT_BHCP <- interceptsDT_BHCP[groupPredictKey_BHCP]

setnames(interceptsDT_BHCP, "X2.5.",  "l95")
setnames(interceptsDT_BHCP, "X97.5.", "u95")

setnames(interceptsDT_BHCP, "X10.",  "l80")
setnames(interceptsDT_BHCP, "X90.",  "u80")


library(ggplot2)

ggIntercept_BHCP <-
    ggplot(interceptsDT_BHCP, aes(x = Pool, y = mean)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) + 
    coord_flip() +
    xlab("Pool") +
    ylab(expression(atop("Length-weight intercept",
                         "estimate ("*log[10]*log[10]*" scale)"))) +
    theme_minimal()

ggIntercept_BHCP
ggsave("intercept_BHCP.pdf", ggIntercept_BHCP, width = 4, height = 6)

################
## Plot slopes
slopes_BHCP <-
    data.frame(stanOutsummary_BHCP[[1]][grepl("(beta|gamma)(\\[\\d,2\\])",
                                              rownames(summary(stanOut_BHCP)[[1]])),
                                        ])
slopes_BHCP$ID = gsub("\\,2]|\\[", "", rownames(slopes_BHCP))
slopes_BHCP

groupPredictKey_BHCP <-
    dat3_BHCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey_BHCP
dat3_BHCP[ , .(PoolID, Pool)]

groupPredictKey_BHCP[ , PoolID := paste0("beta", PoolID)]
groupPredictKey_BHCP <-
    rbind(groupPredictKey_BHCP,
          data.table(Pool = "Hyper-parameter", PoolID = 'gamma1'))
groupPredictKey_BHCP

slopesDT_BHCP <- data.table(slopes_BHCP)
setkey(slopesDT_BHCP, "ID")
setkey(groupPredictKey_BHCP, "PoolID")
slopesDT_BHCP <- slopesDT_BHCP[groupPredictKey_BHCP]

setnames(slopesDT_BHCP, "X2.5.",  "l95")
setnames(slopesDT_BHCP, "X97.5.", "u95")

setnames(slopesDT_BHCP, "X10.",  "l80")
setnames(slopesDT_BHCP, "X90.",  "u80")

slopesDT_BHCP

ggSlope_BHCP <-
    ggplot(slopesDT_BHCP, aes(x = Pool, y = mean)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) + 
    coord_flip() +
    xlab("Pool") +
    ylab(expression(atop("Length-weight slope",
                         "estimate ("*log[10]*log[10]*" scale)"))) +
    theme_minimal()

ggSlope_BHCP
ggsave("slope_BHCP.pdf", ggSlope_BHCP, width = 4, height = 6)

## plot projections 
siteProjections_BHCP <-
    data.frame(
        stanOutsummary_BHCP[[1]][grepl("yProject",
                                       rownames(summary(stanOut_BHCP)[[1]])), ])
siteProjections_BHCP$parameter <- rownames(siteProjections_BHCP)
siteProjectionsDT_BHCP <- data.table(siteProjections_BHCP)
siteProjectionsDT_BHCP[ ,
                       PoolID := gsub("yProject\\[(\\d{1,2}),(\\d{1,2})\\]",
                                      "\\1", parameter)]
siteProjectionsDT_BHCP[ ,
                       lengthID :=
                           as.numeric(
                               gsub("yProject\\[(\\d{1,2}),(\\d{1,2})\\]",
                                    "\\2", parameter))]

setnames( siteProjectionsDT_BHCP, "X2.5.",  "l95")
setnames( siteProjectionsDT_BHCP, "X97.5.", "u95")
setnames( siteProjectionsDT_BHCP, "X10.",  "l80")
setnames( siteProjectionsDT_BHCP, "X90.",  "u80")

## Merge in length
lengthDT_BHCP <- data.table(lengthID = 1:length(xProject_BHCP),
                            length = xProject_BHCP)
setkey(lengthDT_BHCP, "lengthID")
setkey(siteProjectionsDT_BHCP, "lengthID")
siteProjectionsDT_BHCP <- siteProjectionsDT_BHCP[lengthDT_BHCP]

## Merge files
siteProjectionsDT_BHCP[ , PoolID := paste0("beta", PoolID)]
setkey(siteProjectionsDT_BHCP, "PoolID")
setkey(groupPredictKey_BHCP, "PoolID")

siteProjectionsDT_BHCP <-
    siteProjectionsDT_BHCP[groupPredictKey_BHCP[ PoolID !="gamma1", ]]

poolMinMax_BHCP <-
    dat3_BHCP[ , .(poolMin =min(TLmL10, na.rm = TRUE),
                   poolMax = max(TLmL10, na.rm = TRUE)), by = Pool]
setkey(siteProjectionsDT_BHCP, "Pool")
setkey(poolMinMax_BHCP, "Pool")
siteProjectionsDT_BHCP <-
    siteProjectionsDT_BHCP[ poolMinMax_BHCP]

head(siteProjectionsDT_BHCP[ , length >= poolMin])


GGlwData_BHCP <- ggplot() +
    geom_point(data = dat3_BHCP, aes( x = TLmL10,  y = WTkgL10)) +
    geom_line(data = siteProjectionsDT_BHCP, aes(x = length, y = mean),
              color = 'blue', size = 1.1) +
    facet_grid( ~ Pool) +
    ## geom_ribbon(data = siteProjectionsDT,
    ##             aes(x = length,  ymin = l95, ymax = u95), color = 'orange', alpha = 0.5) +
    ylab(expression(log[10]*"(weight kg)")) +
    xlab(expression(log[10]*"(length m)")) +
    theme_minimal()
GGlwData_BHCP
ggsave("lengthWeightData_BHCP.pdf", GGlwData_BHCP, width = 8, height = 4)

## exract out hyper parameter
yHyper_BHCP <-
    data.frame(stanOutsummary_BHCP[[1]][grepl(
                                      "yHyper",
                                      rownames(summary(stanOut_BHCP)[[1]])), ])
yHyper_BHCP$parameter = rownames(yHyper_BHCP)
yHyperDT_BHCP <- data.table(yHyper_BHCP)
yHyperDT_BHCP[ ,
              lengthID := as.numeric(gsub("yHyper\\[(\\d{1,2})\\]",
                                          "\\1", parameter))]
setnames( yHyperDT_BHCP, "X2.5.",  "l95")
setnames( yHyperDT_BHCP, "X97.5.", "u95")
setnames( yHyperDT_BHCP, "X10.",  "l80")
setnames( yHyperDT_BHCP, "X90.",  "u80")

## Merge in length
lengthDT_BHCP <- data.table(lengthID = 1:length(xProject_BHCP),
                            length = xProject_BHCP)
setkey(lengthDT_BHCP, "lengthID")
setkey(yHyperDT_BHCP, "lengthID")
yHyperDT_BHCP <-
    yHyperDT_BHCP[lengthDT_BHCP]


ggHyper_BHCP <-
    ggplot() +
    geom_line(data = siteProjectionsDT_BHCP,
              aes(x = length, y = mean, color = Pool), size = 1.1) +
    ylab(expression(log[10]*"(weight kg)")) +
    xlab(expression(log[10]*"(length m)")) +
    theme_minimal() +
    scale_color_manual( values = c("red", "blue", "seagreen",
                                   "orange", "skyblue", "navyblue")) +
    geom_ribbon(data = yHyperDT_BHCP,
                aes(x = length, ymin = l95, ymax = u95),
                fill = 'grey', alpha = 0.5) +
    geom_line(data = yHyperDT_BHCP,
              aes(x = length, y = mean),
              color = 'black', size = 1)

ggHyper_BHCP
ggsave("lengthWeightHyper_BHCP.pdf", ggHyper_BHCP, width = 6, height = 4) 

