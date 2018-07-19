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
## 
dat2[ Species == "SVCP", .N, by = Pool][ order(N, decreasing = TRUE), ]

dat3 <- dat2[ Pool %in% c("LaGrange", "Alton", "Peoria",
                          "Marseilles", "Pool 26", "Pool 27") ,][ Species == "SVCP", ]

dat3[ , Pool := factor(Pool)]
dat3[ , PoolID := as.numeric(Pool)]
dat3[ , .N, by = .(Pool, Species)]

## Convert to M from mm to stabilisze results

dat3[ , summary(TL/1000)]
dat3[ , TLm  := TL/1000]
dat3[ , WTkg := WT/1000]

## Creat group-level predictors
dat3[ , Pool := factor(Pool)]
dat3[ , PoolID := as.numeric(Pool)]

## Log10 transorm data and center it 
dat3[ , TLmL10 := log10(TLm) - mean(log10(TLm))]
dat3[ , WTkgL10 := log10(WTkg)]

## build simple length-weight model
## with hardcoded regression 
s1 <- list(
    N  = dim(dat3)[1],
    y  = dat3[ , WTkgL10],
    x  = dat3[ , TLmL10]
    )

lwSimpleOut <- stan(file = "lwSimple.stan",
                    data = s1,
                    chains = 4, iter = 1000,
                    control = list(adapt_delta = 0.8))
lwSimpleOut

## build length-weight model
## with matrix input
x <- model.matrix( ~ TLmL10, data = dat3)

s2 <- list(
    N  = dim(dat3)[1],
    K  = ncol(x),
    y  = dat3[ , WTkgL10],
    x  = x
    )

lwSimMatOut <- stan(file = "lwSimpleMatrix.stan",
                    data = s2,
                    chains = 4, iter = 1000,
                    control = list(adapt_delta = 0.8))
lwSimMatOut

## run model with correlated structure

x  = dat3[ , model.matrix( ~ TLmL10)]
x[1, ]

groupPredictKey <- dat3[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey
dat3[ , .(PoolID, Pool)]

u  = matrix(rep(1, length(dat3[, unique(PoolID)])), ncol = 1)
u

## output
dat3[ , summary(WTkgL10)]
## input
dat3[ , summary(TLmL10)]

xProject <- seq(-1, 0.25, length.out = 10)
xProject

stanData <- list(
    N  = dim(dat3)[1], # Num obs
    J  = length(dat3[, unique(PoolID)]), # num groups 
    L  = 1, # num group predictors 
    y  = dat3[ , WTkgL10], # observations 
    jj = dat3[ , PoolID], # groups for each indivdual 
    x  = x, # individual predictor matrix 
    u  = u, # group predictors 
    K  = ncol(x), # num individual predictors
    xProject = xProject,
    nProject = length(xProject)
)

stanOut <- stan(file = "lengthWeight.stan", data = stanData,
                chains = 4, iter = 2000,
                control = list(adapt_delta = 0.8))
save(stanOut, file = "lengthWeight2.RData")
## load("lengthWeight2.RData")
stanOut

##########################################################
##########################################################
## Define paratere here: see page 147 to better describe 
## beta are individual-level parameters
## sigma corresponds to this level
## gamma are hyper-parameter for beta
## tau corresponds to this level
## omega is correlation matrix with tau coefficients

stanOutsummary <- summary(stanOut, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutsummary[[1]][grepl("beta", rownames(summary(stanOut)[[1]])), ]
stanOutsummary[[1]][grepl("gamma", rownames(summary(stanOut)[[1]])), ]
stanOutsummary[[1]][grepl("tau", rownames(summary(stanOut)[[1]])), ]
stanOutsummary[[1]][grepl("Omega", rownames(summary(stanOut)[[1]])), ]


################
## Plot intercepts
intercepts <- data.frame(stanOutsummary[[1]][grepl("(beta|gamma)(\\[\\d,1\\])", rownames(summary(stanOut)[[1]])), ])
intercepts$ID = gsub("\\,1]|\\[", "", rownames(intercepts))
intercepts

groupPredictKey <- dat3[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey
dat3[ , .(PoolID, Pool)]
groupPredictKey[ , PoolID := paste0("beta", PoolID)]
groupPredictKey <- rbind(groupPredictKey, data.table(Pool = "Hyper-parameter", PoolID = 'gamma1'))
groupPredictKey

interceptsDT <- data.table(intercepts)
setkey(interceptsDT, "ID")
setkey(groupPredictKey, "PoolID")
interceptsDT <- interceptsDT[groupPredictKey]

setnames(interceptsDT, "X2.5.",  "l95")
setnames(interceptsDT, "X97.5.", "u95")

setnames(interceptsDT, "X10.",  "l80")
setnames(interceptsDT, "X90.",  "u80")


library(ggplot2)

ggIntercept <- ggplot(interceptsDT, aes(x = Pool, y = mean)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) + 
    coord_flip() +
    xlab("Pool") +
    ylab(expression(atop("Length-weight intercept", "estimate ("*log[10]*log[10]*" scale)"))) +
    theme_minimal()

ggIntercept
ggsave("intercept.pdf", ggIntercept, width = 4, height = 6)

################
## Plot slopes
slopes <- data.frame(stanOutsummary[[1]][grepl("(beta|gamma)(\\[\\d,2\\])", rownames(summary(stanOut)[[1]])), ])
slopes$ID = gsub("\\,2]|\\[", "", rownames(slopes))
slopes

groupPredictKey <- dat3[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey
dat3[ , .(PoolID, Pool)]
groupPredictKey[ , PoolID := paste0("beta", PoolID)]
groupPredictKey <- rbind(groupPredictKey, data.table(Pool = "Hyper-parameter", PoolID = 'gamma1'))
groupPredictKey

slopesDT <- data.table(slopes)
setkey(slopesDT, "ID")
setkey(groupPredictKey, "PoolID")
slopesDT <- slopesDT[groupPredictKey]

setnames(slopesDT, "X2.5.",  "l95")
setnames(slopesDT, "X97.5.", "u95")

setnames(slopesDT, "X10.",  "l80")
setnames(slopesDT, "X90.",  "u80")

slopesDT

ggSlope <- ggplot(slopesDT, aes(x = Pool, y = mean)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) + 
    coord_flip() +
    xlab("Pool") +
    ylab(expression(atop("Length-weight slope", "estimate ("*log[10]*log[10]*" scale)"))) +
    theme_minimal()

ggSlope
ggsave("slope.pdf", ggSlope, width = 4, height = 6)

## plot projections 
siteProjections <- data.frame(stanOutsummary[[1]][grepl("yProject", rownames(summary(stanOut)[[1]])), ])
siteProjections$parameter <- rownames(siteProjections)
siteProjectionsDT <- data.table(siteProjections)
siteProjectionsDT[ , PoolID := gsub("yProject\\[(\\d{1,2}),(\\d{1,2})\\]", "\\1", parameter)]
siteProjectionsDT[ , lengthID := as.numeric(gsub("yProject\\[(\\d{1,2}),(\\d{1,2})\\]", "\\2", parameter))]
setnames( siteProjectionsDT, "X2.5.",  "l95")
setnames( siteProjectionsDT, "X97.5.", "u95")
setnames( siteProjectionsDT, "X10.",  "l80")
setnames( siteProjectionsDT, "X90.",  "u80")
## Merge in length
lengthDT <- data.table(lengthID = 1:length(xProject), length = xProject)
setkey(lengthDT, "lengthID")
setkey(siteProjectionsDT, "lengthID")
siteProjectionsDT <- siteProjectionsDT[lengthDT]
## Merge files
siteProjectionsDT[ , PoolID := paste0("beta", PoolID)]
setkey(siteProjectionsDT, "PoolID")
setkey(groupPredictKey, "PoolID")

siteProjectionsDT <- siteProjectionsDT[groupPredictKey[ PoolID !="gamma1", ]]

poolMinMax <- dat3[ , .(poolMin =min(TLmL10, na.rm = TRUE), poolMax = max(TLmL10, na.rm = TRUE)), by = Pool]
setkey(siteProjectionsDT, "Pool")
setkey(poolMinMax, "Pool")
siteProjectionsDT <- siteProjectionsDT[ poolMinMax]

head(siteProjectionsDT[ , length >= poolMin])


GGlwData <- ggplot() +
    geom_point(data = dat3, aes( x = TLmL10,  y = WTkgL10)) +
    geom_line(data = siteProjectionsDT, aes(x = length, y = mean), color = 'blue', size = 1.1) +
    facet_grid( ~ Pool) +
    ## geom_ribbon(data = siteProjectionsDT,
    ##             aes(x = length,  ymin = l95, ymax = u95), color = 'orange', alpha = 0.5) +
    ylab(expression(log[10]*"(weight kg)")) +
    xlab(expression(log[10]*"(length m)")) +
    theme_minimal()
GGlwData
ggsave("lengthWeightData.pdf", GGlwData, width = 8, height = 4)

## exract out hyper parameter
yHyper <- data.frame(stanOutsummary[[1]][grepl("yHyper", rownames(summary(stanOut)[[1]])), ])
yHyper$parameter = rownames(yHyper)
yHyperDT <- data.table(yHyper)
yHyperDT[ , lengthID := as.numeric(gsub("yHyper\\[(\\d{1,2})\\]", "\\1", parameter))]
setnames( yHyperDT, "X2.5.",  "l95")
setnames( yHyperDT, "X97.5.", "u95")
setnames( yHyperDT, "X10.",  "l80")
setnames( yHyperDT, "X90.",  "u80")
## Merge in length
lengthDT <- data.table(lengthID = 1:length(xProject), length = xProject)
setkey(lengthDT, "lengthID")
setkey(yHyperDT, "lengthID")
yHyperDT <-
    yHyperDT[lengthDT]


ggHyper <- ggplot() +
    geom_line(data = siteProjectionsDT, aes(x = length, y = mean, color = Pool),, size = 1.1) +
    ylab(expression(log[10]*"(weight kg)")) +
    xlab(expression(log[10]*"(length m)")) +
    theme_minimal() +
    scale_color_manual( values = c("red", "blue", "seagreen",
                                   "orange", "skyblue", "navyblue")) +
    geom_ribbon(data = yHyperDT, aes(x = length, ymin = l95, ymax = u95), fill = 'grey', alpha = 0.5) +
    geom_line(data = yHyperDT, aes(x = length, y = mean), color = 'black', size = 1)

ggHyper
ggsave("lengthWeightHyper.pdf", ggHyper, width = 6, height = 4) 

