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

stanData <- list(
    N  = dim(dat3)[1], # Num obs
    J  = length(dat3[, unique(PoolID)]), # num groups 
    L  = 1, # num group predictors 
    y  = dat3[ , WTkgL10], # observations 
    jj = dat3[ , PoolID], # groups for each indivdual 
    x  = x, # individual predictor matrix 
    u  = u, # group predictors 
    K  = ncol(x) # num individual predictors 
)

## stanOut <- stan(file = "lengthWeight.stan", data = stanData,
##                 chains = 4, iter = 400,
##                 control = list(adapt_delta = 0.8))
## save(stanOut, file = "lengthWeight2.RData")
load("lengthWeight2.RData")
stanOut

############################
############################
## Define paratere here: see page 147 to better describe 
## beta are individual-level parameters
## sigma corresponds to this level
## gamma are hyper-parameter for beta
## tau corresponds to this level
## omega is correlation matrix with tau coefficients
lwSimpleOut

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

groupPredictKey[ , PoolID := paste0("beta", PoolID)]
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
    ylab(expression(over("Length-weight intercept", "estimate ("*log[10]*log[10]*" scale)"))) +
    theme_minimal()

ggIntercept
ggsave("intercept.pdf", ggIntercept, width = 4, height = 6)

################
## Plot slopes
slopes <- data.frame(stanOutsummary[[1]][grepl("(beta|gamma)(\\[\\d,2\\])", rownames(summary(stanOut)[[1]])), ])
slopes$ID = gsub("\\,2]|\\[", "", rownames(slopes))
slopes

groupPredictKey[ , PoolID := paste0("beta", PoolID)]
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

library(ggplot2)

ggSlope <- ggplot(slopesDT, aes(x = Pool, y = mean)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) + 
    coord_flip() +
    xlab("Pool") +
    ylab(expression(over("Length-weight slope", "estimate ("*log[10]*log[10]*" scale)"))) +
    theme_minimal()

ggSlope
ggsave("slope.pdf", ggSlope, width = 4, height = 6)

