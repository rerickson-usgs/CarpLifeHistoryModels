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

dat2 <- dat[ !is.na(TL) & !is.na(WT), ]
## Get data from Stan
## 
dat2[ Species == "SVCP", .N, by = Pool][ order(N, decreasing = TRUE), ]

dat3 <- dat2[ Pool %in% c("LaGrange", "Alton", "Peoria",
                          "Marseilles", "Pool 26", "Pool 27") ,][ Species == "SVCP", ]

dat3[ , Pool := factor(Pool)]
dat3[ , PoolID := as.numeric(Pool)]
dat3[ , .N, by = .(Pool, Species)]

## Convert to M to stabilisze results

dat3[ , summary(TL/1000)]
dat3[ , TLm  := TL/1000]
dat3[ , WTkg := WT/1000]

## Creat group-level predictors

x  = dat3[ , model.matrix( ~ Pool - 1 + TLm)]

###############
## AM HERE creating u matrix (see page 149 of stan user manual)

u  = model.matrix( ~ factor(data.frame(pool = colnames(x)[ -(ncol(x))])))

## Explore prior for expoendital
stanData <- list(
    N  = dim(dat3)[1],
    J  = length(dat3[, unique(PoolID)]),
    L  = length(dat3[, unique(PoolID)]),
    y  = dat3[ , WTkg]
    x  = x,
    length = dat3[ , TLm], 
    poolID = dat3[ , PoolID],
    weight = dat3[ , WTkg]
    )

stanOut <- stan(file = "vonB.stan", data = stanData, chains = 4, iter = 1000,
               control = list(adapt_delta = 0.8))


plot(stanOutMV, pars =c("sigmaLength", "tau", "Omega"))

plot(stanOutMV, pars =c("Linf_bar", "t0_bar", "K_bar", "Linf"))
plot(stanOutMV, pars =c( "t0_bar", "t0"))

plot(stanOutO, pars =c("Linf_bar", "t0_bar", "K_bar", "Linf"))
quartz()
plot(stanOutO, pars =c( "t0_bar", "t0"))

traceplot(stanOutO, pars =c( "t0_bar", "t0"))

plot(stanOutMV, pars =c( "Linf"))

plot(stanOutMV, pars =c("Linf_bar", "Linf"))



pairs(stanOut)
traceplot(stanOut, inc_warmup = TRUE)
traceplot(stanOut, inc_warmup = FALSE)

summaryOut <- summary(stanOut)$summary
summary(summaryOut[, "Rhat"])

parLook <- rownames(summaryOut)[!grepl("lengthHat", rownames(summaryOut))]

summaryOut[ rownames(summaryOut) %in% parLook,]

