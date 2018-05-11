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

## Look at relationship between total length and fork length
summary(dat[ Species == "SVCP", lm(TL ~ FL)])
summary(dat[ Species == "BHCP", lm(TL ~ FL)])

summary(dat[ Species %in% c( "SVCP", "BHCP"), lm(TL ~ FL * Species)])

FLvTL <- ggplot(data  = dat[Species %in% c( "SVCP", "BHCP"), ],
                aes(x = TL, y = FL)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = 'lm') +
    facet_grid( ~ Species) +
    theme_minimal() +
    ylab("Fork length") +
    xlab("Total length")

## Consider modeling total length from fork length here

dat2 <- dat[ !is.na(TL) & !is.na(Age), ]
dat2[ , .N, by = Pool]


dat2[ , Age2 := floor(Age)]

## Dave, why the -5? 
dat2[ , Age3 := Age2 + (month(Sampdate)-5)/12]
dat2[ , Pool :=factor(Pool)]
dat2[ , levels(Pool)]
dat2[ , PoolID := as.numeric(Pool)]

dat2[ , .N, by = Pool]

## Plot only SVCP across all pools for summary slides
AvTL <- ggplot(dat2[ Species %in% c( "SVCP"), ], aes(x = Age2, y = TL/1000)) +
    geom_point(alpha = 0.25) +
    facet_grid( Species~Pool) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal()

## Plot by pools
AvTLbyPool <- ggplot(dat2[ Species %in% c( "SVCP", "BHCP"), ],
                     aes(x = Age2, y = TL/1000)) +
    geom_point(alpha = 0.25) +
    facet_grid( Species~Pool) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal()

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
dat3[ , TLm := TL/1000]

## Explore prior for expoendital
x <- seq(0.01, 5, by = 0.01)
expPlot <- data.table(x = x,
                      y1   = dexp(x, 1),
                      y0.5 = dexp(x, 0.5),
                      y2   = dexp(x, 2),
                      y3   = dexp(x, 3),
                      y4   = dexp(x, 4))
expPlot <- melt(expPlot, id.vars = 'x', variable.name= 'rate', value.name = 'y')
expPlot[ , rate := as.numeric(gsub( "y", "", rate))]

ggexp <- ggplot(expPlot, aes(x = x, y = y, color = rate, group = rate)) + geom_line()


stanData <- list(
    nFish  = dim(dat3)[1],
    nSites = length(dat3[, unique(PoolID)]),
    length = dat3[ , TLm], 
    poolID = dat3[ , PoolID],
    age = dat3[ , Age3],
    hp_tau = 1.5,
    hp_sigma = 10,
    hp_omega = 2,
    p_mu_gamma = 0,
    p_mu_gammaSD = 2
    )

names(stanData)
dat3[ , max(TLm, na.rm = TRUE), by = .(Species, Pool)]
dat3[ , quantile(TLm, probs = 0.80, na.rm = TRUE), by = .(Species, Pool)]


#stanOut <- stan(file = "vonB.stan", data = stanData, chains = 4, iter = 1000,
#                control = list(adapt_delta = 0.8))

#stanOut

## stanOutMV <- stan(file = "vonBmultivariate.stan",
##                   data = stanData, chains = 4, iter = 1000,
##                   control = list(adapt_delta = 0.8))


stanOutO <- stan(file = "vonBo.stan",
                 data = stanData, chains = 4, iter = 3000,
                 control = list(adapt_delta = 0.8))

## stanOutMV
stanOutO

summary(stanOutO)[[1]][grepl("M", rownames(summary(stanOutO)[[1]])), ]
summary(stanOutO)[[1]][grepl("M", rownames(summary(stanOutO)[[1]])), ][ , 1]


summary(stanOutO)[[1]][grepl("K|Linf", rownames(summary(stanOutO)[[1]])), ]

summary(stanOutO)[[1]][grepl("Linf", rownames(summary(stanOutO)[[1]])), ]

Linf = summary(stanOutO)[[1]][grepl("Linf", rownames(summary(stanOutO)[[1]])), ][1, 1]
Linf
Linf ^-0.33
K = summary(stanOutO)[[1]][grepl("K", rownames(summary(stanOutO)[[1]])), ][1, 1]
K


4.118 * K ^(0.73) * (Linf * 100) ^(-0.33)

4.118 * (K ^(0.73))
(Linf * 100.0) ^(-0.33)


## plot(stanOutMV, pars =c("sigmaLength", "tau", "Omega"))

## plot(stanOutMV, pars =c("Linf_bar", "t0_bar", "K_bar", "Linf"))
## plot(stanOutMV, pars =c( "t0_bar", "t0"))

plot(stanOutO, pars =c("Linf_bar", "t0_bar", "K_bar", "Linf"))
quartz()
plot(stanOutO, pars =c( "t0_bar", "t0"))

traceplot(stanOutO, pars =c( "t0_bar", "t0"))

plot(stanOutO, pars =c( "M"))

## plot(stanOutMV, pars =c( "Linf"))

## plot(stanOutMV, pars =c("Linf_bar", "Linf"))


## Plot results 
ggplot(dat2[ Species %in% c( "SVCP"), ], aes(x = Age2, y = TL/1000)) +
    geom_point(alpha = 0.25) +
    facet_grid( Species~Pool) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal()
