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

head(dat)

dat2 <- dat[ Maturity != "NA",] 
dat2[ , Maturity := factor(Maturity)]
dat2[ , M2 := as.numeric(Maturity) - 1]
dat2[ , TLm := TL / 1000]


ggplot(data  = dat2[Species %in% c( "SVCP", "BHCP"), ],
       aes(x = TLm, y = GonadWT)) +
    geom_point(alpha = 0.5) +
    facet_grid( ~ Species) +
    theme_minimal() +
    ylab("Gonad weight") +
    xlab("Total length")


ggplot(data  = dat2[Species %in% c( "SVCP", "BHCP"), ],
       aes(x = TL, y = M2)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = 'glm',  method.args = list(family = "binomial")) +
    facet_grid( . ~ Species) +
    theme_minimal() +
    ylab("Maturity") +
    xlab("Total length (m)")


dat2[ , .N, by = Pool]


## Plot by pools
ggplot(dat2[ Species %in% c( "SVCP", "BHCP"), ], aes(x = TLm, y = M2)) +
    geom_point(alpha = 0.25) +
    facet_grid( Species~Pool) +
    xlab("Length (m)") +
    ylab("Maturity") + theme_minimal()

## Cannot stiamte by pool, not enough data, so we must pool across pools...

dat2[ Species == "SVCP", .N, by = Pool][ order(N, decreasing = TRUE), ]

dat3 <- dat2[ Pool %in% c("LaGrange", "Alton", "Peoria",
                          "Marseilles", "Pool 26", "Pool 27") ,][ Species == "SVCP", ]

## Convert to M to stabilisze results

stanData <- list(
    N  = dim(dat3)[1],
    x = dat3[ , TLm], 
    y = dat3[ , M2]
)

names(stanData)


stanOut <- stan(file = "maturity.stan", data = stanData, chains = 4, iter = 1000,
               control = list(adapt_delta = 0.8))

stanOut
plot(stanOut)

traceplot(stanOut)
traceplot(stanOut, inc_warmup = TRUE)

pairs(stanOut)
traceplot(stanOut, inc_warmup = TRUE)
traceplot(stanOut, inc_warmup = FALSE)

summaryOut <- summary(stanOut)$summary
summary(summaryOut[, "Rhat"])
