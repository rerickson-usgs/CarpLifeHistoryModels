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
FLvTL
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

ageProjection = seq(0, 20, by = 1)

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
    p_mu_gammaSD = 2,
    nProject = length(ageProjection),
    ageProject = ageProjection
    )

names(stanData)

## Model takes ~0.5 hrs to run.
## the stan function is commented out and the outputs loaded from a saved file
## unless it needs to be re-run. 
## stanOutO <- stan(file = "vonBo.stan",
##                  data = stanData, chains = 4, iter = 6000,
##                  control = list(adapt_delta = 0.8))

## save(stanOutO, file = "vonBfit.RData")
load("vonBfit.RData")

stanOutO
stanOutOsummary <- summary(stanOutO, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutOsummary[[1]][grepl("M", rownames(summary(stanOutO)[[1]])), ]

stanOutOsummary[[1]][grepl("bar", rownames(summary(stanOutO)[[1]])), ]
stanOutOsummary[[1]][grepl("K", rownames(summary(stanOutO)[[1]])), ]
stanOutOsummary[[1]][grepl("Linf", rownames(summary(stanOutO)[[1]])), ]

stanOutOsummaryDT <- data.table(stanOutOsummary[[1]])
stanOutOsummaryDT[ , Parameter := rownames(stanOutOsummary[[1]])]


plot(stanOutO, pars =c("Linf_bar", "t0_bar", "K_bar", "Linf"))
quartz()
plot(stanOutO, pars =c( "t0_bar", "t0"))

traceplot(stanOutO, pars =c( "t0_bar", "t0"))

poolNames <- dat3[ , levels(Pool)]

## Plot parameter estimates

## Plot linf
LinfPlot <- copy(stanOutOsummaryDT[ grepl("Linf", Parameter), ])
poolNamesDT <- data.table(Pool = c(poolNames, "across sites"),
                        PoolID = c(1:6, "Linf_bar"))
setkey(poolNamesDT, "PoolID")

LinfPlot[ , PoolID := gsub("(Linf)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(LinfPlot, "PoolID")

LinfPlot <- poolNamesDT[ LinfPlot]
setnames(LinfPlot,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


LinfPlotGG <- ggplot(data = LinfPlot, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(L)[infinity]))
print(LinfPlotGG)
ggsave("LinfPlot.pdf", LinfPlotGG, width = 3, height = 4)

LinfPlot

## Plot growth rates
KPlot <- copy(stanOutOsummaryDT[ grepl("K", Parameter), ])
poolNamesDT <- data.table(Pool = c(poolNames, "across sites"),
                        PoolID = c(1:6, "K_bar"))
setkey(poolNamesDT, "PoolID")

KPlot[ , PoolID := gsub("(K)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(KPlot, "PoolID")

KPlot <- poolNamesDT[ KPlot]
setnames(KPlot,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


KPlotGG <- ggplot(data = KPlot, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(K)))
print(KPlotGG)
ggsave("KPlot.pdf", KPlotGG, width = 3, height = 4)

## Plot natural mortality across sites
MPlot <- copy(stanOutOsummaryDT[ grepl("M", Parameter), ])
MPlot
poolNamesDT <- data.table(Pool = c(poolNames, "across sites"),
                        PoolID = c(1:6, "M_bar"))
setkey(poolNamesDT, "PoolID")

MPlot[ , PoolID := gsub("(M)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(MPlot, "PoolID")

MPlot <- poolNamesDT[ MPlot]
setnames(MPlot,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


MPlotGG <- ggplot(data = MPlot, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(M)))
print(MPlotGG)
ggsave("MPlot.pdf", MPlotGG, width = 3, height = 4)

## Plot theoretical time of zero length
t0Plot <- copy(stanOutOsummaryDT[ grepl("t0", Parameter), ])
poolNamesDT <- data.table(Pool = c(poolNames, "across sites"),
                        PoolID = c(1:6, "t0_bar"))
setkey(poolNamesDT, "PoolID")

t0Plot[ , PoolID := gsub("(t0)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(t0Plot, "PoolID")

t0Plot <- poolNamesDT[ t0Plot]
setnames(t0Plot,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


t0PlotGG <- ggplot(data = t0Plot, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(t)[0]))
print(t0PlotGG)
ggsave("t0Plot.pdf", t0PlotGG, width = 3, height = 4)

## Extract out hyper projections
hyperProjection <- copy(stanOutOsummaryDT[ grepl("hyperProjection", Parameter), ])
hyperProjection[ , age := as.numeric(gsub("hyperProjection|\\[|\\]", "", Parameter))]
hyperProjection[ , Pool := "Across pools"]

## Extract out site projections 
siteProjections <- copy(stanOutOsummaryDT[ grepl("siteProjection", Parameter)])
siteProjections[ , PoolID :=  gsub("(siteProjections\\[)(\\d),(\\d{1,2})\\]", "\\2", Parameter)]
siteProjections[ , age :=  as.numeric(gsub("(siteProjections\\[)(\\d),(\\d{1,2})\\]", "\\3", Parameter))]

setkey(siteProjections, "PoolID")

siteProjections<- siteProjections[poolNamesDT][ Pool!= "across sites",]

siteProjections[ , PoolID := NULL]

setnames(siteProjections,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))
setnames(hyperProjection,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


allProjections <- rbind(siteProjections,
                        hyperProjection)

stanOutOsummaryDT[ !grepl("siteProjections|hyperProjection|t0|K|Linf|M|Omega|mu_beta_raw|tau|mu", Parameter), ]

siteProjections[ , Pool := factor(Pool)]
siteProjections[ , Pool := factor(Pool, levels = levels(Pool)[c(5, 6, 1, 2, 4, 3)])]


dataVBplot <- ggplot() + 
    geom_point(data = dat3, aes(x = Age3, y = TLm), alpha = 0.25) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    geom_line(data = siteProjections[  age < 13, ],
              aes(x = age, y = mean), color = 'blue')   + 
    geom_ribbon(data = siteProjections[  age < 13, ],
                aes(x = age, ymin = L80, ymax = U80), fill = 'blue',
                alpha = 0.25)  +
    geom_ribbon(data = siteProjections[  age < 13, ],
                aes(x = age, ymin = L95, ymax = U95), fill = 'blue',
                alpha = 0.25)  +
    facet_wrap( ~ Pool, nrow = 2) +
    scale_x_continuous(breaks = ageProjection)
print(dataVBplot)
ggsave("dataVBplot.pdf", dataVBplot, width = 6, height = 6)

hyperPlot <-
    ggplot(data = hyperProjection[ age < 13, ], aes(x = age, y = mean)) +
    geom_line(color = 'black', size = 1.3) +
    geom_ribbon(aes(x = age, ymin = L95, ymax = U95), fill = 'grey50',
                alpha = 0.25) + 
    geom_ribbon(aes(x = age, ymin = L80, ymax = U80), fill = 'grey50',
                alpha = 0.25) +
    geom_line(data = siteProjections[ age < 13, ],
              aes(x = age, y = mean, color = Pool),
              size = 1.1) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    scale_x_continuous(breaks = ageProjection) +
    scale_color_manual( values = c("red", "blue", "seagreen",
                                   "orange", "skyblue", "navyblue")) 
print(hyperPlot)
ggsave("hyperPlot.pdf", hyperPlot, width = 6, height = 4)


## Extract out raw parameter estimates for SEACarP sims

