library(data.table) # used for data manipulation
library(lubridate) # used to format date
library(ggplot2) # used for plotting 
library(rstan) # used to fit Bayesian model
options(mc.cores = parallel::detectCores())

rerunStan = TRUE

## Read in a format data
dat <- fread("../Demographics_080318.csv")

dat[ , Sampdate :=ymd(Sampdate)] 
dat[,  FL := as.numeric(FL)]

dat[ , unique(Species)]
dat[ , unique(Pool)]
dat[ , unique(Species)]
dat[ , unique(Pool)]
dat[ Pool == "OR(pool 27)", Pool := "Pool 27"]
dat[ Pool == "Dresden", Pool := "Dresden Island"]

## FLvTL <- ggplot(data  = dat[Species %in% c( "SVCP", "BHCP"), ],
##                 aes(x = TL, y = FL)) +
##     geom_point(alpha = 0.5) +
##     stat_smooth(method = 'lm') +
##     facet_grid( ~ Species) +
##     theme_minimal() +
##     ylab("Fork length") +
##     xlab("Total length")
## FLvTL

dat2 <- dat[ !is.na(TL) & !is.na(Age), ]
## dat2[ , .N, by = Pool]
dat2[ , Age2 := floor(Age)]

dat2[ , Age3 := Age2 + (month(Sampdate)-5)/12]
dat2[ , Pool :=factor(Pool)]
## dat2[ , levels(Pool)]                   
dat2[ , PoolID := as.numeric(Pool)]

## dat[ , .N, by = Pool][ order(N, decreasing = TRUE)]
## dat2[ , .N, by = Pool][ order(N, decreasing = TRUE)]

## Plot only SVCP across all pools for summary slides
## AvTL <- ggplot(dat2[ Species %in% c( "SVCP"), ], aes(x = Age2, y = TL/1000)) +
##     geom_point(alpha = 0.25) +
##     facet_grid( Species~Pool) +
##     xlab("Age (years)") +
##     ylab("Length (m)") + theme_minimal()

## Plot by pools
## AvTLbyPool <- ggplot(dat2[ Species %in% c( "SVCP", "BHCP"), ],
##                      aes(x = Age2, y = TL/1000)) +
##     geom_point(alpha = 0.25) +
##     facet_grid( Species~Pool) +
##     xlab("Age (years)") +
##     ylab("Length (m)") + theme_minimal()

## Get data from Stan

## dat2[ Species == "SVCP", .N, by = Pool][ order(N, decreasing = TRUE), ]

## Silver carp SVCP analysis
dat3_SVCP <- dat2[ Species == "SVCP", ] 
dat3_SVCP[ , Pool := factor(Pool)]
dat3_SVCP[ , PoolID := as.numeric(Pool)]
## dat3_SVCP[ , .N, by = .(Pool, Species)]

## Convert to M to stabilisze results
dat3_SVCP[ , TLm := TL/1000]

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

stanData_SVCP <- list(
    nFish  = dim(dat3_SVCP)[1],
    nSites = length(dat3_SVCP[, unique(PoolID)]),
    length = dat3_SVCP[ , TLm], 
    poolID = dat3_SVCP[ , PoolID],
    age = dat3_SVCP[ , Age3],
    hp_tau = 1.5,
    hp_sigma = 10,
    hp_omega = 2,
    p_mu_gamma = 0,
    p_mu_gammaSD = 2,
    nProject = length(ageProjection),
    ageProject = ageProjection
    )

## Model takes ~0.25 hrs to run.
## the stan function is commented out and the outputs loaded from a saved file
## unless it needs to be re-run. 
if(rerunStan){
    stanOutO_SVCP <- stan(file = "vonBoNot0.stan",
                          data = stanData_SVCP, chains = 4, iter = 6000,
                          control = list(adapt_delta = 0.8))
    save(stanOutO_SVCP, file = "vonBfitNot0_SVCP.RData")
} else {
    load("vonBfitNot0_SVCP.RData")
}

## stanOutO_SVCP
stanOutOsummary_SVCP <-
    summary(stanOutO_SVCP, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutOsummary_SVCP[[1]][grepl("M", rownames(summary(stanOutO_SVCP)[[1]])), ]

stanOutOsummary_SVCP[[1]][grepl("bar", rownames(summary(stanOutO_SVCP)[[1]])), ]
stanOutOsummary_SVCP[[1]][grepl("K", rownames(summary(stanOutO_SVCP)[[1]])), ]
stanOutOsummary_SVCP[[1]][grepl("Linf", rownames(summary(stanOutO_SVCP)[[1]])),]

stanOutOsummaryDT_SVCP <- data.table(stanOutOsummary_SVCP[[1]])
stanOutOsummaryDT_SVCP[ , Parameter := rownames(stanOutOsummary_SVCP[[1]])]


plot(stanOutO_SVCP, pars =c("Linf_bar", "K_bar", "Linf"))

poolNames_SVCP <- dat3_SVCP[ , levels(Pool)]

## Plot parameter estimates

## Plot linf
LinfPlot_SVCP <- copy(stanOutOsummaryDT_SVCP[ grepl("Linf", Parameter), ])
poolNamesDT_SVCP <- data.table(Pool = c(poolNames_SVCP, "across sites"),
                        PoolID = c(1:length(poolNames_SVCP), "Linf_bar"))
setkey(poolNamesDT_SVCP, "PoolID")

LinfPlot_SVCP[ , PoolID := gsub("(Linf)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(LinfPlot_SVCP, "PoolID")

LinfPlot_SVCP <- poolNamesDT_SVCP[ LinfPlot_SVCP]
setnames(LinfPlot_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))

LinfPlot_SVCP[ , Pool := factor(Pool)]

LinfPlotGG_SVCP <- ggplot(data = LinfPlot_SVCP, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(L)[infinity]))
print(LinfPlotGG_SVCP)
ggsave("LinfPlotNot0_SVCP.pdf", LinfPlotGG_SVCP, width = 3, height = 4)

## Plot growth rates
KPlot_SVCP <- copy(stanOutOsummaryDT_SVCP[ grepl("K", Parameter), ])
poolNamesDT_SVCP <- data.table(Pool = c(poolNames_SVCP, "across sites"),
                               PoolID = c(1:length(poolNames_SVCP), "K_bar"))

setkey(poolNamesDT_SVCP, "PoolID")

KPlot_SVCP[ , PoolID := gsub("(K)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(KPlot_SVCP, "PoolID")

KPlot_SVCP <- poolNamesDT_SVCP[ KPlot_SVCP]
setnames(KPlot_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


KPlot_SVCP[ , Pool := factor(Pool)]


KPlotGG_SVCP <- ggplot(data = KPlot_SVCP, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(K)))
print(KPlotGG_SVCP)
ggsave("KPlotNot0_SVCP.pdf", KPlotGG_SVCP, width = 3, height = 4)

## Plot natural mortality across sites
MPlot_SVCP <- copy(stanOutOsummaryDT_SVCP[ grepl("M", Parameter), ])
MPlot_SVCP
poolNamesDT_SVCP <- data.table(Pool = c(poolNames_SVCP, "across sites"),
                        PoolID = c(1:length(poolNames_SVCP), "M_bar"))
setkey(poolNamesDT_SVCP, "PoolID")

MPlot_SVCP[ , PoolID := gsub("(M)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(MPlot_SVCP, "PoolID")

MPlot_SVCP <- poolNamesDT_SVCP[ MPlot_SVCP]
setnames(MPlot_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))

MPlot_SVCP[ , Pool := factor(Pool)]



MPlotGG_SVCP <- ggplot(data = MPlot_SVCP, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(M)))
print(MPlotGG_SVCP)
ggsave("MPlotNot0_SVCP.pdf", MPlotGG_SVCP, width = 3, height = 4)


## Extract out hyper projections
hyperProjection_SVCP <- copy(stanOutOsummaryDT_SVCP[ grepl("hyperProjection", Parameter), ])
hyperProjection_SVCP[ , age := as.numeric(gsub("hyperProjection|\\[|\\]", "", Parameter))]
hyperProjection_SVCP[ , Pool := "Across pools"]

## Extract out site projections 
siteProjections_SVCP <- copy(stanOutOsummaryDT_SVCP[ grepl("siteProjection", Parameter)])
siteProjections_SVCP[ , PoolID :=  gsub("(siteProjections\\[)(\\d),(\\d{1,2})\\]", "\\2", Parameter)]
siteProjections_SVCP[ , age :=  as.numeric(gsub("(siteProjections\\[)(\\d),(\\d{1,2})\\]", "\\3", Parameter))]

setkey(siteProjections_SVCP, "PoolID")

siteProjections_SVCP<- siteProjections_SVCP[poolNamesDT_SVCP][ Pool!= "across sites",]

siteProjections_SVCP[ , PoolID := NULL]

setnames(siteProjections_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))
setnames(hyperProjection_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


allProjections_SVCP <- rbind(siteProjections_SVCP,
                             hyperProjection_SVCP)

stanOutOsummaryDT_SVCP[ !grepl("siteProjections|hyperProjection|t0|K|Linf|M|Omega|mu_beta_raw|tau|mu", Parameter), ]

siteProjections_SVCP[ , Pool := factor(Pool)]
                                            
dat3_SVCP[ , Pool := factor(Pool)]


dataVBplot_SVCP <- ggplot() + 
    geom_point(data = dat3_SVCP, aes(x = Age3, y = TLm), alpha = 0.1) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    geom_line(data = siteProjections_SVCP[  age < 13, ],
              aes(x = age - 1, y = mean), color = 'blue')   + 
    geom_ribbon(data = siteProjections_SVCP[  age < 13, ],
                aes(x = age - 1, ymin = L80, ymax = U80), fill = 'blue',
                alpha = 0.25)  +
    geom_ribbon(data = siteProjections_SVCP[  age < 13, ],
                aes(x = age -1, ymin = L95, ymax = U95), fill = 'blue',
                alpha = 0.25)  +
    facet_wrap( ~ Pool, nrow = 2) +
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 2))
print(dataVBplot_SVCP)

ggsave("dataVBplotNot0_SVCP.pdf", dataVBplot_SVCP, width = 6, height = 6)

hyperPlot_SVCP <-
    ggplot(data = hyperProjection_SVCP[ age < 13, ], aes(x = age - 1, y = mean)) +
    geom_line(color = 'black', size = 1.3) +
    geom_ribbon(aes(x = age -1, ymin = L95, ymax = U95), fill = 'grey50',
                alpha = 0.25) + 
    geom_ribbon(aes(x = age -1, ymin = L80, ymax = U80), fill = 'grey50',
                alpha = 0.25) +
    geom_line(data = siteProjections_SVCP[ age < 13, ],
              aes(x = age -1, y = mean, color = Pool),
              size = 1.1) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 2)) + 
    scale_color_manual( values = c("red", "blue", "seagreen",
                                   "orange", "skyblue", "navyblue", "wheat2")) 
print(hyperPlot_SVCP)
ggsave("hyperPlotNot0_SVCP.pdf", hyperPlot_SVCP, width = 6, height = 4)
ggsave("hyperPlotNot0_SVCP.jpg", hyperPlot_SVCP, width = 6, height = 4)



## Bighead carp analysis 
dat3_BHCP <- dat2[ Species == "BHCP", ]
dat3_BHCP[ , Pool := factor(Pool)]
dat3_BHCP[ , PoolID := as.numeric(Pool)]
## Convert to M to stabilisze results
dat3_BHCP[ , TLm := TL/1000]

ageProjection = seq(0, 20, by = 1)

stanData_BHCP <- list(
    nFish  = dim(dat3_BHCP)[1],
    nSites = length(dat3_BHCP[, unique(PoolID)]),
    length = dat3_BHCP[ , TLm], 
    poolID = dat3_BHCP[ , PoolID],
    age = dat3_BHCP[ , Age3],
    hp_tau = 1.5,
    hp_sigma = 10,
    hp_omega = 2,
    p_mu_gamma = 0,
    p_mu_gammaSD = 2,
    nProject = length(ageProjection),
    ageProject = ageProjection
    )

dat3_BHCP[ , unique(PoolID)]
dat3_BHCP[ , unique(Pool)]

## Model takes ~0.25 hrs to run.
## the stan function is commented out and the outputs loaded from a saved file
## unless it needs to be re-run. 
if(rerunStan){
    stanOutO_BHCP <- stan(file = "vonBoNot0.stan",
                          data = stanData_BHCP, chains = 4, iter = 6000,
                          control = list(adapt_delta = 0.8))
    save(stanOutO_BHCP, file = "vonBfitNot0_BHCP.RData")
} else {
    load("vonBfitNot0_BHCP.RData")                    
}


stanOutOsummary_BHCP <- 
    summary(stanOutO_BHCP, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))

stanOutOsummary_BHCP[[1]][grepl("M", rownames(summary(stanOutO_BHCP)[[1]])), ]

stanOutOsummary_BHCP[[1]][grepl("bar", rownames(summary(stanOutO_BHCP)[[1]])), ]
stanOutOsummary_BHCP[[1]][grepl("K", rownames(summary(stanOutO_BHCP)[[1]])), ]
stanOutOsummary_BHCP[[1]][grepl("Linf", rownames(summary(stanOutO_BHCP)[[1]])),]

stanOutOsummaryDT_BHCP <- data.table(stanOutOsummary_BHCP[[1]])
stanOutOsummaryDT_BHCP[ , Parameter := rownames(stanOutOsummary_BHCP[[1]])]


plot(stanOutO_BHCP, pars =c("Linf_bar", "K_bar", "Linf"))

poolNames_BHCP <- dat3_BHCP[ , levels(Pool)]
poolNames_BHCP
## Plot parameter estimates

## Plot linf
LinfPlot_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("Linf", Parameter), ])
poolNamesDT_BHCP <- data.table(Pool = c(poolNames_BHCP, "across sites"),
                        PoolID = c(1:length(poolNames_BHCP), "Linf_bar"))
setkey(poolNamesDT_BHCP, "PoolID")

LinfPlot_BHCP[ , PoolID := gsub("(Linf)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(LinfPlot_BHCP, "PoolID")

LinfPlot_BHCP <- poolNamesDT_BHCP[ LinfPlot_BHCP]
setnames(LinfPlot_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))

LinfPlot_BHCP[ , Pool := factor(Pool)]
                           ## levels = rev(c("Marseilles",  "Peoria", "LaGrange",
                           ##                "Alton",
                           ##                "Pool 26", "Pool 27", "across sites")))]

LinfPlotGG_BHCP <- ggplot(data = LinfPlot_BHCP, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(L)[infinity]))
print(LinfPlotGG_BHCP)
ggsave("LinfPlotNot0_BHCP.pdf", LinfPlotGG_BHCP, width = 3, height = 4)

## Plot growth rates
KPlot_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("K", Parameter), ])
poolNamesDT_BHCP <- data.table(Pool = c(poolNames_BHCP, "across sites"),
                        PoolID = c(1:length(poolNames_BHCP), "K_bar"))
setkey(poolNamesDT_BHCP, "PoolID")

KPlot_BHCP[ , PoolID := gsub("(K)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(KPlot_BHCP, "PoolID")

KPlot_BHCP <- poolNamesDT_BHCP[ KPlot_BHCP]
setnames(KPlot_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


KPlot_BHCP[ , Pool := factor(Pool)]
                             ## levels = rev(c("Marseilles",  "Peoria", "LaGrange",
                             ##                "Alton",
                             ##                "Pool 26", "Pool 27", "across sites")))]


KPlotGG_BHCP <- ggplot(data = KPlot_BHCP, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(K)))
print(KPlotGG_BHCP)
ggsave("KPlotNot0_BHCP.pdf", KPlotGG_BHCP, width = 3, height = 4)

## Plot natural mortality across sites
MPlot_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("M", Parameter), ])
poolNamesDT_BHCP <- data.table(Pool = c(poolNames_BHCP, "across sites"),
                               PoolID = c(1:length(poolNames_BHCP), "M_bar"))
setkey(poolNamesDT_BHCP, "PoolID")

MPlot_BHCP[ , PoolID := gsub("(M)(\\[)(\\d)(\\])", "\\3", Parameter) ]
setkey(MPlot_BHCP, "PoolID")

MPlot_BHCP <- poolNamesDT_BHCP[ MPlot_BHCP]
setnames(MPlot_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))

MPlot_BHCP[ , Pool := factor(Pool)]
                        ## levels = rev(c("Marseilles",  "Peoria", "LaGrange",
                        ##                "Alton",
                        ##                "Pool 26", "Pool 27", "across sites")))]



MPlotGG_BHCP <- ggplot(data = MPlot_BHCP, aes(x = Pool, y = median)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.4)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 1)  +
    coord_flip() +
    theme_minimal() +
    ylab(expression(italic(M)))
print(MPlotGG_BHCP)
ggsave("MPlotNot0_BHCP.pdf", MPlotGG_BHCP, width = 3, height = 4)


## Extract out hyper projections
hyperProjection_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("hyperProjection", Parameter), ])
hyperProjection_BHCP[ , age := as.numeric(gsub("hyperProjection|\\[|\\]", "", Parameter))]
hyperProjection_BHCP[ , Pool := "Across pools"]

## Extract out site projections 
siteProjections_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("siteProjection", Parameter)])
siteProjections_BHCP[ , PoolID :=  gsub("(siteProjections\\[)(\\d),(\\d{1,2})\\]", "\\2", Parameter)]
siteProjections_BHCP[ , age :=  as.numeric(gsub("(siteProjections\\[)(\\d),(\\d{1,2})\\]", "\\3", Parameter))]

setkey(siteProjections_BHCP, "PoolID")

siteProjections_BHCP<- siteProjections_BHCP[poolNamesDT_BHCP][ Pool!= "across sites",]

siteProjections_BHCP[ , PoolID := NULL]

setnames(siteProjections_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))
setnames(hyperProjection_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


allProjections_BHCP <- rbind(siteProjections_BHCP,
                             hyperProjection_BHCP)

stanOutOsummaryDT_BHCP[ !grepl("siteProjections|hyperProjection|t0|K|Linf|M|Omega|mu_beta_raw|tau|mu", Parameter), ]

siteProjections_BHCP[ , Pool := factor(Pool)]
                                       ## levels = c("Marseilles",
                                       ##            "Peoria", "LaGrange",
                                       ##            "Alton",
                                       ##            "Pool 26", "Pool 27"))]
                                            
dat3_BHCP[ , Pool := factor(Pool)]
                            ## levels = c("Marseilles",
                            ##            "Peoria", "LaGrange",
                            ##            "Alton",
                            ##            "Pool 26", "Pool 27"))]



dataVBplot_BHCP <- ggplot() + 
    geom_point(data = dat3_BHCP, aes(x = Age3 -1, y = TLm), alpha = 0.1) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    geom_line(data = siteProjections_BHCP[  age < 13, ],
              aes(x = age -1, y = mean), color = 'blue')   + 
    geom_ribbon(data = siteProjections_BHCP[  age < 13, ],
                aes(x = age-1, ymin = L80, ymax = U80), fill = 'blue',
                alpha = 0.25)  +
    geom_ribbon(data = siteProjections_BHCP[  age < 13, ],
                aes(x = age-1, ymin = L95, ymax = U95), fill = 'blue',
                alpha = 0.25)  +
    facet_wrap( ~ Pool, nrow = 2) +
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 2))
print(dataVBplot_BHCP)

ggsave("dataVBplotNot0_BHCP.pdf", dataVBplot_BHCP, width = 6, height = 6)

hyperPlot_BHCP <-
    ggplot(data = hyperProjection_BHCP[ age < 13, ],
           aes(x = age-1, y = mean)) +
    geom_line(color = 'black', size = 1.3) +
    geom_ribbon(aes(x = age-1, ymin = L95, ymax = U95), fill = 'grey50',
                alpha = 0.25) + 
    geom_ribbon(aes(x = age-1, ymin = L80, ymax = U80), fill = 'grey50',
                alpha = 0.25) +
    geom_line(data = siteProjections_BHCP[ age < 13, ],
              aes(x = age-1, y = mean, color = Pool),
              size = 1.1) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 2)) + 
    scale_color_manual( values = c("red", "blue", "seagreen",
                                   "orange", "skyblue", "navyblue")) 
print(hyperPlot_BHCP)
ggsave("hyperPlotNot0_BHCP.pdf", hyperPlot_BHCP, width = 6, height = 4)
ggsave("hyperPlotNot0_BHCP.jpg", hyperPlot_BHCP, width = 6, height = 4)
