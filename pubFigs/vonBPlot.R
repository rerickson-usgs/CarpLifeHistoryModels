library(ggplot2)
library(lubridate)
library(rstan)
library(tidyverse)
library(plyr)
library(data.table)
library(ggrepel)


## Read in demographic data
dat <- fread("../DemographicsData.csv")
dat[ , Sampdate :=ymd(Sampdate)] 
dat[ , Pool := factor(Pool)]

dat2 <- dat[ !is.na(TL) & !is.na(Age), ]
dat2[ , .N, by = Pool]
dat2[ , Age2 := floor(Age)]
dat2[ , Age3 := Age2 + (month(Sampdate)-5)/12]
dat2[ , Pool :=factor(Pool)]
dat2[ , levels(Pool)]
dat2[ , PoolID := as.numeric(Pool)]

ageProjection = seq(0, 20, by = 1)

## Silver carp data
dat3_SVCP <- dat2[ Species == "SVCP", ] 
dat3_SVCP[ , Pool := factor(Pool)]
dat3_SVCP[ , PoolID := as.numeric(Pool)]
dat3_SVCP[ , TLm := TL/1000]

dat[ , levels(Pool)]
dat2[ , levels(Pool)]

load("../vonB/vonBfitNot0_SVCP.RData")

stanOutOsummary_SVCP <-
    summary(stanOutO_SVCP, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutOsummaryDT_SVCP <- data.table(stanOutOsummary_SVCP[[1]])
stanOutOsummaryDT_SVCP[ , Parameter := rownames(stanOutOsummary_SVCP[[1]])]
poolNames_SVCP <- dat3_SVCP[ , levels(Pool)]
poolNames_SVCP

LinfPlot_SVCP <- copy(stanOutOsummaryDT_SVCP[ grepl("Linf", Parameter), ])
poolNamesDT_SVCP <- data.table(Pool = c(poolNames_SVCP, "Hyper-parameter"),
                        PoolID = c(1:length(poolNames_SVCP), "Linf_bar"))
setkey(poolNamesDT_SVCP, "PoolID")

LinfPlot_SVCP[ , PoolID := gsub("(Linf)(\\[)(\\d+)(\\])", "\\3", Parameter) ]
LinfPlot_SVCP
poolNamesDT_SVCP
setkey(LinfPlot_SVCP, "PoolID")

LinfPlot_SVCP <- poolNamesDT_SVCP[ LinfPlot_SVCP]
setnames(LinfPlot_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))

LinfPlot_SVCP[ , Pool := factor(Pool)]


## growth rates
KPlot_SVCP <- copy(stanOutOsummaryDT_SVCP[ grepl("K", Parameter), ])
poolNamesDT_SVCP <- data.table(Pool = c(poolNames_SVCP, "Hyper-parameter"),
                               PoolID = c(1:length(poolNames_SVCP), "K_bar"))

setkey(poolNamesDT_SVCP, "PoolID")

KPlot_SVCP[ , PoolID := gsub("(K)(\\[)(\\d+)(\\])", "\\3", Parameter) ]
setkey(KPlot_SVCP, "PoolID")

KPlot_SVCP <- poolNamesDT_SVCP[ KPlot_SVCP]
setnames(KPlot_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))



## natural mortality across sites
MPlot_SVCP <- copy(stanOutOsummaryDT_SVCP[ grepl("M", Parameter), ])
poolNamesDT_SVCP <- data.table(Pool = c(poolNames_SVCP, "Hyper-parameter"),
                        PoolID = c(1:length(poolNames_SVCP), "M_bar"))
setkey(poolNamesDT_SVCP, "PoolID")

MPlot_SVCP[ , PoolID := gsub("(M)(\\[)(\\d+)(\\])", "\\3", Parameter) ]
setkey(MPlot_SVCP, "PoolID")

MPlot_SVCP <- poolNamesDT_SVCP[ MPlot_SVCP]
setnames(MPlot_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))

MPlot_SVCP[ , Pool := factor(Pool)]



## Extract out hyper projections
hyperProjection_SVCP <-
    copy(stanOutOsummaryDT_SVCP[ grepl("hyperProjection", Parameter), ])
hyperProjection_SVCP[ , age := as.numeric(gsub("hyperProjection|\\[|\\]", "",
                                               Parameter))]
hyperProjection_SVCP[ , Pool := "hyper-parameter"]

## Extract out site projections 
siteProjections_SVCP <- copy(stanOutOsummaryDT_SVCP[ grepl("siteProjection",
                                                           Parameter)])
siteProjections_SVCP[ , PoolID := gsub("(siteProjections\\[)(\\d+),(\\d+)\\]",
                                        "\\2", Parameter)]
siteProjections_SVCP[ , age := as.numeric(gsub("(siteProjections\\[)(\\d+),(\\d+)\\]",
                                                "\\3", Parameter))]

setkey(siteProjections_SVCP, "PoolID")
siteProjections_SVCP<- siteProjections_SVCP[poolNamesDT_SVCP][ Pool!= "Hyper-parameter",]
siteProjections_SVCP[ , PoolID := NULL]

setnames(siteProjections_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))
setnames(hyperProjection_SVCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


allProjections_SVCP <- rbind(siteProjections_SVCP,
                             hyperProjection_SVCP)


## bighead carp data
dat3_BHCP <- dat2[ Species == "BHCP", ] 
dat3_BHCP[ , Pool := factor(Pool)]
dat3_BHCP[ , PoolID := as.numeric(Pool)]
dat3_BHCP[ , TLm := TL/1000]

load("../vonB/vonBfitNot0_BHCP.RData")

stanOutOsummary_BHCP <-
    summary(stanOutO_BHCP, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutOsummaryDT_BHCP <- data.table(stanOutOsummary_BHCP[[1]])
stanOutOsummaryDT_BHCP[ , Parameter := rownames(stanOutOsummary_BHCP[[1]])]
poolNames_BHCP <- dat3_BHCP[ , levels(Pool)]
poolNames_BHCP

LinfPlot_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("Linf", Parameter), ])
poolNamesDT_BHCP <- data.table(Pool = c(poolNames_BHCP, "Hyper-parameter"),
                        PoolID = c(1:length(poolNames_BHCP), "Linf_bar"))
setkey(poolNamesDT_BHCP, "PoolID")

LinfPlot_BHCP[ , PoolID := gsub("(Linf)(\\[)(\\d+)(\\])", "\\3", Parameter) ]
LinfPlot_BHCP
poolNamesDT_BHCP
setkey(LinfPlot_BHCP, "PoolID")

LinfPlot_BHCP <- poolNamesDT_BHCP[ LinfPlot_BHCP]
setnames(LinfPlot_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))

LinfPlot_BHCP[ , Pool := factor(Pool)]


## growth rates
KPlot_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("K", Parameter), ])
poolNamesDT_BHCP <- data.table(Pool = c(poolNames_BHCP, "Hyper-parameter"),
                               PoolID = c(1:length(poolNames_BHCP), "K_bar"))

setkey(poolNamesDT_BHCP, "PoolID")

KPlot_BHCP[ , PoolID := gsub("(K)(\\[)(\\d+)(\\])", "\\3", Parameter) ]
setkey(KPlot_BHCP, "PoolID")

KPlot_BHCP <- poolNamesDT_BHCP[ KPlot_BHCP]
setnames(KPlot_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))



## natural mortality across sites
MPlot_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("M", Parameter), ])
poolNamesDT_BHCP <- data.table(Pool = c(poolNames_BHCP, "Hyper-parameter"),
                        PoolID = c(1:length(poolNames_BHCP), "M_bar"))
setkey(poolNamesDT_BHCP, "PoolID")

MPlot_BHCP[ , PoolID := gsub("(M)(\\[)(\\d+)(\\])", "\\3", Parameter) ]
setkey(MPlot_BHCP, "PoolID")

MPlot_BHCP <- poolNamesDT_BHCP[ MPlot_BHCP]
setnames(MPlot_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))

MPlot_BHCP[ , Pool := factor(Pool)]



## Extract out hyper projections
hyperProjection_BHCP <-
    copy(stanOutOsummaryDT_BHCP[ grepl("hyperProjection", Parameter), ])
hyperProjection_BHCP[ , age := as.numeric(gsub("hyperProjection|\\[|\\]", "",
                                               Parameter))]
hyperProjection_BHCP[ , Pool := "hyper-parameter"]

## Extract out site projections 
siteProjections_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("siteProjection",
                                                           Parameter)])
siteProjections_BHCP[ , PoolID := gsub("(siteProjections\\[)(\\d+),(\\d+)\\]",
                                        "\\2", Parameter)]
siteProjections_BHCP[ , age := as.numeric(gsub("(siteProjections\\[)(\\d+),(\\d+)\\]",
                                                "\\3", Parameter))]

setkey(siteProjections_BHCP, "PoolID")
siteProjections_BHCP<- siteProjections_BHCP[poolNamesDT_BHCP][ Pool!= "Hyper-parameter",]
siteProjections_BHCP[ , PoolID := NULL]

setnames(siteProjections_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))
setnames(hyperProjection_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


allProjections_BHCP <- rbind(siteProjections_BHCP,
                             hyperProjection_BHCP)




## Plot natural mortality across sites
MPlot_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("M", Parameter), ])
poolNamesDT_BHCP <- data.table(Pool = c(poolNames_BHCP, "Hyper-parameter"),
                        PoolID = c(1:length(poolNames_BHCP), "M_bar"))
setkey(poolNamesDT_BHCP, "PoolID")

MPlot_BHCP[ , PoolID := gsub("(M)(\\[)(\\d+)(\\])", "\\3", Parameter) ]
setkey(MPlot_BHCP, "PoolID")

MPlot_BHCP <- poolNamesDT_BHCP[ MPlot_BHCP]
setnames(MPlot_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))

MPlot_BHCP[ , Pool := factor(Pool)]



## Extract out hyper projections
hyperProjection_BHCP <-
    copy(stanOutOsummaryDT_BHCP[ grepl("hyperProjection", Parameter), ])
hyperProjection_BHCP[ , age := as.numeric(gsub("hyperProjection|\\[|\\]", "",
                                               Parameter))]
hyperProjection_BHCP[ , Pool := "hyper-parameter"]

## Extract out site projections 
siteProjections_BHCP <- copy(stanOutOsummaryDT_BHCP[ grepl("siteProjection",
                                                           Parameter)])
siteProjections_BHCP[ , PoolID := gsub("(siteProjections\\[)(\\d+),(\\d+)\\]",
                                        "\\2", Parameter)]
siteProjections_BHCP[ , age := as.numeric(gsub("(siteProjections\\[)(\\d+),(\\d+)\\]",
                                                "\\3", Parameter))]

setkey(siteProjections_BHCP, "PoolID")

siteProjections_BHCP<- siteProjections_BHCP[poolNamesDT_BHCP][ Pool!= "Hyper-parameter",]
siteProjections_BHCP
siteProjections_BHCP[ , PoolID := NULL]

setnames(siteProjections_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))
setnames(hyperProjection_BHCP,
         c("2.5%", "10%", "50%",    "90%", "97.5%"),
         c("L95",  "L80", "median", "U80", "U95"))


allProjections_BHCP <- rbind(siteProjections_BHCP,
                             hyperProjection_BHCP)


## Merge and plot both species
MPlot_SVCP[ , Species := "Silver carp"]
MPlot_BHCP[ , Species := "Bighead carp"]
MPlot <- rbind(MPlot_SVCP, MPlot_BHCP)


LinfPlot_SVCP[ , Species :="Silver carp"]
LinfPlot_BHCP[ , Species :="Bighead carp"]
LinfPlot <- rbind(LinfPlot_SVCP,
                  LinfPlot_BHCP)

KPlot_SVCP[ , Species := "Silver carp"]
KPlot_BHCP[ , Species := "Bighead carp"]

KPlot <- rbind(KPlot_SVCP, KPlot_BHCP)


LinfPlot[ , Parameter := "Linfinty"]
KPlot[ , Parameter := "K"]
MPlot[ , Parameter := "M"]
coefAll <- rbind(LinfPlot,KPlot, MPlot)
coefAll

## Merge in River key here
RiverKey <- fread("RiverKey.txt")
RiverKey[ , Pool := factor(Pool, levels = Pool)]
coefAll[ , levels(Pool)]
RiverKey[ , levels(Pool)]
RiverKey[ which( !levels(Pool) %in% coefAll[ , levels(Pool)]), ]
coefAll[ which( !levels(Pool) %in% RiverKey[ , levels(Pool)]),]
RiverKey <- RiverKey[ which(levels(Pool) %in% coefAll[ , levels(Pool)]), ]
RiverKey[ , Pool := factor(Pool)]
setkey(coefAll, "Pool")
setkey(RiverKey, "Pool")
coefAll <- RiverKey[coefAll]
coefAll[ , Pool := factor(Pool, levels = RiverKey[ , Pool])]
coefAll[ ! complete.cases(coefAll),]



hyperProjection_SVCP[ , Species := "Silver carp"]
hyperProjection_BHCP[ , Species := "Bighead carp"]

hyperProjection <-  rbind(hyperProjection_SVCP,
                          hyperProjection_BHCP)

hyperProjection[ , Pool := factor(Pool)]
allProjections_SVCP[ , Species := "Silver carp"]
allProjections_BHCP[ , Species := "Bighead carp"]
allProjections <- rbind(allProjections_SVCP,
                        allProjections_BHCP)
siteProjections_SVCP[ , Species := "Silver carp"]
siteProjections_BHCP[ , Species := "Bighead carp"]
siteProjections <- rbind(siteProjections_SVCP,
                         siteProjections_BHCP)

datGG <- rbind(dat3_SVCP,
               dat3_BHCP)
datGG[ , Pool := factor(Pool)]

## Plot data
coefAll[, ParameterPlot := factor(Parameter)]
levels(coefAll$ParameterPlot) <-
    c(expression(italic(K)),
      expression(italic(L)[infinity]),
      expression(italic(M)))
levels(coefAll$ParameterPlot)

## Remove maximum value 
coefAllOhio <- coefAll[ grepl("Ohio", River), ]
coefAllNotOhio <- coefAll[ !grepl("Ohio", River), ]

coefAllOhio[ , Pool2 := factor(Pool, levels = rev(levels(Pool)))]
coefAllNotOhio[ , Pool2 := factor(Pool, levels = rev(levels(Pool)))]


coefAllGGOhio <- ggplot(data = coefAllOhio, aes(x = Pool2, y = median,
                                        color = River)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.3)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 0.5)  +
    coord_flip() +
    theme_minimal() +
    facet_grid( Species ~ ParameterPlot,
               labeller = labeller(ParameterPlot = label_parsed),
               scales = "free") + 
    ylab("Estimate") +
    scale_color_manual(values = c("black", "blue", "seagreen", "orange")) 
print(coefAllGGOhio)

ggsave("VB_coefOhio.pdf", coefAllGGOhio, width = 6, height = 4)

##
coefAllGGNotOhio <- ggplot(data = coefAllNotOhio, aes(x = Pool2, y = median,
                                        color = River)) +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.3)  +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 0.5)  +
    coord_flip() +
    theme_minimal() +
    facet_grid( Species ~ ParameterPlot,
               labeller = labeller(ParameterPlot = label_parsed),
               scales = "free") + 
    ylab("Estimate") +
    scale_color_manual(values = c("black", "blue", "seagreen", "orange")) 
print(coefAllGGNotOhio)

ggsave("VB_coefNotOhio.pdf", coefAllGGNotOhio, width = 6, height = 4)


datGG[ , Species := ifelse(Species == "SVCP", "Silver carp", "Bighead carp")]


setkey(datGG, "Pool")
setkey(datGG, "Pool")
datGG <- RiverKey[ River != "Hyper-parameter",][datGG]

setkey(siteProjections, "Pool")
siteProjections <- RiverKey[ River != "Hyper-parameter",][siteProjections]


RiverKey2 <- RiverKey[ Pool  %in% datGG[ ,unique(Pool)], ]



datGG[ , Pool2 := factor(Pool, levels = RiverKey2[ , Pool])]
siteProjections[ , Pool2 := factor(Pool, levels = RiverKey2[ , Pool])]

dataVBplot <- ggplot() + 
    geom_point(data = datGG, aes(x = Age3, y = TLm), alpha = 0.1) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    geom_line(data = siteProjections,
              aes(x = age - 1, y = mean, color = River))   + 
    geom_ribbon(data = siteProjections,
                aes(x = age - 1, ymin = L80, ymax = U80, fill = River),
                alpha = 0.25)  +
    geom_ribbon(data = siteProjections,
                aes(x = age -1, ymin = L95, ymax = U95, fill = River),
                alpha = 0.25)  +
    facet_grid( Species ~ River + Pool2) +
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 5)) +
    scale_color_manual(values = c( "blue", "seagreen", "orange")) +
    scale_fill_manual(values = c( "blue", "seagreen", "orange")) +
    theme(legend.position="none")

print(dataVBplot)
datGG[ , .N, by = .(Pool, Species)]
ggsave("dataVBplotNot0.pdf", dataVBplot, width = 14, height = 8)

hyperProjection_SVCP
hyperProjection

hyperPlot <-
    ggplot(data = hyperProjection, aes(x = age - 1, y = mean)) +
    geom_line(color = 'black', size = 1.3) +
    geom_ribbon(aes(x = age -1, ymin = L95, ymax = U95), fill = 'grey50',
                alpha = 0.25) + 
    geom_ribbon(aes(x = age -1, ymin = L80, ymax = U80), fill = 'grey50',
                alpha = 0.25) +
    geom_line(data = siteProjections,
              aes(x = age -1, y = mean, group = Pool),
              size = 1.1) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 2)) + 
    facet_grid(~ Species)

print(hyperPlot)

ggsave("hyperPlotNot0.pdf", hyperPlot, width = 8, height = 4)
ggsave("hyperPlotNot0.jpg", hyperPlot, width = 6, height = 4)


poolNames <- siteProjections %>%
    filter(age == 21)


poolLabelLocation <- function(
                              poolNames = poolNames){

    minY = min(poolNames$mean)
    maxY = max(poolNames$mean)
    nPoints = length(unique(poolNames$Pool))
    
    poolOrder <- tibble(y  = seq(minY, maxY, length.out = nPoints))
    poolOrder <- poolOrder %>%
        mutate(Order = order(y))
    
    poolNames <-
        poolNames %>%
        mutate(Order = rank(mean))

    poolOrderPlot <- full_join(poolOrder, poolNames, by = 'Order')
    return(poolOrderPlot)
}



poolNamesSC <-
    poolNames %>%
    select(River, Pool, mean, Species, age) %>%
    filter(Species == 'Silver carp')

poolNamesBC <-
    poolNames %>%
    select(River, Pool, mean, Species, age) %>%
    filter(Species == 'Bighead carp')


poolLabelPlot <- rbind(
    poolLabelLocation(poolNames = poolNamesSC),
    poolLabelLocation(poolNames = poolNamesBC)
)

poolLabelPlot

hyperPlot <-
    ggplot(data = hyperProjection, aes(x = age - 1, y = mean)) +
    geom_line(color = 'black', size = 1.3) +
    geom_ribbon(aes(x = age -1, ymin = L95, ymax = U95), fill = 'grey50',
                alpha = 0.25) + 
    geom_ribbon(aes(x = age -1, ymin = L80, ymax = U80), fill = 'grey50',
                alpha = 0.25) +
    geom_line(data = siteProjections,
              aes(x = age -1, y = mean, group = Pool, color = Pool),
              size = 1.1) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 2)) + 
    facet_grid(~ Species)

print(hyperPlot)


hyperPlotWithLabel <-hyperPlot +
    geom_text(data = poolLabelPlot, aes(x = age, y = y, label = Pool, color = Pool),
              hjust = "outward") + 
    xlim(c(0, 30))   +
    theme(legend.position="none") +
    scale_color_hue(l=58)

print(hyperPlotWithLabel)
ggsave("hyperPlotNot0.pdf", hyperPlotWithLabel, width = 8, height = 6)
ggsave("hyperPlotNot0.jpg", hyperPlotWithLabel, width = 8, height = 6)


## export data for simulations
head(coefAll)

coefAll[ order(abs(mean - median)), .(Pool, River, Parameter, mean, median)]

head(coefAll)

ggplot(coefAll[ Species == "Bighead carp", ], aes(x = Pool, y = mean)) +
    geom_linerange(aes(ymin = L95, ymax = U95), size = 2)+ 
    geom_linerange(aes(ymin = mean + qnorm(0.025) * sd , ymax = mean + qnorm(0.975) * sd), size = 1, color = 'red')+ 
    geom_point(size = 3) +
    facet_grid(Species ~ Parameter, scale = "free_y") + 
    coord_flip()
                
fwrite(coefAll, "coefAll_vonB.csv")

## Plot coefficents and data for MS.
RiverKey <- fread("RiverKey.txt")
RiverKey
RiverKey <- RiverKey[ River != "Hyper-parameter"]
setkey(RiverKey, "Pool")
setkey(coefAll, "Pool")

coefRiver <- RiverKey[coefAll]
colnames(coefRiver)

coefRiver2 <- coefRiver[ !is.na(River), ]

vonBLat <- ggplot(coefRiver2, aes(x = mean, y = y)) +
    geom_point(aes(x = mean, y = y, shape = River)) +
    facet_grid( Species ~ ParameterPlot,
               labeller = labeller(ParameterPlot = label_parsed),
               scales = "free_x")  +
    ylab("Latitude") +
    xlab("Parameter estimate") +
    geom_smooth(method = 'lm', se =FALSE) +
    theme_bw() +
    theme(strip.background = element_blank()) 
print(vonBLat)
ggsave("vonBLat.pdf", width = 6, height = 4)

colnames(coefRiver)

coefRiverWide <- coefRiver %>%
    select(River, Pool, x, y, mean, Parameter, Species) %>%
    spread(Parameter, mean)

library(GGally)

ggPairsData <- coefRiverWide %>%
    select(Linfinty, K, M)

pairsPlot <- ggpairs(ggPairsData) +
    theme_bw() +
    theme(strip.background = element_blank()) 
pairsPlot
ggsave("pairsPlot.pdf", width = 6, height = 6)

coefRiverWide
ggplot(coefRiverWide, aes(x = K, y = Linfinty)) +
    geom_point() +
    facet_grid( . ~ Species) 

## Plot data
RiverKey <- fread("RiverKey.txt")
RiverKey[ , Pool := factor(Pool, levels = Pool)]

setkey(RiverKey, "Pool")

setkey(dat, "Pool")
dat2 <- RiverKey[dat]
dat2[ , Pool := factor(Pool, levels = rev(RiverKey[ , Pool]))]
dat2[ , Age2 := floor(Age)]
dat2[ , Age3 := Age2 + (month(Sampdate)-5)/12]

dat3 <- dat2[Species %in% c("BHCP", "SVCP")]

dat3[ , SpeciesPlot := factor(Species,
                              levels = c("BHCP", "SVCP"),
                              labels = c("Bighead carp",
                                         "Silver carp"))]
dat3[ , .(Species, SpeciesPlot)]
                                                  
lengthPlot <- ggplot(dat3, aes(x = Pool, y = TL/1000)) +
    geom_violin(draw_quantiles = .5) +
    geom_point(size = 0.4) + 
    facet_grid( River ~ SpeciesPlot, scales = 'free_y') +
    coord_flip() +
    theme_bw() +
    theme(strip.background = element_blank()) + 
    ylab("Length (m)") 
print(lengthPlot)
ggsave("lengthPlot.pdf", width = 6, height = 6)


datAge <- dat3[ !is.na(Age3), ]
datAge[ Pool =="Pool 22",]
agePlot <- ggplot(datAge, aes(x = Pool, y = Age3)) +
    geom_violin(draw_quantiles = .5) +
    geom_point(size = 0.4) + 
    facet_grid( River ~ SpeciesPlot, scales = 'free_y') +
    coord_flip() +
    theme_bw() +
    theme(strip.background = element_blank()) + 
    ylab("Age (years)") 
print(agePlot)

ggsave("AgePlot.pdf", width = 6, height = 6)

