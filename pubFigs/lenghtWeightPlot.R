library(ggplot2)
library(lubridate)
library(rstan)
library(tidyverse)
library(data.table)
library(scales)

## Read in a format data
dat <- fread("../DemographicsData.csv")
dat[ , Sampdate :=ymd(Sampdate)] 
dat[ , unique(Species)]
dat[ Pool == "OR(pool 27)", Pool := "Pool 27"]
dat[ Pool == "Dresden", Pool := "Dresden Island"]
dat[ , Pool := factor(Pool)]


dat2 <- dat[ !is.na(TL) & !is.na(WT), ]
dat2[ , Pool := factor(Pool)]

dat2[ , TLm  := TL/1000]
dat2[ , WTkg := WT/1000]
dat2[ , TLmL10 := log10(TLm)]
dat2[ , WTkgL10 := log10(WTkg)]


## SVCP data format 
dat3_SVCP<- dat2[ Species == "SVCP", ]
dat3_SVCP[ , Pool := factor(Pool)]
dat3_SVCP[ , PoolID := as.numeric(Pool)]


groupPredictKey_SVCP <-
    dat3_SVCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey_SVCP


xProject_SVCP <- seq(dat2[ , range(TLmL10)][1], dat2[ , range(TLmL10)][2], length.out = 100)

xProject_SVCP

## Load in fittied data
load("../lengthWeight/lengthWeight3rd_SVCP.RData")
stanOutsummary_SVCP <- summary(stanOut_SVCP,
                               probs = c(0.025, 0.1, 0.50, 0.9, 0.975))

################
## extratc intercepts
intercepts_SVCP <- data.frame(stanOutsummary_SVCP[[1]][
    grepl("(beta|gamma)(\\[\\d+,1\\])",
          rownames(summary(stanOut_SVCP)[[1]])), ])
intercepts_SVCP$ID = gsub("\\,1]|\\[", "", rownames(intercepts_SVCP))

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

## BHCP data format 
dat3_BHCP<- dat2[ Species == "BHCP", ]
dat3_BHCP[ , Pool := factor(Pool)]
dat3_BHCP[ , PoolID := as.numeric(Pool)]

groupPredictKey_BHCP <-
    dat3_BHCP[ , .(PoolID =mean(PoolID)), by = Pool][ order(PoolID),]
groupPredictKey_BHCP

xProject_BHCP <-  xProject_SVCP

## Load in fittied data
load("../lengthWeight/lengthWeight3rd_BHCP.RData")
stanOutsummary_BHCP <- summary(stanOut_BHCP,
                               probs = c(0.025, 0.1, 0.50, 0.9, 0.975))

################
## extratc intercepts
intercepts_BHCP <- data.frame(stanOutsummary_BHCP[[1]][
    grepl("(beta|gamma)(\\[\\d+,1\\])",
          rownames(summary(stanOut_BHCP)[[1]])), ])
intercepts_BHCP$ID = gsub("\\,1]|\\[", "", rownames(intercepts_BHCP))

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

## extract slopes

## SVCP 
slopes_SVCP <-
    data.frame(stanOutsummary_SVCP[[1]][grepl("(beta|gamma)(\\[\\d+,2\\])",
                                              rownames(summary(stanOut_SVCP)[[1]])),
                                        ])
slopes_SVCP$ID = gsub("\\,2]|\\[", "", rownames(slopes_SVCP))

slopesDT_SVCP <- data.table(slopes_SVCP)
setkey(slopesDT_SVCP, "ID")
setkey(groupPredictKey_SVCP, "PoolID")
slopesDT_SVCP <- slopesDT_SVCP[groupPredictKey_SVCP]

setnames(slopesDT_SVCP, "X2.5.",  "l95")
setnames(slopesDT_SVCP, "X97.5.", "u95")

setnames(slopesDT_SVCP, "X10.",  "l80")
setnames(slopesDT_SVCP, "X90.",  "u80")

## BHCP

slopes_BHCP <-
    data.frame(stanOutsummary_BHCP[[1]][grepl("(beta|gamma)(\\[\\d+,2\\])",
                                              rownames(summary(stanOut_BHCP)[[1]])),
                                        ])
slopes_BHCP$ID = gsub("\\,2]|\\[", "", rownames(slopes_BHCP))

slopesDT_BHCP <- data.table(slopes_BHCP)
setkey(slopesDT_BHCP, "ID")
setkey(groupPredictKey_BHCP, "PoolID")
slopesDT_BHCP <- slopesDT_BHCP[groupPredictKey_BHCP]
setnames(slopesDT_BHCP, "X2.5.",  "l95")
setnames(slopesDT_BHCP, "X97.5.", "u95")

setnames(slopesDT_BHCP, "X10.",  "l80")
setnames(slopesDT_BHCP, "X90.",  "u80")

## Merge together
interceptsDT_SVCP[ , SpeciesFull := "Silver carp"]
interceptsDT_BHCP[ , SpeciesFull := "Bighead carp"]


interceptsDT <- rbind(interceptsDT_BHCP, interceptsDT_SVCP)
interceptsDT[ , parameter := "Intercept"]
interceptsDT


slopesDT_SVCP[ , SpeciesFull := "Silver carp"]
slopesDT_BHCP[ , SpeciesFull := "Bighead carp"]

slopesDT <-
    rbind(slopesDT_SVCP,
          slopesDT_BHCP)
slopesDT[ , parameter := "Slope"]

coefAll <- rbind(slopesDT, interceptsDT)
coefAll <- coefAll[ complete.cases(coefAll),]
coefAll[ , Pool := factor(Pool)]
coefAll[ , levels(Pool)]

## Load in River key file here
## this assumes the order in the
## text file is the desired plotting order
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

coefAll[ , Pool := factor(Pool, levels = rev(RiverKey[ , Pool]))]
coefAll[ ! complete.cases(coefAll),]

## Plot all
ggCoef <-
    ggplot(coefAll, aes(x = Pool, y = mean, color = River)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = l95, ymax = u95)) +
    geom_linerange( aes(ymin = l80, ymax = u80), size = 1.2) + 
    coord_flip() +
    facet_grid( SpeciesFull~ parameter, scales = "free") + 
    xlab("Pool") +
    ylab(expression("Coefficient estimate")) +
    theme_minimal() +
    scale_color_manual(values = c("black", "blue", "seagreen", "orange"))
ggCoef
ggsave("LW_intercept.pdf", ggCoef, width = 8, height = 6)



## Extract out predictions 
## SVCP 

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

dat3_SVCP[ , .(TLm, 10^TLmL10)]


GGlwData_SVCP <-
    ggplot() +
    geom_point(data = dat3_SVCP, aes( x = TLm,  y = WTkg)) +
    geom_line(data = siteProjectionsDT_SVCP, aes(x = 10^(length), y = 10^(mean)),
              color = 'blue', size = 1.1) +
    facet_wrap( ~ Pool , nrow = 3)  +
    geom_ribbon(data = siteProjectionsDT_SVCP,
                aes(x = 10^length,  ymin = 10^l95, ymax = 10^u95), fill = 'blue', alpha = 0.25) +
    geom_ribbon(data = siteProjectionsDT_SVCP,
                aes(x = 10^length,  ymin = 10^l80, ymax = 10^u80), fill = 'blue', alpha = 0.25) +
    ylab("Weight (kg)") +
    xlab("Length (m)") +
    theme_minimal() 
GGlwData_SVCP

ggsave("lengthWeightData_SVCP.pdf", GGlwData_SVCP, width = 12, height = 12)

## exract out hyper parameter
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
lengthDT_SVCP <- data.table(lengthID = 1:length(xProject_SVCP),
                            length = xProject_SVCP)
setkey(lengthDT_SVCP, "lengthID")
setkey(yHyperDT_SVCP, "lengthID")
yHyperDT_SVCP <-
    yHyperDT_SVCP[lengthDT_SVCP]



## Mere and plot both hypers 
siteProjectionsDT_SVCP[ , SpeciesFull := "Silver carp"]
yHyperDT_SVCP[ , SpeciesFull := "Silver carp"]



## BHCP 
siteProjections_BHCP <-
    data.frame(
        stanOutsummary_BHCP[[1]][grepl("yProject",
                                       rownames(summary(stanOut_BHCP)[[1]])), ])
siteProjections_BHCP$parameter <- rownames(siteProjections_BHCP)
siteProjectionsDT_BHCP <- data.table(siteProjections_BHCP)
siteProjectionsDT_BHCP[ ,
                       PoolID := gsub("yProject\\[(\\d+),(\\d+)\\]",
                                      "\\1", parameter)]
siteProjectionsDT_BHCP[ ,
                       lengthID :=
                           as.numeric(
                               gsub("yProject\\[(\\d+),(\\d+)\\]",
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

lengthDT_BHCP
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


GGlwData_BHCP <- 
    ggplot() +
    geom_point(data = dat3_BHCP, aes( x = TLm,  y = WTkg)) +
    geom_line(data = siteProjectionsDT_BHCP, aes(x = 10^(length), y = 10^(mean)),
              color = 'blue', size = 1.1) +
    facet_wrap( ~ Pool , nrow = 3)  +
    geom_ribbon(data = siteProjectionsDT_BHCP,
                aes(x = 10^length,  ymin = 10^l95, ymax = 10^u95), fill = 'blue', alpha = 0.25) +
    geom_ribbon(data = siteProjectionsDT_BHCP,
                aes(x = 10^length,  ymin = 10^l80, ymax = 10^u80), fill = 'blue', alpha = 0.25) +    
    ylab("Weight (kg)") +
    xlab("Length (m)") +
    theme_minimal() 
GGlwData_BHCP

ggsave("lengthWeightData_BHCP.pdf", GGlwData_BHCP, width = 12, height = 12)

## exract out hyper parameter
yHyper_BHCP <-
    data.frame(stanOutsummary_BHCP[[1]][grepl(
                                      "yHyper",
                                      rownames(summary(stanOut_BHCP)[[1]])), ])
yHyper_BHCP$parameter = rownames(yHyper_BHCP)
yHyperDT_BHCP <- data.table(yHyper_BHCP)
yHyperDT_BHCP[ ,
              lengthID := as.numeric(gsub("yHyper\\[(\\d+)\\]",
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



## Mere and plot both hypers 
siteProjectionsDT_BHCP[ , SpeciesFull := "Bighead carp"]
yHyperDT_BHCP[ , SpeciesFull := "Bighead carp"]


siteProjectionsDT_SVCP[ , SpeciesFull := "Silver carp"]
yHyperDT_SVCP[ , SpeciesFull := "Silver carp"]


ggHyper_SVCP <-
    ggplot() +
    geom_line(data = siteProjectionsDT_SVCP,
              aes(x = 10^length, y = 10^mean, group = Pool), size = 1.1) +
    ylab(expression("Weight (kg)")) +
    xlab(expression("Length (m)")) +
    theme_minimal() +
    geom_ribbon(data = yHyperDT_SVCP,
                aes(x = 10^length, ymin = 10^l95, ymax = 10^u95),
                fill = 'red', alpha = 0.5) +
    geom_line(data = yHyperDT_SVCP,
              aes(x = 10^length, y = 10^mean),
              color = 'red', size = 1)

ggHyper_SVCP
ggsave("lengthWeightHyper_SVCP.pdf", ggHyper_SVCP, width = 6, height = 4) 

###
ggHyper_BHCP <-
    ggplot() +
    geom_line(data = siteProjectionsDT_BHCP,
              aes(x = 10^length, y = 10^mean, group = Pool), size = 1.1) +
    ylab(expression("Weight (kg)")) +
    xlab(expression("Length (m)")) +
    theme_minimal() +
    ## scale_color_manual( values = c("red", "blue", "seagreen",
    ##                                "orange", "skyblue", "navyblue")) +
    geom_ribbon(data = yHyperDT_BHCP,
                aes(x = 10^length, ymin = 10^l95, ymax = 10^u95),
                fill = 'red', alpha = 0.5) +
    geom_line(data = yHyperDT_BHCP,
              aes(x = 10^length, y = 10^mean),
              color = 'red', size = 1)

ggHyper_BHCP
ggsave("lengthWeightHyper_BHCP.pdf", ggHyper_BHCP, width = 6, height = 4) 


fwrite(file = "lengthWeightCoefAll.csv", x = coefAll)
