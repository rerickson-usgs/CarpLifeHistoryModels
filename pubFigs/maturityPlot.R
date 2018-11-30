library(ggplot2)
library(lubridate)
library(rstan)
library(data.table)

## Load and format raw data
dat <- fread("../Demographics_080318.csv")
dat[ , Sampdate :=ymd(Sampdate)] 
dat[,  FL := as.numeric(FL)]
dat2 <- dat[ Maturity != "NA",] 
dat2[ , Maturity := factor(Maturity)]
dat2[ , M2 := as.numeric(Maturity) - 1]
dat2[ , TLm := TL / 1000]
dat2[ , WTkg := WT/1000]
dat2[ , GonadWT := as.numeric(GonadWT)]
dat2[ , GonadScaled := GonadWT/max(GonadWT, na.rm = TRUE), by = Species]
dat2[ , Month := month(Sampdate)]
dat2[ , GonadWTkg := GonadWT/1000]
dat2[ , SpeciesFull := factor(Species, levels = c("BHCP", "SVCP"), labels = c("Bighead carp", "Silver carp"))]

## Exctract and cleanup data

## SVCP Silver carp
load("../maturity/logisticRegressionSVCP.Rda")
summaryOutSVCP <- summary(stanOutSVCP)$summary
parOutSVCP <- data.frame(summary(stanOutSVCP, pars = c("alpha", "beta"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
parOutSVCP$parameter <- rownames(parOutSVCP)
parOutSVCPDT <- data.table(parOutSVCP)
setnames(parOutSVCPDT, "X2.5.", "l95")
setnames(parOutSVCPDT, "X97.5.", "u95")
setnames(parOutSVCPDT, "X10.", "l90")
setnames(parOutSVCPDT, "X90.", "u90")


summaryOutSVCP <- summary(stanOutSVCP)$summary
summary(summaryOutSVCP[, "Rhat"])

parOutSVCP <- data.frame(summary(stanOutSVCP, pars = c("alpha", "beta"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
parOutSVCP$parameter <- rownames(parOutSVCP)
parOutSVCPDT <- data.table(parOutSVCP)
setnames(parOutSVCPDT, "X2.5.", "l95")
setnames(parOutSVCPDT, "X97.5.", "u95")
setnames(parOutSVCPDT, "X10.", "l90")
setnames(parOutSVCPDT, "X90.", "u90")
parOutSVCPDT


### Predicted distribution
dataInSVCP <- seq(dat2[ Species == "SVCP", range(TLm)[1]],
                  dat2[ Species == "SVCP", range(TLm)[2]],
                  by = 0.001)


predOutSVCP <- data.frame(summary(stanOutSVCP, pars = c("yProject"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
predOutSVCP$parameter <- rownames(predOutSVCP)
predOutSVCPDT <- data.table(predOutSVCP)
setnames(predOutSVCPDT, "X2.5.", "l95")
setnames(predOutSVCPDT, "X97.5.", "u95")
setnames(predOutSVCPDT, "X10.", "l90")
setnames(predOutSVCPDT, "X90.", "u90")
predOutSVCPDT[ , index := as.numeric(gsub("yProject\\[(\\d{1+})\\]", "\\1", parameter))]
setkey(predOutSVCPDT, "index")
dataInSVCPDT <- data.table(length = dataInSVCP, index = 1:length(dataInSVCP))
setkey(dataInSVCPDT, "index")
predOutSVCPDT <- dataInSVCPDT[predOutSVCPDT]



## BHCP bighead carp
load("../maturity/logisticRegressionBHCP.Rda")
summaryOutBHCP <- summary(stanOutBHCP)$summary
parOutBHCP <- data.frame(summary(stanOutBHCP, pars = c("alpha", "beta"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
parOutBHCP$parameter <- rownames(parOutBHCP)
parOutBHCPDT <- data.table(parOutBHCP)
setnames(parOutBHCPDT, "X2.5.", "l95")
setnames(parOutBHCPDT, "X97.5.", "u95")
setnames(parOutBHCPDT, "X10.", "l90")
setnames(parOutBHCPDT, "X90.", "u90")


summaryOutBHCP <- summary(stanOutBHCP)$summary
summary(summaryOutBHCP[, "Rhat"])

parOutBHCP <- data.frame(summary(stanOutBHCP, pars = c("alpha", "beta"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
parOutBHCP$parameter <- rownames(parOutBHCP)
parOutBHCPDT <- data.table(parOutBHCP)
setnames(parOutBHCPDT, "X2.5.", "l95")
setnames(parOutBHCPDT, "X97.5.", "u95")
setnames(parOutBHCPDT, "X10.", "l90")
setnames(parOutBHCPDT, "X90.", "u90")
parOutBHCPDT


### Predicted distribution
dataInBHCP <- seq(dat2[ Species == "BHCP", range(TLm)[1]],
                  dat2[ Species == "BHCP", range(TLm)[2]],
                  by = 0.001)

predOutBHCP <- data.frame(summary(stanOutBHCP, pars = c("yProject"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
predOutBHCP$parameter <- rownames(predOutBHCP)
predOutBHCPDT <- data.table(predOutBHCP)
setnames(predOutBHCPDT, "X2.5.", "l95")
setnames(predOutBHCPDT, "X97.5.", "u95")
setnames(predOutBHCPDT, "X10.", "l90")
setnames(predOutBHCPDT, "X90.", "u90")

predOutBHCPDT[ , index := as.numeric(gsub("yProject\\[(\\d+)\\]",
                                          "\\1", parameter))]
setkey(predOutBHCPDT, "index")

dataInBHCPDT <- data.table(length = dataInBHCP, index = 1:length(dataInBHCP))
setkey(dataInBHCPDT, "index")
predOutBHCPDT <- dataInBHCPDT[predOutBHCPDT]

##########
## Merge and Plot together
parOutSVCPDT[ , Species := "Silver carp"]
parOutBHCPDT[ , Species := "Bighead carp"]

parOut <- rbind(parOutBHCPDT, parOutSVCPDT)
parOut



## Plot results
parEst <- ggplot(parOut, aes(x = Species, y = mean)) +
    geom_linerange(aes(ymin = l95, ymax = u95)) +
    geom_linerange(aes(ymin = l90, ymax = u90), size = 1.25) +
    geom_point(size = 1.6) +
    coord_flip() +
    facet_grid( . ~ parameter, scales = "free") + 
    ylab("Parameter estimate") +
    xlab("Species") +
    theme_minimal()
parEst

ggsave("parEst.pdf", parEst, width = 6, height = 3)

predOutSVCPDT[ , SpeciesFull := "Silver carp"]
predOutBHCPDT[ , SpeciesFull := "Bighead carp"]

predOut <- rbind(predOutSVCPDT, predOutBHCPDT)

dat2
str(predOut)
predEst <-
    ggplot(predOut, aes(x = length, y = mean)) +
    geom_ribbon(aes(ymin = l95, ymax = u95), fill = 'blue', alpha = 0.50)+ 
    geom_ribbon(aes(ymin = l90, ymax = u90), fill = 'blue', alpha = 0.50) +
    geom_line(size = 1.6) +
    ylab("Probability of being mature") +
    xlab("Length (m)") +
    theme_minimal()  +
    facet_grid( . ~ SpeciesFull) +
    geom_jitter(data = dat2, aes(x = TLm, y = M2),
                width = 0, height = 0.005)

predEst
ggsave("maturityPred.pdf", predEst, width = 8, height = 4)


## save outputs
fwrite(x = parOut, file = "maturityCoefAll.csv")
