library(tidyverse)
library(lubridate)
library(rstan)

## Load and format raw data
dat <-
    read_csv("../DemographicsData.csv") %>%
    filter(!is.na(Maturity) & !is.na(TL)) %>%
    mutate(Maturity = factor(Maturity),
           TLm = TL / 1000,
           Species = factor(Species)) %>%
    mutate(Species = recode(Species,
                            BHCP = "Bighead carp",
                            SVCP = "Silver carp"),
           M2 = as.numeric(Maturity) - 1)

           
## Exctract and cleanup data

## SVCP Silver carp
load("../maturity/logisticRegressionSVCP.Rda")
summaryOutSVCP <- summary(stanOutSVCP)$summary
parOutSVCP <- data.frame(summary(stanOutSVCP, pars = c("alpha", "beta"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
parOutSVCP$parameter <- rownames(parOutSVCP)
parOutSVCPDT <- as.tibble(parOutSVCP)


colnames(parOutSVCPDT)[4:8] <- c("L95", "L80", "median", "U80", "U95")

summaryOutSVCP <- summary(stanOutSVCP)$summary
summary(summaryOutSVCP[, "Rhat"])


### Predicted distribution
rangeSVCP <-
    dat %>%
    filter(Species == "Silver carp") %>% 
    pull(TLm) %>%
    range(.)

dataInSVCP <- seq(rangeSVCP[1],
                  rangeSVCP[2],
                  by = 0.001)

lenIndexSVCP <-tibble(TLm = dataInSVCP,
                      index = 1:length(dataInSVCP))


predOutSVCP <- data.frame(summary(stanOutSVCP, pars = c("yProject"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
predOutSVCP$parameter <- rownames(predOutSVCP)

predOutSVCPDT <- as.tibble(predOutSVCP)
colnames(predOutSVCPDT)[4:8] <- c("L95", "L80", "median", "U80", "U95")
predOutSVCPDT <- 
    predOutSVCPDT %>%
    mutate(index = as.numeric(gsub("yProject\\[(\\d{1+})\\]", "\\1", parameter))) %>%
    full_join(lenIndexSVCP, by = 'index') %>%
    select(parameter, TLm, mean, L95, L80, median, U80, U95)

predOutSVCPDT 

## BHCP bighead carp
load("../maturity/logisticRegressionBHCP.Rda")
summaryOutBHCP <- summary(stanOutBHCP)$summary
parOutBHCP <- data.frame(summary(stanOutBHCP, pars = c("alpha", "beta"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
parOutBHCP$parameter <- rownames(parOutBHCP)
parOutBHCPDT <- as.tibble(parOutBHCP)


colnames(parOutBHCPDT)[4:8] <- c("L95", "L80", "median", "U80", "U95")

summaryOutBHCP <- summary(stanOutBHCP)$summary
summary(summaryOutBHCP[, "Rhat"])


### Predicted distribution
rangeBHCP <-
    dat %>%
    filter(Species == "Silver carp") %>% 
    pull(TLm) %>%
    range(.)

dataInBHCP <- seq(rangeBHCP[1],
                  rangeBHCP[2],
                  by = 0.001)

lenIndexBHCP <-tibble(TLm = dataInBHCP,
                      index = 1:length(dataInBHCP))


predOutBHCP <- data.frame(summary(stanOutBHCP, pars = c("yProject"), prob = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)
predOutBHCP$parameter <- rownames(predOutBHCP)

predOutBHCPDT <- as.tibble(predOutBHCP)
colnames(predOutBHCPDT)[4:8] <- c("L95", "L80", "median", "U80", "U95")
predOutBHCPDT <- 
    predOutBHCPDT %>%
    mutate(index = as.numeric(gsub("yProject\\[(\\d{1+})\\]", "\\1", parameter))) %>%
    full_join(lenIndexBHCP, by = 'index') %>%
    select(parameter, TLm, mean, L95, L80, median, U80, U95)

predOutBHCPDT 

##########
## Merge and Plot together
parOutSVCPDT <-
    parOutSVCPDT %>%
    mutate(Species = "Silver carp")

parOutBHCPDT <- 
    parOutBHCPDT %>%
    mutate(Species = "Bighead carp")

parOut <- rbind(parOutBHCPDT, parOutSVCPDT)
parOut


## Plot results
parEst <- ggplot(parOut, aes(x = Species, y = mean)) +
    geom_linerange(aes(ymin = L95, ymax = U95)) +
    geom_linerange(aes(ymin = L80, ymax = U80), size = 1.25) +
    geom_point(size = 1.6) +
    coord_flip() +
    facet_grid( . ~ parameter, scales = "fixed",
               labeller = label_parsed) + 
    ylab("Parameter estimate") +
    xlab("Species") +
    theme_minimal()
parEst

ggsave("parEst.pdf", parEst, width = 6, height = 3)

predOutSVCPDT <- 
    predOutSVCPDT %>%
    mutate(Species = "Silver carp")

predOutBHCPDT <- 
    predOutBHCPDT %>%
    mutate(Species = "Bighead carp")

predOut <- rbind(predOutSVCPDT, predOutBHCPDT)

predEst <-
    ggplot(predOut, aes(x = TLm, y = mean)) +
    facet_grid( . ~ Species) +
    geom_ribbon(aes(ymin = L95, ymax = U95), fill = 'blue', alpha = 0.50) + 
    geom_ribbon(aes(ymin = L80, ymax = U80), fill = 'blue', alpha = 0.50) + 
    geom_line(size = 1.6) + 
    ylab("Probability of being mature") +
    xlab("Length (m)") +
    theme_minimal()  +
     geom_jitter(data = dat, aes(x = TLm, y = M2),
                width = 0, height = 0.005)

predEst
ggsave("maturityPred.pdf", predEst, width = 8, height = 4)


## save outputs
write_csv(x = parOut, path = "maturityCoefAll.csv")
