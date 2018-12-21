library(ggplot2)
library(lubridate)
library(tidyverse)
library(scales)
library(rstan)

## Read in a format data
dat <- read_csv("../DemographicsData.csv")
dat <- dat %>%
    select(Sampdate, Year, Pool, Species, TL, WT) %>%
    mutate(Sampdate =ymd(Sampdate),
           Pool = factor(Pool),
           TLm = TL/1000,
           TLmL10 = log10(TL/1000),
           WTkg = WT/1000)

dat2 <- dat %>%
    filter(!is.na(TL) & !is.na(WT))

dat2 %>%
    group_by(Pool) %>%
    summarize(n())


dat3 <-
    dat2 %>%
    mutate(Species = recode(Species,
                            BHCP = "Bighead carp",
                            SVCP = "Silver carp"))


RiverKeyIn <-
    read_csv("./RiverKey.txt") 

    
## SVCP data format 
SVCP_lw_key <- read_csv("../lengthWeight/SVCP_lw_key.csv")


dat3_SVCP <-
    dat3 %>%
    filter(Species == "Silver carp")

dat3_SVCP %>%
    filter(is.na(TLmL10))

SVCP_pools <-
    dat3_SVCP %>%
    distinct(Pool) %>%
    pull(Pool) %>%
    levels()

RiverKeySVCP <- 
    RiverKeyIn %>%
    filter(Pool %in% SVCP_pools) %>%
    full_join(SVCP_lw_key, by = "Pool")

RiverKeySVCP_pools <-
    RiverKeySVCP %>%
    pull(Pool)

dat3_SVCP <- 
    dat3_SVCP %>% 
    full_join(RiverKeySVCP, by = 'Pool') %>%
    mutate(Pool = factor(Pool, levels = RiverKeySVCP_pools)) %>%
    filter(!is.na(TL))

SVCP_project_range <-
    dat3_SVCP %>%
    pull(TLmL10) %>%
    range()

xProject_SVCP <- seq(SVCP_project_range[1], SVCP_project_range[2], length.out = 100)
xProject_SVCP

## Load in fitted data for SVCP

load("../lengthWeight/lengthWeight3rd_SVCP.RData")

stanOutSummary_SVCP <- summary(stanOut_SVCP,
                               probs = c(0.025, 0.1, 0.50, 0.9, 0.975))

################
## extratc intercepts
rowNames <- rownames(stanOutSummary_SVCP[[1]])

intercepts_SVCP <-
    stanOutSummary_SVCP[[1]] %>%
    as.tibble() %>%
    mutate(RawParameter = rowNames) %>% 
    filter( grepl("(beta|gamma)(\\[\\d+,1\\])",
                  RawParameter)) %>%
    mutate( Parameter = gsub("(gamma|beta)\\[(\\d*),(\\d*)\\]",
                             "\\1", RawParameter)) %>%
    mutate( Pool = gsub("(gamma|beta)\\[(\\d*),(\\d*)\\]",
                        "\\2", RawParameter)) %>%
    mutate( PoolID = ifelse( Parameter == "gamma",
                          'hyper-parameter',
                          Pool) )

intercepts_SVCP %>%
    select(RawParameter, Parameter, Pool, PoolID)

## Read in keys
groupPredictKey_SVCP <- read_csv("../lengthWeight/SVCP_lw_key.csv")
groupPredictKey_SVCP

groupPredictKey_SVCP <- rbind(groupPredictKey_SVCP,
                              tibble(Pool = "Hyper-parameter",
                                     PoolID = 'hyper-parameter'))
groupPredictKey_SVCP


interceptsDT_SVCP <-
    intercepts_SVCP %>%
    select(-n_eff, -Rhat, -sd, -se_mean, -Pool, -Parameter) %>%
    full_join(groupPredictKey_SVCP, by = "PoolID") %>%
    select(-PoolID, - RawParameter) 

colnames(interceptsDT_SVCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")

interceptsDT_SVCP

## extract slopes

## SVCP 
slopes_SVCP <-
    stanOutSummary_SVCP[[1]] %>%
    as.tibble() %>%
    mutate(RawParameter = rowNames) %>% 
    filter( grepl("(beta|gamma)(\\[\\d+,2\\])",
                  RawParameter)) %>%
    mutate( Parameter = gsub("(gamma|beta)\\[(\\d*),(\\d*)\\]",
                             "\\1", RawParameter)) %>%
    mutate( Pool = gsub("(gamma|beta)\\[(\\d*),(\\d*)\\]",
                        "\\2", RawParameter)) %>%
    mutate( PoolID = ifelse( Parameter == "gamma",
                          'hyper-parameter',
                          Pool) )


slopesDT_SVCP <-
    slopes_SVCP %>%
    select(-n_eff, -Rhat, -sd, -se_mean, -Pool, -Parameter) %>%
    full_join(groupPredictKey_SVCP, by = "PoolID") %>%
    select(-PoolID, - RawParameter) 


colnames(slopesDT_SVCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")

slopesDT_SVCP

## format BHCP data
BHCP_lw_key <- read_csv("../lengthWeight/BHCP_lw_key.csv")
    

dat3_BHCP <-
    dat3 %>%
    filter(Species == "Bighead carp")


dat3_BHCP %>%
    filter(is.na(TLmL10))


BHCP_pools <-
    dat3_BHCP %>%
    distinct(Pool) %>%
    pull(Pool) %>%
    levels()


RiverKeyBHCP <- 
    RiverKeyIn %>%
    filter(Pool %in% BHCP_pools) %>%
    full_join(BHCP_lw_key, by = "Pool")


RiverKeyBHCP_pools <-
    RiverKeyBHCP %>%
    pull(Pool)


dat3_BHCP <- 
    dat3_BHCP %>% 
    full_join(RiverKeyBHCP, by = 'Pool') %>%
    mutate(Pool = factor(Pool, levels = RiverKeyBHCP_pools)) %>%
    filter(!is.na(TL))


BHCP_project_range <-
    dat3_BHCP %>%
    pull(TLmL10) %>%
    range()

xProject_BHCP <- seq(BHCP_project_range[1], BHCP_project_range[2], length.out = 100)
xProject_BHCP


## Load in fitted data for BHCP
load("../lengthWeight/lengthWeight3rd_BHCP.RData")

stanOutSummary_BHCP <- summary(stanOut_BHCP,
                               probs = c(0.025, 0.1, 0.50, 0.9, 0.975))

################
## extratc intercepts
rowNames <- rownames(stanOutSummary_BHCP[[1]])

intercepts_BHCP <-
    stanOutSummary_BHCP[[1]] %>%
    as.tibble() %>%
    mutate(RawParameter = rowNames) %>% 
    filter( grepl("(beta|gamma)(\\[\\d+,1\\])",
                  RawParameter)) %>%
    mutate( Parameter = gsub("(gamma|beta)\\[(\\d*),(\\d*)\\]",
                             "\\1", RawParameter)) %>%
    mutate( Pool = gsub("(gamma|beta)\\[(\\d*),(\\d*)\\]",
                        "\\2", RawParameter)) %>%
    mutate( PoolID = ifelse( Parameter == "gamma",
                          'hyper-parameter',
                          Pool) )

intercepts_BHCP %>%
    select(RawParameter, Parameter, Pool, PoolID)

## Read in keys
groupPredictKey_BHCP <- read_csv("../lengthWeight/BHCP_lw_key.csv")
groupPredictKey_BHCP

groupPredictKey_BHCP <- rbind(groupPredictKey_BHCP,
                              tibble(Pool = "Hyper-parameter",
                                     PoolID = 'hyper-parameter'))
groupPredictKey_BHCP


interceptsDT_BHCP <-
    intercepts_BHCP %>%
    select(-n_eff, -Rhat, -sd, -se_mean, -Pool, -Parameter) %>%
    full_join(groupPredictKey_BHCP, by = "PoolID") %>%
    select(-PoolID, - RawParameter) 

colnames(interceptsDT_BHCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")

interceptsDT_BHCP

## extract slopes

## BHCP 
slopes_BHCP <-
    stanOutSummary_BHCP[[1]] %>%
    as.tibble() %>%
    mutate(RawParameter = rowNames) %>% 
    filter( grepl("(beta|gamma)(\\[\\d+,2\\])",
                  RawParameter)) %>%
    mutate( Parameter = gsub("(gamma|beta)\\[(\\d*),(\\d*)\\]",
                             "\\1", RawParameter)) %>%
    mutate( Pool = gsub("(gamma|beta)\\[(\\d*),(\\d*)\\]",
                        "\\2", RawParameter)) %>%
    mutate( PoolID = ifelse( Parameter == "gamma",
                          'hyper-parameter',
                          Pool) )


slopesDT_BHCP <-
    slopes_BHCP %>%
    select(-n_eff, -Rhat, -sd, -se_mean, -Pool, -Parameter) %>%
    full_join(groupPredictKey_BHCP, by = "PoolID") %>%
    select(-PoolID, - RawParameter) 


colnames(slopesDT_BHCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")

slopesDT_BHCP

## Merge together all coefficients 
slopesDT_BHCP <- 
    slopesDT_BHCP %>%
    mutate(Species = "Bighead carp",
           Parameter = "Slope")

interceptsDT_BHCP <- 
    interceptsDT_BHCP %>%
    mutate(Species = "Bighead carp",
           Parameter = "Intercept")

slopesDT_SVCP <- 
    slopesDT_SVCP %>%
    mutate(Species = "Silver carp",
           Parameter = "Slope")

interceptsDT_SVCP <- 
    interceptsDT_SVCP %>%
    mutate(Species = "Silver carp",
           Parameter = "Intercept")


coefAll <- rbind(
    slopesDT_BHCP,
    slopesDT_SVCP,
    interceptsDT_BHCP,
    interceptsDT_SVCP)

lwPools <-
    coefAll %>%
    pull(Pool) %>%
    unique(.)

## Load in River key file here
## this assumes the order in the
## text file is the desired plotting order
RiverKey <- read_csv("RiverKey.txt") %>%
    filter(Pool %in% lwPools)
RiverKey


coefAll <-
    coefAll %>%
    full_join(RiverKey, "Pool") %>%
    select(-x, -y) %>%
    mutate(Pool = factor(Pool, levels = rev(RiverKey$Pool)))

## Plot all
ggCoef <-
    ggplot(coefAll, aes(x = Pool, y = mean, color = River)) +
    geom_point(size = 1.5) +
    geom_linerange( aes(ymin = L95, ymax = U95)) +
    geom_linerange( aes(ymin = L80, ymax = U80), size = 1.2) + 
    coord_flip() +
    facet_grid( Species ~ Parameter, scales = "free") + 
    xlab("Pool") +
    ylab(expression("Coefficient estimate")) +
    theme_minimal() +
    scale_color_manual(values = c("black", "blue", "seagreen", "orange"))
ggCoef

ggsave("LW_intercept.pdf", ggCoef, width = 8, height = 6)



## Extract out predictions SVCP 
siteProjections_SVCP <-
    data.frame(
        stanOutSummary_SVCP[[1]][grepl("yProject",
                                       rownames(summary(stanOut_SVCP)[[1]])), ])
siteProjections_SVCP$parameter <- rownames(siteProjections_SVCP)


siteProjectionsDT_SVCP <-
    as.tibble(siteProjections_SVCP) %>%
    select(-se_mean, -sd, -n_eff, -Rhat) %>%
    mutate(PoolID = gsub("yProject\\[(\\d+),(\\d+)\\]",
                         "\\1", parameter),
           LengthID = as.numeric(
               gsub("yProject\\[(\\d+),(\\d+)\\]",
                    "\\2", parameter))
           )

colnames(siteProjectionsDT_SVCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")


## Merge in length
lengthDT_SVCP <- tibble(LengthID = 1:length(xProject_SVCP),
                        length = xProject_SVCP)

## Mergin in mins and maxs
poolMinMax_SVCP <-
    dat3_SVCP %>%
    group_by( Pool) %>% 
    summarize(poolMin = min( TLmL10, na.rm = TRUE),
              poolMax = max( TLmL10, na.rm = TRUE)
              )

siteProjectionsDT_last_SVCP <-
    siteProjectionsDT_SVCP %>%
    full_join(lengthDT_SVCP, by = "LengthID") %>%
    full_join(groupPredictKey_SVCP, by = "PoolID") %>%
    full_join(poolMinMax_SVCP, by = "Pool") %>%
    mutate(TLm = 10^mean, Species = "Silver carp") %>%
    select( -PoolID, -LengthID) %>%
    filter(!grepl("Hyper", Pool))


## Extract out predictions BHCP 
siteProjections_BHCP <-
    data.frame(
        stanOutSummary_BHCP[[1]][grepl("yProject",
                                       rownames(summary(stanOut_BHCP)[[1]])), ])
siteProjections_BHCP$parameter <- rownames(siteProjections_BHCP)


siteProjectionsDT_BHCP <-
    as.tibble(siteProjections_BHCP) %>%
    select(-se_mean, -sd, -n_eff, -Rhat) %>%
    mutate(PoolID = gsub("yProject\\[(\\d+),(\\d+)\\]",
                         "\\1", parameter),
           LengthID = as.numeric(
               gsub("yProject\\[(\\d+),(\\d+)\\]",
                    "\\2", parameter))
           )

colnames(siteProjectionsDT_BHCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")


## Merge in length
lengthDT_BHCP <- tibble(LengthID = 1:length(xProject_BHCP),
                        length = xProject_BHCP)

## Mergin in mins and maxs
poolMinMax_BHCP <-
    dat3_BHCP %>%
    group_by( Pool) %>% 
    summarize(poolMin = min( TLmL10, na.rm = TRUE),
              poolMax = max( TLmL10, na.rm = TRUE)
              )

siteProjectionsDT_last_BHCP <-
    siteProjectionsDT_BHCP %>%
    full_join(lengthDT_BHCP, by = "LengthID") %>%
    full_join(groupPredictKey_BHCP, by = "PoolID") %>%
    full_join(poolMinMax_BHCP, by = "Pool") %>%
    mutate(TLm = 10^mean, Species = "Bighead carp") %>%
    select( -PoolID, -LengthID) %>%
    filter(!grepl("Hyper", Pool))


## Merge files
siteProjections <-
    siteProjectionsDT_last_BHCP %>%
    bind_rows(siteProjectionsDT_last_SVCP)


## exract out hyper parameter
yHyper_SVCP <-
    data.frame(stanOutSummary_SVCP[[1]][grepl(
                                      "yHyper",
                                      rownames(summary(stanOut_SVCP)[[1]])), ])

yHyper_SVCP$parameter = rownames(yHyper_SVCP)

yHyperDT_SVCP <-
    as.tibble(yHyper_SVCP) %>%
    select(-se_mean, -sd, -n_eff, -Rhat) %>%
    mutate(LengthID = as.numeric(gsub("yHyper\\[(\\d+)\\]",
                                      "\\1", parameter)) ) 


colnames(yHyperDT_SVCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")

yHyperDT_SVCP <-
    yHyperDT_SVCP %>%
    full_join(lengthDT_SVCP, by = "LengthID") %>%
    select(-LengthID) %>%
    mutate(speices = "Silver carp")


yHyper_BHCP <-
    data.frame(stanOutSummary_BHCP[[1]][grepl(
                                      "yHyper",
                                      rownames(summary(stanOut_BHCP)[[1]])), ])

yHyper_BHCP$parameter = rownames(yHyper_BHCP)

yHyperDT_BHCP <-
    as.tibble(yHyper_BHCP) %>%
    select(-se_mean, -sd, -n_eff, -Rhat) %>%
    mutate(LengthID = as.numeric(gsub("yHyper\\[(\\d+)\\]",
                                      "\\1", parameter)) ) 


colnames(yHyperDT_BHCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")

yHyperDT_BHCP <-
    yHyperDT_BHCP %>%
    full_join(lengthDT_BHCP, by = "LengthID") %>%
    select(-LengthID) %>%
    mutate(speices = "Bighead carp")


## Plot results

GGlwData_BHCP <- 
    ggplot() +
    geom_point(data = dat3_BHCP, aes( x = TLm,  y = WTkg)) +
    geom_line(data = siteProjectionsDT_last_BHCP, aes(x = 10^(length), y = 10^(mean)),
              color = 'blue', size = 1.1) +
    facet_wrap( ~ Pool , nrow = 3) + 
    geom_ribbon(data = siteProjectionsDT_last_BHCP,
                aes(x = 10^length,  ymin = 10^L95, ymax = 10^U95), fill = 'blue', alpha = 0.25) +
    geom_ribbon(data = siteProjectionsDT_last_BHCP,
                aes(x = 10^length,  ymin = 10^L80, ymax = 10^U80), fill = 'blue', alpha = 0.25) +    
    ylab("Weight (kg)") +
    xlab("Length (m)") +
    theme_minimal() 
GGlwData_BHCP

ggsave("lengthWeightData_BHCP.pdf", GGlwData_BHCP, width = 5, height = 5)


GGlwData_SVCP <- 
    ggplot() +
    geom_point(data = dat3_SVCP, aes( x = TLm,  y = WTkg)) +
    geom_line(data = siteProjectionsDT_last_SVCP, aes(x = .1 +  10^(length), y = 10^(mean)),
              color = 'blue', size = 1.1) +
    facet_wrap( ~ Pool , nrow = 3) + 
    geom_ribbon(data = siteProjectionsDT_last_SVCP,
                aes(x = .1 + 10^length,  ymin = 10^L95, ymax = 10^U95), fill = 'blue', alpha = 0.25) +
    geom_ribbon(data = siteProjectionsDT_last_SVCP,
                aes(x = .1 + 10^length,  ymin = 10^L80, ymax = 10^U80), fill = 'blue', alpha = 0.25) +    
    ylab("Weight (kg)") +
    xlab("Length (m)") +
    theme_minimal() 
GGlwData_SVCP

ggsave("lengthWeightData_SVCP.pdf", GGlwData_SVCP, width = 5, height = 5)


## Plot hyper

ggHyper_SVCP <-
    ggplot() +
    geom_line(data = siteProjectionsDT_last_SVCP,
              aes(x = 10^length, y = 10^mean, group = Pool), size = 1.1) +
    ylab(expression("Weight (kg)")) +
    xlab(expression("Length (m)")) +
    theme_minimal() +
    geom_ribbon(data = yHyperDT_SVCP,
                aes(x = 10^length, ymin = 10^L95, ymax = 10^U95),
                fill = 'red', alpha = 0.5) +
    geom_line(data = yHyperDT_SVCP,
              aes(x = 10^length, y = 10^mean),
              color = 'red', size = 1)

ggHyper_SVCP
ggsave("lengthWeightHyper_SVCP.pdf", ggHyper_SVCP, width = 6, height = 4) 



###
ggHyper_BHCP <-
    ggplot() +
    geom_line(data = siteProjectionsDT_last_BHCP,
              aes(x = 10^length, y = 10^mean, group = Pool), size = 1.1) +
    ylab(expression("Weight (kg)")) +
    xlab(expression("Length (m)")) +
    theme_minimal() +
    ## scale_color_manual( values = c("red", "blue", "seagreen",
    ##                                "orange", "skyblue", "navyblue")) +
    geom_ribbon(data = yHyperDT_BHCP,
                aes(x = 10^length, ymin = 10^L95, ymax = 10^U95),
                fill = 'red', alpha = 0.5) +
    geom_line(data = yHyperDT_BHCP,
              aes(x = 10^length, y = 10^mean),
              color = 'red', size = 1)

ggHyper_BHCP
ggsave("lengthWeightHyper_BHCP.pdf", ggHyper_BHCP, width = 6, height = 4) 


write_csv(file = "lengthWeightCoefAll.csv", x = coefAll)
