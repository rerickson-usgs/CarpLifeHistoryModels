## Load required packages 
library(ggplot2)
library(lubridate)
library(rstan)
library(tidyverse)


## Read in demographic data
dat <- read_csv("../DemographicsData.csv")  %>%
    select(Sampdate, Year, Pool, Species, TL, Age) %>%
    mutate(Sampdate =ymd(Sampdate),
           Pool := factor(Pool),
           Age2 = floor(Age),
           TLm = TL/1000,
           Age3 = Age + (month(Sampdate)-5)/1)

dat2 <- dat %>%
    filter(!is.na(TL) & !is.na(Age))

dat2 %>%
    group_by(Pool) %>%
    summarize(n())


ageProjection = seq(0, 20, by = 1)

## Silver carp data
dat3_SVCP <- dat2 %>%
    filter(Species == "SVCP")

load("../vonB/vonBfitNot0_SVCP.RData")

## Create svcp pool lookup table
PoolKeyTable_SVCP <- read_csv("../vonB/SVCP_vonBkey.csv")
PoolKeyTable_SVCP <- rbind(PoolKeyTable_SVCP,
                           tibble(Pool = c("Linf_bar", "K_bar", "M_bar"),
                                  PoolID = c("Linf_bar", "K_bar", "M_bar")))
PoolKeyTable_SVCP <-
    PoolKeyTable_SVCP %>%
    mutate(Pool := factor(Pool))
    
RiverKey <- read_csv("./RiverKey.txt") %>%
    mutate( Pool := factor(Pool, levels = Pool))

PoolKeyTable_SVCP <-
    RiverKey %>%
    full_join(PoolKeyTable_SVCP) %>%
    filter( !is.na(PoolID))  %>%
    mutate(River = ifelse(is.na(River), PoolID, River))

PoolKeyTable_SVCP %>% print(n = Inf)

## Calculate posterior probs 
stanOutOsummary_SVCP <-
    summary(stanOutO_SVCP, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutOsummaryDT_SVCP <- as.tibble(stanOutOsummary_SVCP[[1]])
stanOutOsummaryDT_SVCP <-
    stanOutOsummaryDT_SVCP %>%
    mutate(Parameter = rownames(stanOutOsummary_SVCP[[1]]))

## Grab Linf, M, and K, hyperProjeciton and siteProjection
SVCP_coef <- stanOutOsummaryDT_SVCP %>%
    filter( grepl("K|M|Linf", Parameter)) %>%
    mutate( PoolID = gsub("(Linf|K|M)(\\[)(\\d+)(\\])", "\\3", Parameter)) %>%
    select(-se_mean, -sd, - n_eff, -Rhat)

SVCP_coef %>% print(n = Inf)
SVCP_coef <- SVCP_coef %>% full_join(PoolKeyTable_SVCP, by = "PoolID") 
SVCP_coef



## BHCP data
dat3_BHCP <- dat2 %>%
    filter(Species == "BHCP")

load("../vonB/vonBfitNot0_BHCP.RData")

## Create bhcp pool lookup table
PoolKeyTable_BHCP <- read_csv("../vonB/BHCP_vonBkey.csv")
PoolKeyTable_BHCP <- rbind(PoolKeyTable_BHCP,
                           tibble(Pool = c("Linf_bar", "K_bar", "M_bar"),
                                  PoolID = c("Linf_bar", "K_bar", "M_bar")))
PoolKeyTable_BHCP <-
    PoolKeyTable_BHCP %>%
    mutate(Pool := factor(Pool))
    
RiverKey <- read_csv("RiverKey.txt") %>%
    mutate( Pool := factor(Pool, levels = Pool))


PoolKeyTable_BHCP <-
    RiverKey %>%
    full_join(PoolKeyTable_BHCP) %>%
    filter( !is.na(PoolID))  %>%
    mutate(River = ifelse(is.na(River), PoolID, River))

PoolKeyTable_BHCP %>% print(n = Inf)

## Calculate posterior probs 
stanOutOsummary_BHCP <-
    summary(stanOutO_BHCP, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutOsummaryDT_BHCP <- as.tibble(stanOutOsummary_BHCP[[1]])
stanOutOsummaryDT_BHCP <-
    stanOutOsummaryDT_BHCP %>%
    mutate(Parameter = rownames(stanOutOsummary_BHCP[[1]]))

## Grab Linf, M, and K, hyperProjeciton and siteProjection
BHCP_coef <- stanOutOsummaryDT_BHCP %>%
    filter( grepl("K|M|Linf", Parameter)) %>%
    mutate( PoolID = gsub("(Linf|K|M)(\\[)(\\d+)(\\])", "\\3", Parameter)) %>%
    select(-se_mean, -sd, - n_eff, -Rhat)

BHCP_coef %>% print(n = Inf)
BHCP_coef <- BHCP_coef %>% full_join(PoolKeyTable_BHCP, by = "PoolID") 
BHCP_coef

colnames(BHCP_coef)
colnames(SVCP_coef)

## Merge together coefficients
BHCP_coef <- BHCP_coef %>%
    mutate(Species = "Bighead carp")
SVCP_coef <- SVCP_coef %>%
    mutate(Species = "Silver carp")


coefAll <- rbind(BHCP_coef, SVCP_coef)  %>% select( -"x", -"y")


coefAll <- coefAll %>%
    mutate(Parameter = gsub("(Linf|K|M)(\\[)(\\d+)(\\])", "\\1", Parameter))
coefAll <- coefAll %>%
    mutate( Parameter = gsub("_bar", "", Parameter)) %>%
    mutate( ParameterPlot =  factor(Parameter,
                                    levels = c("Linf", "K", "M"),
                                    labels = c(expression(italic(K)),
                                               expression(italic(L)[infinity]),
                                               expression(italic(M)))))
colnames(coefAll)[2:6] <- c("L95", "L80", "median", "U80", "U95")

## Save coef estimates
write_csv(coefAll, "coefAll_vonB.csv")

## Reorder factors for plotting 
RiverKey <- read_csv("RiverKey.txt") %>%
    filter(Pool != "Hyper-parameter") %>%
    select(-"x", -"y")

RiverKey <- rbind(RiverKey,
                  tibble(River = rep("Hyper-parameter", 3),
                         Pool = c("Linf_bar", "K_bar", "M_bar"))) %>%
    filter(  Pool %in%  unique(coefAll$Pool))


RiverKey %>% print(n = Inf)    

coefAll <- coefAll %>%
    mutate(Pool = factor(Pool, levels = rev(RiverKey$Pool)),
           River = ifelse(grepl("K|Linf|M", Pool), "Hyper-parameter", River)) %>%
    mutate( River = ifelse(grepl("Mc", Pool), "Ohio River", River)) %>%
    mutate( River = ifelse(grepl("Mar", Pool), "Illinois River", River))

levels(coefAll$Pool)

coefAll <-
    coefAll %>%
    mutate(River = ifelse(grepl("JT Myers", Pool), "Ohio River", River),
           Pool = recode(Pool,
                         M_bar = "Hyper-parameter",
                         Linf_bar = "Hyper-parameter",
                         K_bar = "Hyper-parameter")) %>%
    mutate(River = ifelse(grepl("Markland", Pool), "Ohio River", River))


## reorder for plotting
coefAllGG <-
    ggplot(data = coefAll, aes(x = Pool, y = median,
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
print(coefAllGG)




##############################################################
##############################################################
##############################################################
## Now, create site and hyper-parameter table 

## SVCP 1st 
## Create lookup tables for hyper and site projections

## Step 1, order dat3 based upon pool key table 
PoolOrder <- 
    PoolKeyTable_SVCP %>%
    filter(!grepl("_bar", Pool)) %>%
    pull(Pool)
PoolOrder

dat3_SVCP <-
    dat3_SVCP %>%
    mutate(Pool2 = factor(Pool, levels = PoolOrder))

## Step 2, Extract out hyper projection and site Projections 
## Extract out hyperProjections and siteProjections and get age and PoolID
hyperAndSiteProjection_SVCP <-
    stanOutOsummaryDT_SVCP %>%
    select("mean", "2.5%", "10%", "50%", "90%", "97.5%", "Parameter") %>%
    filter( grepl("hyperProjection|siteProjections", Parameter)) %>%
    mutate( age = as.numeric(if_else(
                grepl("hyperProjection", Parameter),
                gsub("hyperProjection||\\[|\\]", "", Parameter),
                gsub("(siteProjections\\[)(\\d+),(\\d+)\\]", "\\3", Parameter))),
           PoolID = if_else(
               grepl("hyperProjection", Parameter),
               "Hyper-parameter", 
               gsub("(siteProjections\\[)(\\d+),(\\d+)\\]", "\\2", Parameter))
           )

    

colnames(hyperAndSiteProjection_SVCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")
colnames(hyperAndSiteProjection_SVCP)

hyperAndSiteProjection_SVCP %>%
    filter( grepl("hyperProjection", Parameter)) %>%
    print(n = Inf)

hyperAndSiteProjection_SVCP %>%
    filter( !grepl("hyperProjection", Parameter)) %>%
    select(Parameter, age, PoolID)

tibble_HP <- tibble(
    River = "Hyper-parameter",
    Pool =  "Hyper-parameter",
    x = NA, y = NA,
    PoolID = "Hyper-parameter")

PoolKeyTable_SVCP_2 <-
    PoolKeyTable_SVCP %>%
    filter( !grepl( "_bar", PoolID)) %>%
    bind_rows(tibble_HP)

hyperAndSiteProjection_SVCP <- 
hyperAndSiteProjection_SVCP %>%
    full_join(PoolKeyTable_SVCP_2, by = "PoolID")

## Repeat for BHCP 
hyperAndSiteProjection_BHCP <-
    stanOutOsummaryDT_BHCP %>%
    select("mean", "2.5%", "10%", "50%", "90%", "97.5%", "Parameter") %>%
    filter( grepl("hyperProjection|siteProjections", Parameter)) %>%
    mutate( age = as.numeric(if_else(
                grepl("hyperProjection", Parameter),
                gsub("hyperProjection||\\[|\\]", "", Parameter),
                gsub("(siteProjections\\[)(\\d+),(\\d+)\\]", "\\3", Parameter))),
           PoolID = if_else(
               grepl("hyperProjection", Parameter),
               "Hyper-parameter", 
               gsub("(siteProjections\\[)(\\d+),(\\d+)\\]", "\\2", Parameter))
           )

    

colnames(hyperAndSiteProjection_BHCP)[2:6] <- c("L95", "L80", "median", "U80", "U95")
colnames(hyperAndSiteProjection_BHCP)

hyperAndSiteProjection_BHCP %>%
    filter( grepl("hyperProjection", Parameter)) %>%
    print(n = Inf)

hyperAndSiteProjection_BHCP %>%
    filter( !grepl("hyperProjection", Parameter)) %>%
    select(Parameter, age, PoolID)

tibble_HP <- tibble(
    River = "Hyper-parameter",
    Pool =  "Hyper-parameter",
    x = NA, y = NA,
    PoolID = "Hyper-parameter")

PoolKeyTable_BHCP_2 <-
    PoolKeyTable_BHCP %>%
    filter( !grepl( "_bar", PoolID)) %>%
    bind_rows(tibble_HP)

hyperAndSiteProjection_BHCP <- 
hyperAndSiteProjection_BHCP %>%
    full_join(PoolKeyTable_BHCP_2, by = "PoolID")

hyperAndSiteProjection_BHCP

## Merge together data sets
hyperAndSiteProjection_BHCP <-
    hyperAndSiteProjection_BHCP %>%
    mutate(Species = "Bighead carp")

hyperAndSiteProjection_SVCP <-
    hyperAndSiteProjection_SVCP %>%
    mutate(Species = "Silver carp")



hyperAndSiteProjection <- 
    hyperAndSiteProjection_BHCP %>%
    bind_rows( hyperAndSiteProjection_SVCP)

## Reorder dat so both can be

## PoolOrder <- 
##     PoolKeyTable_SVCP %>%
##     filter(!grepl("_bar", Pool)) %>%
##     pull(Pool)



## Get dat2 Pools
dat2_pools <- dat2 %>% pull(Pool) %>% unique()
dat2_pools

## Merge two 
RiverKey_use <-
    read_csv("RiverKey.txt") %>%
    filter(!grepl("Hyper-parameter", River)) %>%
    filter(Pool %in% dat2_pools)

dat3 <-
    dat2 %>%
    mutate(Pool = as.character(Pool)) %>%
    full_join(RiverKey_use, by = "Pool") %>% 
    filter(!is.na(Age3)) %>%
    mutate(Species = recode(Species,
                            SVCP = "Silver carp",
                            BHCP = "Bighead carp"),
           Pool_plot = factor(Pool, levels = RiverKey_use$Pool))


## extract out site projections
siteProjection <-
    hyperAndSiteProjection %>%
    mutate(Pool_plot = factor(Pool, levels = RiverKey_use$Pool)) %>%
    filter(grepl("site", Parameter))
    

                                      
dataVBplot <-
    ggplot() + 
    geom_point(data = dat3, aes(x = Age3, y = TLm), alpha = 0.1) + 
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() +
    facet_grid( Species ~ River + Pool_plot) +
    geom_line(data = siteProjection ,
              aes(x = age - 1, y = mean, color = River)) + 
    geom_ribbon(data = siteProjection,
                aes(x = age - 1, ymin = L80, ymax = U80, fill = River),
                alpha = 0.25)  +
    geom_ribbon(data = siteProjection,
                aes(x = age -1, ymin = L95, ymax = U95, fill = River),
                alpha = 0.25)  +
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 5)) +
    scale_color_manual(values = c( "blue", "seagreen", "orange")) +
    scale_fill_manual(values = c( "blue", "seagreen", "orange")) +
    theme(legend.position="none")
print(dataVBplot)

ggsave("dataVBplotNot0.pdf", dataVBplot, width = 14, height = 8)

hyperProjection <- 
    hyperAndSiteProjection %>%
    filter( grepl("hyper", Parameter))

hyperPlot <-
    ggplot(data = hyperProjection, aes(x = age - 1, y = mean)) +
    geom_line(color = 'black', size = 1.3) +
    geom_ribbon(aes(x = age -1, ymin = L95, ymax = U95), fill = 'grey50',
                alpha = 0.25) + 
    geom_ribbon(aes(x = age -1, ymin = L80, ymax = U80), fill = 'grey50',
                alpha = 0.25) +
    geom_line(data = siteProjection,
              aes(x = age -1, y = mean, group = Pool),
              size = 1.1) +
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 2)) + 
    facet_grid(~ Species)

print(hyperPlot)

ggsave("hyperPlotNot0.pdf", hyperPlot, width = 8, height = 4)
ggsave("hyperPlotNot0.jpg", hyperPlot, width = 6, height = 4)


poolNames <- siteProjection %>%
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
    geom_line(data = siteProjection,
              aes(x = age -1, y = mean, group = Pool, color = River),
              size = 1.1) +
    scale_color_manual(values = c("red", "blue", "gold")) + 
    xlab("Age (years)") +
    ylab("Length (m)") + theme_minimal() + 
    scale_x_continuous(breaks = seq(0, max(ageProjection), by = 2)) + 
    facet_grid(~ Species)

print(hyperPlot)


hyperPlotWithLabel <-
    hyperPlot +
    geom_text(data = poolLabelPlot, aes(x = age, y = y, label = Pool, color = River),
              hjust = "outward") + 
    xlim(c(0, 30))   +
    theme(legend.position="none") 

print(hyperPlotWithLabel)
ggsave("hyperPlotNot0.pdf", hyperPlotWithLabel, width = 8, height = 6)
ggsave("hyperPlotNot0.jpg", hyperPlotWithLabel, width = 8, height = 6)


## export data for simulations
head(coefAll)

                
write_csv(coefAll, "coefAll_vonB.csv")

## Plot coefficents and data for MS.
RiverKey <- read_csv("RiverKey.txt") %>%
    filter( River != "Hyper-parameter" )


coefRiver <-
    coefAll %>%
    full_join(RiverKey) %>%
    filter(!is.na(PoolID )) %>%
    filter(!grepl("Hyper", River))



vonBLat <-
    ggplot(coefRiver, aes(x = y, y = mean)) +
    geom_point(aes(y = mean, x = y, shape = River)) +
    facet_grid( Species ~ ParameterPlot,
               labeller = labeller(ParameterPlot = label_parsed),
               scales = "free_x")  +
    xlab("Latitude") +
    ylab("Parameter estimate") +
    geom_smooth(method = 'lm', se =FALSE) +
    theme_bw() +
    theme(strip.background = element_blank()) 
print(vonBLat)
ggsave("vonBLat.pdf", width = 6, height = 4)


## Expand data for some other plotting 
coefRiverWide <- coefRiver %>%
    select(River, Pool, x, y, mean, Parameter, Species) %>%
    spread(Parameter, mean)

coefRiverWide

library(GGally)

ggPairsData <- coefRiverWide %>%
    select(Linf, K, M)

pairsPlot <- ggpairs(ggPairsData) +
    theme_bw() +
    theme(strip.background = element_blank()) 
pairsPlot
ggsave("pairsPlot.pdf", width = 6, height = 6)

## Plot data
RiverKey <- read_csv("RiverKey.txt") %>%
    filter( River != "Hyper-parameter" )


dat2a <-
    dat %>% 
    mutate(Species = recode(Species,
                            SVCP = "Silver carp",
                            BHCP = "Bighead carp"))
dat3a <-
    dat2a %>%
    full_join(RiverKey, by = 'Pool') %>%
    filter(!is.na(River) & !is.na(Species))


dat3a <- 
    dat3a %>%
    mutate(Pool_plot = factor(Pool, levels = RiverKey$Pool))

lengthPlot <-
    ggplot(dat3a, aes(x = Pool_plot, y = TLm)) +
    geom_violin(draw_quantiles = .5) +
    geom_point(size = 0.4) + 
    facet_grid( River ~ Species, scales = 'free_y') +
    coord_flip() +
    theme_bw() +
    theme(strip.background = element_blank()) + 
    ylab("Length (m)") +
    xlab("Pool")
print(lengthPlot)
ggsave("lengthPlot.pdf", width = 6, height = 6)


datAge <-
    dat3a %>%
    filter(!is.na(Age3))

agePlot <- ggplot(datAge, aes(x = Pool_plot, y = Age3)) +
    geom_violin(draw_quantiles = .5) +
    geom_point(size = 0.4) + 
    facet_grid( River ~ Species, scales = 'free_y') +
    coord_flip() +
    theme_bw() +
    theme(strip.background = element_blank()) + 
    ylab("Age (years)") +
    xlab("Pool")
print(agePlot)

ggsave("AgePlot.pdf", width = 6, height = 6)

