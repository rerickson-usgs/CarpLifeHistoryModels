library(ggplot2)
library(lubridate)
library(rstan)
library(tidyverse)
library(data.table)
library(scales)

## Von B
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
vonBtablePool <- dat2[ Species %in% c("SVCP", "BHCP"),
                      .N, by = .(Species, Pool)]

vonBtable <- dat2[ Species %in% c("SVCP", "BHCP"),
                  .N, by = .(Species)]
vonBtable


## maturity
## Load and format raw data
dat <- fread("../DemographicsData.csv")
dat[ , Sampdate :=ymd(Sampdate)]
dat2 <- dat[ Maturity != "NA",]
dat2[ , Maturity := factor(Maturity)]
dat2[ , M2 := as.numeric(Maturity) - 1]
dat2[ , TLm := TL / 1000]
dat2[ , WTkg := WT/1000]
dat2[ , Month := month(Sampdate)]
dat2[ , SpeciesFull := factor(Species, levels = c("BHCP", "SVCP"), labels = c("Bighead carp", "Silver carp"))]

MattablePool <- dat2[ Species %in% c("SVCP", "BHCP"),
                     .N, by = .(Species, Pool)]

Mattable <- dat2[ Species %in% c("SVCP", "BHCP"),
                  .N, by = .(Species)]
Mattable

## length-weight
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

## Examine sex ratio
SexTotal <- dat2[ Species %in% c("SVCP", "BHCP"), .N, by = .(Species, Sex)]
SexTotal[ , SpeciesTotal := sum(N), by = Species]
SexTotal[ , SexPer := N/SpeciesTotal]
SexTotal


SexTotalNoNA <- dat2[ !is.na(Sex) & Species %in% c("SVCP", "BHCP"), .N, by = .(Species, Sex)]
SexTotalNoNA[ , SpeciesTotal := sum(N), by = Species]
SexTotalNoNA[ , SexPer := N/SpeciesTotal]
SexTotalNoNA



LWtablePool <- dat2[ Species %in% c("SVCP", "BHCP"),
                     .N, by = .(Species, Pool)]

LWtable <- dat2[ Species %in% c("SVCP", "BHCP"),
                  .N, by = .(Species)]
LWtable

vonBtablePool[ , Model := "von Bertalanffy"]
MattablePool[ , Model := "Maturity"]
LWtablePool[ , Model := "Length-weight"]

sums <- rbind(
    vonBtablePool,
    MattablePool,
    LWtablePool
    )

sums
RiverKey <- fread("RiverKey.txt")
RiverKey[ , Pool := factor(Pool, levels = Pool)]
RiverKey <- RiverKey[ Pool!= "Hyper-parameter",]
sums[ Pool == "OR(pool 27)", Pool := "Pool 27"]
setkey(RiverKey, "Pool")
setkey(sums, "Pool")

sums <- RiverKey[sums]


sums[ , Pool := factor(Pool, levels = RiverKey[ , Pool])]

sums[ , PoolPlot := paste0(Pool, " (N = ", N, ")")]

sums[ , Species := factor(Species)]
levels(sums$Species) <- c("Bighead carp" , "Silver carp")

sumsPlot <-
    ggplot(sums, aes(x = Pool, y = N, fill = Species)) + geom_col(position = 'dodge') +
    facet_grid( River ~  Model, scales ="free_y") +
    coord_flip() +
    scale_fill_manual(values = c("red", "blue")) +
    theme_minimal() +
    scale_y_continuous(label=comma)
sums
print(sumsPlot)
ggsave("countPlot.pdf", sumsPlot, width = 9, height = 4)



