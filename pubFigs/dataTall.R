library(ggplot2)
library(lubridate)
library(rstan)
library(tidyverse)
library(data.table)
library(scales)


## Read in all data and fromat
dat <- fread("../vonB/data_use.csv")
dat[ Sex == "Unkown", Sex := "Unknown"]

## Order pools
new_pool_order <-
    c("Pool 14",
      "Pool 16",
      "Pool 17",
      "Pool 18",
      "Pool 19",
      "Pool 20",
      "Pool 22",
      "Pool 24",
      "Pool 26",
      "Pool 27",
      "Alton",
      "LaGrange",
      "Peoria",
      "Starved Rock",
      "Marseilles",
      "Dresden Island",
      "JT Myers",
      "Newburgh",
      "Cannelton",
      "McAlpine",
      "Markland",
      "Meldahl",
      "RC Byrd"
      )

dat[ , Pool := factor(Pool,
                      levels = new_pool_order
                      )]

dat[ , Sampdate :=ymd(Sampdate)]

## vonb
dat_vb <- dat[ !is.na(TL) & !is.na(Age),
              .N, by = .(Species, Pool, System)]
dat_vb

## maturity
dat_mat  <- dat[ !is.na(TL) & !is.na(Maturity),
                .N, by = .(Species, Pool, System)]
dat_mat

## length-weight
dat_lw <- dat[ !is.na(TL) & !is.na(WT),
              .N, by = .(Species, Pool, System)]

dat_lw

## Examine sex ratio
sex_total <- dat[ , .N, by = .(Species, Sex)]

sex_total[ Sex == "Immature", Sex := "Unknown"]
sex_total <- sex_total[ , .(N = sum(N)), by = .(Species, Sex)]
sex_total[ , species_total := sum(N), by = Species]
sex_total[ , sex_per := round(N/species_total, 3) * 100]
sex_total

SexTotalNoNA <- dat[ Sex != "Unknown" & Sex !="Immature",
                    .N, by = .(Species, Sex)]
SexTotalNoNA[ , SpeciesTotal := sum(N), by = Species]
SexTotalNoNA[ , SexPer := round(N/SpeciesTotal, 3) * 100]
SexTotalNoNA

## Join tables together
dat_vb[ , Model := "von Bertalanffy"]
dat_lw[ , Model := "Length-weight"]
dat_mat[ , Model := "Maturity"]


sums <-
    rbind(
    dat_vb,
    dat_lw,
    dat_mat
    )

sums[ , PoolPlot := paste0(Pool, " (N = ", N, ")")]
sums

sums[ , Species := factor(Species)]

sumsPlot <-
    ggplot(sums, aes(x = Pool, y = N, fill = Species)) +
    geom_col(position = 'dodge') +
    facet_grid( System ~  Model, scales ="free_y") +
    coord_flip() +
    scale_fill_manual(values = c("red", "blue")) +
    theme_bw() +
    theme(strip.background = element_blank()) +
    scale_y_continuous(label=comma)
sums
print(sumsPlot)
ggsave("countPlot.pdf", sumsPlot, width = 9, height = 5)
ggsave("countPlot.jpg", sumsPlot, width = 9, height = 5)



