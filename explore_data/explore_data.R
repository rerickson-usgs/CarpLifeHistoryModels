## The purpose of this file is to explore
## the data prior to modeling it.
## The results are presented in a Markdown file.

## @knitr load_libs
suppressMessages({
    library(data.table) # used for data manipulation
    library(lubridate) # used to format date
    library(ggplot2) # used for plotting
    library(ggthemes) # used with plots
    library(scales) # used with plots
    library(lme4) # Used to explore data
    library(broom) # Used to examine lmer output
})

## @knitr read_in_data
dat <- fread("data_use.csv")

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

## new_pool_order[!new_pool_order %in% dat[ , levels(factor((Pool)))]]

dat[ , Pool := factor(Pool,
                      levels = new_pool_order
                      )]
## Order gear_summary
new_gear_summary_order <-
    c("Electrofishing",
      "Commercial",
      "Jumped",
      "Net",
      "Unknown/Other")

## new_gear_summary_order[ !new_gear_summary_order %in%
##                         dat[ , levels(factor(Gear_summary))]]

dat[ , Gear_summary := factor(Gear_summary,
                              levels = new_gear_summary_order), ]

## Create summaries of the data
dat_year_pool  <- dat[ , .N, by = .(Pool, Year, System, Gear_summary,
                                    Species),]
dat_pool  <- dat[ , .N, by = .(Pool, System, Gear_summary, Species),]
dat_pool_c  <- dat[ , .N, by = .(Pool, System, Commercial, Species),]


## @knitr gear_summary
gg_gear_summary <-
    ggplot(dat_year_pool,
       aes(x = Pool, y = N, fill = Gear_summary)) +
    facet_grid( System ~ Species, scales = "free_y") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(strip.background = element_blank()) +
    scale_fill_colorblind("Gear type") +
    scale_y_continuous(label=comma)  +
    ylab("Number of fish") +
    xlab("Pool")
print(gg_gear_summary)
ggsave("gg_gear_summary.jpg", gg_gear_summary,
       width = 6, height = 4)

## @knitr examine_comm
gg_comm <-
    ggplot(dat_pool_c, aes(x =  Pool, y = N, fill = Commercial)) +
    geom_col() +
    facet_grid( System ~ Species, scales = "free_y") +
    scale_y_continuous(label = comma) +
    coord_flip() +
    scale_fill_colorblind("Gear type") +
    theme_bw() +
    theme(strip.background = element_blank())
print(gg_comm)
ggsave("gg_com.jpg", gg_comm,
       width = 6, height = 4)


## @knitr examine_comm_time
dat_year_pool_IL <-
    dat[ System == "Illinois", .N, by = .(Pool, Commercial, Year, Species)]

ggplot(dat_year_pool_IL, aes(x = Year, y = N, fill = Commercial)) +
    geom_col() +
    facet_grid(Species ~ Pool) +
    scale_fill_colorblind("Fish source") +
    theme_bw() +
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = -45, hjust = 0))

## @knitr all_length_by_harvest
dat_tl <- dat[ !is.na(TLm), ]
dat_tl[ , Pool := factor(Pool)]
options(warn=-1)

ggplot(dat_tl,
       aes(x = Pool, y = TLm,
           fill = Gear_summary)) +
    facet_grid( . ~ System , scales = 'free') +
    geom_violin(draw_quantiles = 0.5) +
    scale_fill_colorblind("Gear type") +
    theme_bw() +
    ylab("Total length (m)") +
    xlab("Pool") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0),
          strip.background = element_blank())
options(warn=0)

## @kntir run_lmer_length
lmer_len_com <- lmer(TLm ~ Gear_summary + Commercial - 1+ (1|Pool), data = dat)
print(summary(lmer_len_com))

options(warn=-1)
lmer_len_com_dt <-data.table(tidy(lmer_len_com, conf.int= TRUE))
options(warn=0)

len_com <- round(lmer_len_com_dt[ grep("Non-", term), estimate], 3)
len_com_l95 <- round(lmer_len_com_dt[ grep("Non-", term), conf.low], 3)
len_com_u95 <- round(lmer_len_com_dt[ grep("Non-", term), conf.high], 3)

len_ef <- round(lmer_len_com_dt[ grep("Electro", term), estimate], 3)
len_ef_l95 <- round(lmer_len_com_dt[ grep("Electro", term), conf.low], 3)
len_ef_u95 <- round(lmer_len_com_dt[ grep("Electro", term), conf.high], 3)

len_net <- round(lmer_len_com_dt[ grep("Net", term), estimate], 3)
len_net_l95 <- round(lmer_len_com_dt[ grep("Net", term), conf.low], 3)
len_net_u95 <- round(lmer_len_com_dt[ grep("Net", term), conf.high], 3)

len_jumped <- round(lmer_len_com_dt[ grep("Jumped", term), estimate], 3)
len_jumped_l95 <- round(lmer_len_com_dt[ grep("Jumped", term), conf.low], 3)
len_jumped_u95 <- round(lmer_len_com_dt[ grep("Jumped", term), conf.high], 3)

len_commercial <- round(lmer_len_com_dt[ grep("Commercial", term), estimate], 3)
len_commercial_l95 <- round(lmer_len_com_dt[ grep("Commercial", term), conf.low], 3)
len_commercial_u95 <- round(lmer_len_com_dt[ grep("Commercial", term), conf.high], 3)

len_unknown <- round(lmer_len_com_dt[ grep("Unknown", term), estimate], 3)
len_unknown_l95 <- round(lmer_len_com_dt[ grep("Unknown", term), conf.low], 3)
len_unknown_u95 <- round(lmer_len_com_dt[ grep("Unknown", term), conf.high], 3)


## @knitr only_examine_pools_with_harvest
pools_with_harvest <- dat[ Commercial == "Commercial", unique(Pool)]

ggplot(dat[ Pool %in% pools_with_harvest & !is.na(TLm), ],
       aes(x = Gear, color = Commercial, y = TLm)) +
    geom_boxplot(varwidth = TRUE) +
    scale_color_colorblind() +
    theme_bw() +
    ylab("Total length (m)") +
    xlab("Gear") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0))

## @knitr age_gear_IL
ggplot(dat[ Pool %in% pools_with_harvest & !is.na(Age), ],
       aes(x = Gear, y = Age)) +
    geom_boxplot() +
    facet_grid(~Pool, scales = 'free_x') +
    scale_color_colorblind() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, hjust = 0),
          strip.background = element_blank()) +
    xlab("Gear") +
    ylab("Age (y)")

## @knitr age_all_gear
pools_age <- dat[ !is.na(Age), unique(Pool)]
pools_age_order <-
    new_pool_order[new_pool_order %in% pools_age]

dat_age <- dat[ !is.na(Age), ]
dat_age[ , Pool := factor(Pool, levels = pools_age_order)]


ggplot(dat_age,
       aes(x = Pool, y = Age, fill = Gear_summary)) +
    geom_violin(draw_quantiles = 0.5, alpha = 0.5) +
    facet_grid( ~ System, scales = 'free_x')  +
    scale_fill_colorblind("Gear") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, hjust = 0),
          strip.background = element_blank()) +
    xlab("Pool") +
    ylab("Age (y)")

## @knitr examine_mat
dat_mat_pool <-
    dat[ !is.na(TL) & !is.na(Maturity), .N,
           by = .( Pool, Maturity, System)]

dat_mat_pool[ , Pool := factor(Pool)]
dat_mat_pool[ , System := factor(System)]

ggplot(dat_mat_pool,
       aes(x =Pool, y =N, fill = factor(Maturity))) +
    geom_col(position = position_dodge()) +
    facet_grid(. ~ System, scales = 'free_x') +
    scale_fill_colorblind("Maturity") +
    theme_bw() +
    theme(strip.background = element_blank())

pools_with_mat <- dat_mat_pool[ Maturity == 0, unique(Pool)]

## @knitr explore_mat
ggplot( dat[ !is.na(TLm) & !is.na(Maturity) & Pool %in% pools_with_mat,],
       aes(x = TLm, y = Maturity)) +
    geom_jitter(width =0, height = 0.05) +
    facet_grid( ~ Pool + System) +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial')) +
    scale_color_colorblind() +
    theme_bw() +
    theme(strip.background = element_blank()) +
    ylab("Maturity (1 = yes, 0 = no)") +
    xlab("Total length (m)")

## @knitr explore_sex_length
ggplot( dat[ !is.na(Age) & !is.na(TLm),],
       aes(x = Pool, y = TLm, color = Sex)) +
    geom_boxplot(varwidth = FALSE) +
    facet_grid( . ~ System, scales = 'free_x') +
    scale_color_colorblind() +
    ylab("Total length (m)") +
    xlab("Pool") +
    theme_bw() +
    theme(strip.background = element_blank())

lmer_len_sex <- lmer(TLm ~ Sex + (1|Pool),
                     data = dat)
summary(lmer_len_sex)

options(warn=-1)
lmer_len_sex_dt  <- data.table(tidy(lmer_len_sex, conf.int = TRUE))
options(warn=0)

sex_len_male <-
    round(lmer_len_sex_dt[ grep("Male", term), estimate], 3)
sex_len_male_u95 <-
    round(lmer_len_sex_dt[ grep("Male", term), conf.high], 3)
sex_len_male_l95 <-
    round(lmer_len_sex_dt[ grep("Male", term), conf.low], 3)

## @knitr explore_sex_age
ggplot( dat[ !is.na(Age) & !is.na(TLm),],
       aes(x = Pool, y = Age, color = Sex)) +
    geom_boxplot(varwidth = FALSE) +
    facet_grid( . ~ System, scales = 'free_x') +
    geom_smooth(aes(color = Sex), se = FALSE) +
    scale_color_colorblind() +
    ylab("Age (y)") +
    xlab("Pool") +
    theme_bw() +
    theme(strip.background = element_blank())


lmer_age_sex <- lmer(Age ~ Sex + (1|Pool),
                     data = dat)


options(warn=-1)
lmer_age_sex_dt  <- data.table(tidy(lmer_age_sex, conf.int = TRUE))
options(warn=0)

sex_age_male <-
    round(lmer_age_sex_dt[ grep("Male", term), estimate], 3)
sex_age_male_l95 <-
    round(lmer_age_sex_dt[ grep("Male", term), conf.low], 3)
sex_age_male_u95 <-
    round(lmer_age_sex_dt[ grep("Male", term), conf.high], 3)


## @knitr explore_age_length
ggplot( dat[ !is.na(Age) & !is.na(TLm),],
       aes(x = Age, y = TLm, color = Sex)) +
    geom_jitter(width = 0, height = 0.0) +
    stat_smooth() +
    scale_color_colorblind() +
    theme_bw() +
    theme(strip.background = element_blank())

## @knitr age_len_sex_explore
age_length_sex <-
    dat[ !is.na(Age) & !is.na(TLm),
        .(TLm_mean = mean(TLm)),
        by = .(Sex, Age = round(Age), System)]

ggplot(age_length_sex,
       aes( x = Age, y = TLm_mean, color = Sex)) +
    geom_point()  +
    facet_grid(~ System) +
    scale_color_colorblind() +
    geom_line() +
        theme_bw() +
    theme(strip.background = element_blank())

## @knitr explore_length_weigth
ggplot( dat[ !is.na(WTkg) & !is.na(TLm) & Sex %in% c("Male", "Female"),],
       aes(x = TLm, y = WTkg, color = Sex)) +
    geom_jitter(width = 0, height = 0.0, alpha = 0.1) +
    geom_smooth(method = 'lm', se = FALSE) +
    scale_color_colorblind() +
    xlab("Total length (m)") +
    ylab("Weight (kg)") +
    theme_bw() +
    scale_x_log10() +
    scale_y_log10() +
    theme(strip.background = element_blank())
