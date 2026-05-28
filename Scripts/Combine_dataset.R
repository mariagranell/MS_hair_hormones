# ---------------
# Title: Test analysis
# Date: 18 april 2026
# Author: mgranellruiz
# Goal: Have a look how the different services relate to cortisol and testosterone

# things that I have had in mind:
# I calculated MS with the MSI: /Users/mariagranell/Repositories/male_services_index/MSpublication/Scripts/MSIndex_automated.R
# with the individuals version of the fucntion. To define what was the MS period, I took the 6 months prior int he first darting
# then the 6 months following if it was a reshave individual and the 6 months before the shave if it was the first time it was captured.

# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')
# plotting
library(patchwork)
library(ggplot2)
library(ggside)
library(ggpubr)
library(gridExtra)
library(ggtext)
# models
library(lme4)
library(ggstatsplot)
library(fitdistrplus)
library(gamlss)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
library(rstatix)
library(effects)
library(emmeans)

make.corr.stars <- aes(
  #group = InmName,
  #color = InmName,
  label = paste(..r.label.., cut(..p..,
                                  breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                  labels = c("'****'", "'***'", "'**'", "'*'", "'ns'")),
                sep = "~"))

# path ------------------------
setwd("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/")

# data ------------------------
# hormone data of 2022 and 2023
horm_df_base <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/horm_df_base.csv")
# bring the male service data done with /Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/Scripts/MSIndex_automated.R
# equivalent to /Users/mariagranell/Repositories/male_services_index/MSpublication_beforecleanup/Scripts/MSIndex_automated.R the indvidual function and the com output
ms <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/MSIndex_individual_com.csv")
# rank data
rank <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/ELO_maleservices_hair.csv") %>% rename(AnimalCode = IDIndividual1)
# csi data
csi <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/CSI_maleservices_hair.csv")

# combine rank and csi
rc <- left_join(csi, rank,  by=c("PeriodCalc", "AnimalCode", "Age_class", "Group", "overlap_start", "overlap_end", "Sex", "DaysPresent", "DartingDate")) %>%
  group_by(AnimalCode, PeriodCalc, DartingDate)  %>%
  summarize(
    zCSI = ifelse(all(is.na(zCSI)), NA_real_, mean(zCSI, na.rm = TRUE)),
    elo  = ifelse(all(is.na(elo)),  NA_real_, mean(elo,  na.rm = TRUE)),
    .groups = "drop"
  )

# migration status
{
  # so I am going to calculate 4 things,
  # for individuals that have been shaved for the first time (independently of this being in darting 2022 or 2023) -> 7 months prior
  # for individuals that are reshaved -> 7 months after aaand
  # -> the next 7 months.

  # condition 1, individual is shaved for the first time - seven months
  cond1<-horm_df_base %>%
    filter(Condition == "Shaved") %>%
    mutate(DartingDate=ymd(DartingDate),
           MSStartDate=DartingDate - months(7),
           MSEndDate=DartingDate,
           year=ifelse(DartingSeason == "darting2022", 2022, 2023)
    ) %>%
    mutate(PeriodCalc="sevenmonths")

  # condition 2, individual is shaved for the first time - 1 year
  cond2<-horm_df_base %>%
    filter(Condition == "Shaved") %>%
    mutate(DartingDate=ymd(DartingDate),
           MSStartDate=DartingDate - months(12),
           MSEndDate=DartingDate,
           year=ifelse(DartingSeason == "darting2022", 2022, 2023)
    ) %>%
    mutate(PeriodCalc="oneyear")

  # condition 3, individual is reshaved + 7 months
  cond3<-horm_df_base %>%
    filter(Condition == "Reshaved") %>%
    mutate(DartingDate=ymd(DartingDate),
           MSStartDate=DartingDate,
           MSEndDate=DartingDate + months(6),
           year=ifelse(DartingSeason == "darting2022", 2022, 2023)
    ) %>%
    mutate(PeriodCalc="sevenmonths")

  # condition 4, individual is reshaved - 1 year
  cond4<-horm_df_base %>%
    filter(Condition == "Reshaved") %>%
    mutate(DartingDate=ymd(DartingDate),
           MSStartDate=DartingDate,
           MSEndDate=DartingDate + months(12),
           year=ifelse(DartingSeason == "darting2022", 2022, 2023)
    ) %>%
    mutate(PeriodCalc="oneyear")

  # condition 5, the shave and reshave is shit. Just do 4 months before shaving
  cond5<-horm_df_base %>%
    mutate(DartingDate=ymd(DartingDate),
           MSStartDate=DartingDate - months(4),
           MSEndDate=DartingDate,
           year=ifelse(DartingSeason == "darting2022", 2022, 2023)
    ) %>%
    mutate(PeriodCalc="alaputa")

  migration<-rbind(cond1, cond2, cond3, cond4, cond5) %>%
    dplyr::select(AnimalCode, MSStartDate, MSEndDate, PeriodCalc, DartingDate) %>%
    left_join(lh %>%
                dplyr::select(AnimalCode, StartDate_mb, EndDate_mb, Group_mb) %>%
                distinct(), by="AnimalCode", relationship="many-to-many") %>%
    filter(StartDate_mb <= MSEndDate & EndDate_mb >= MSStartDate) %>%
    distinct() %>%
    # update the male services timeline considering migration for each group
    # so that while accepting the overal timeline we calcualte before, adjusting it to the actual presence of the monkey in each group
    mutate(MSStartDate=ifelse(MSStartDate < StartDate_mb, as.character(StartDate_mb), as.character(MSStartDate)),
           MSEndDate=ifelse(MSEndDate > EndDate_mb, as.character(EndDate_mb), as.character(MSEndDate)),
           DaysPresent=ymd(MSEndDate) - ymd(MSStartDate),
           DartingDate = as.character(DartingDate),
           has_mating_season = purrr::map2_lgl(as.Date(MSStartDate), as.Date(MSEndDate), \(start, end) {
      seq(start, end, by = "day") %>%
        month() %>%
        `%in%`(4:6) %>%
        any()}
    )) %>%
    group_by(AnimalCode, DartingDate, PeriodCalc) %>%
    summarize(number_groups=n(), has_mating_season = any(has_mating_season, na.rm = TRUE)) %>%
    mutate(number_groups = as.factor(ifelse(number_groups == 1, "one", "two"))) %>% distinct()

  rm(cond1, cond2, cond3, cond4)
}

# combine df
df <- horm_df_base %>%
  left_join(., ms, by=c("AnimalCode", "DartingDate"), relationship = "many-to-many") %>%
  left_join(., rc, by=c("PeriodCalc", "AnimalCode", "DartingDate"), relationship = "many-to-many") %>%
  left_join(.,migration, by = c("PeriodCalc", "AnimalCode", "DartingDate")) %>%
  mutate(DartingSeason = ifelse(DartingSeason == "darting2022", "dartingMating", "dartingBaby"),
         tracker = paste0(AnimalCode, DartingSeason),
          PeriodCalc = ifelse(PeriodCalc == "alaputa", "fourmonths", PeriodCalc)) %>%
  # select only adult males
  filter(Sex == "M") %>%
  left_join(.,lh %>% dplyr::select(AnimalCode, Tenure_type, Group_mb) %>% filter(Tenure_type== "BirthGroup"),
                                                              by = c("AnimalCode", "DartingGroup"="Group_mb")) %>%
  filter(is.na(Tenure_type)) %>% select(-Tenure_type) %>% distinct()
  #pivot_wider(names_from = hormones, values_from = concentration, values_fn = ~ mean(.x, na.rm = TRUE)) %>% distinct()
rm(ms,rc,migration,csi,rank,horm_df_base)

length(unique(df$tracker))
missing_trackers <- df %>%
  count(tracker, name = "n_rows") %>%
  filter(n_rows != 2)

# counting individuals
length(unique(df$AnimalCode)) # 38 unique adult males
length(unique(df$DartingGroup)) # 8 unique groups
table(df$DartingSeason) # 25 (2022) and 26 (2023)

investigating_period = "alaputa"

dfm <- df %>%
  filter(PeriodCalc == investigating_period,
         DartingSeason == "dartingMating")
dfb <- df %>%
  filter(PeriodCalc == investigating_period,
         DartingSeason == "dartingBaby")


# modelling
# we are going to look at how the long term horm profile is reflected by the services they did
df_modelling <- df %>%
  filter(PeriodCalc == investigating_period) %>%
  mutate(
    log_testosterone = as.character(log(Testosterone +1)),
    log_cortisol = as.character(log(Cortisol + 1))
  ) %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>%
  mutate(
    log_testosterone = as.numeric(log_testosterone),
    log_cortisol = as.numeric(log_cortisol),
  ) %>%
  #select(-Crs_prop, -Vig_prop, -AlarmMP_prop) %>%
  drop_na() %>% distinct()   %>%
  filter(AnimalCode != "Sey")

nrow(df_modelling)
# sevenmonths = 26
# oneyear = 28
# alaputa = 28


# investigfate distribution
plot(fitdist(df_modelling$log_testosterone, "norm")); shapiro.test(df_modelling$log_testosterone)
plot(fitdist(df_modelling$log_cortisol, "norm")); shapiro.test(df_modelling$log_cortisol)

ggplot(df_modelling %>% filter(PeriodCalc== "alaputa"), aes(y=Testosterone, x= Condition, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.7) + geom_boxplot(alpha = 0.3, outlier.shape = NA)
ggplot(df %>% filter(PeriodCalc== "sevenmonths"), aes(y=Testosterone, x= DartingSeason, colour = mean_sampling_effort)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.7) + geom_boxplot(alpha = 0.3, outlier.shape = NA)

ggplot(df_modelling %>% filter(PeriodCalc== "sevenmonths"), aes(y=DaysPresent, x= DartingSeason, colour = DartingSeason)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) + geom_boxplot(alpha = 0.3, outlier.shape = NA)

{
b7 <-ggplot(df %>% filter(PeriodCalc== "sevenmonths"), aes(y=Testosterone, x= BGE_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
s7 <-ggplot(df %>% filter(PeriodCalc== "sevenmonths"), aes(y=Testosterone, x= Vig_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
a7 <- ggplot(df %>% filter(PeriodCalc== "sevenmonths"), aes(y=Testosterone, x= AlarmMP_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
c7 <-ggplot(df %>% filter(PeriodCalc== "sevenmonths"), aes(y=Testosterone, x= Crs_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
seven <- a7+b7+c7+s7

bo <-ggplot(df %>% filter(PeriodCalc== "oneyear"), aes(y=Testosterone, x= BGE_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
so <-ggplot(df %>% filter(PeriodCalc== "oneyear"), aes(y=Testosterone, x= Vig_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
ao <- ggplot(df %>% filter(PeriodCalc== "oneyear"), aes(y=Testosterone, x= AlarmMP_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
co <-ggplot(df %>% filter(PeriodCalc== "oneyear"), aes(y=Testosterone, x= Crs_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
one <- ao+bo+co+so

ba <-ggplot(df %>% filter(PeriodCalc== "alaputa"), aes(y=Testosterone, x= BGE_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
sa <-ggplot(df %>% filter(PeriodCalc== "alaputa"), aes(y=Testosterone, x= Vig_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
aa <- ggplot(df %>% filter(PeriodCalc== "alaputa"), aes(y=Testosterone, x= AlarmMP_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
ca <-ggplot(df %>% filter(PeriodCalc== "alaputa"), aes(y=Testosterone, x= Crs_prop, colour = DartingSeason)) +
   geom_jitter(width = 0.2, height = 0, alpha = 0.6, aes(size = mean_sampling_effort)) + geom_smooth(method = "lm")
alaputa <- aa+ba+ca+sa

seven
one
alaputa
}
# model
m0 <- lmer(log_testosterone ~ (BGE_prop + elo + AlarmMP_prop + Vig_prop + Crs_prop) : DartingSeason +
  DaysPresent + (1|AnimalCode),
           data = df_modelling
)

plot(simulateResiduals(m0, asFactor = F))
summary(m0)
Anova(m0)
plot(allEffects(m0))
AIC(m0)

services <- df_modelling %>%
  dplyr::select(
    AlarmMP_prop,
    BGE_prop,
    Vig_prop,
    Crs_prop
  )

cor(services, use = "pairwise.complete.obs")

m_season <- lmer(
  log_testosterone ~
    DartingSeason +
    Condition +
    (1 | AnimalCode),
  data = df_modelling
)
  # Results
summary(m_season)
plot_model(m_season, vline.color = "darkred", show.values = TRUE); Anova(m_season)
plot(allEffects(m_season))
# effect sizes
standardized_effects(m_season)
performance::r2(m_season)
ggplot(df_modelling,
       aes(Crs_prop, log_testosterone,
           colour = DartingSeason)) +
  geom_point() +
  geom_smooth(method = "lm")


####
library(dplyr)
library(purrr)
library(tibble)
library(lme4)
library(MuMIn)
library(performance)
library(parameters)

options(na.action = "na.fail")

# predictors to test one-by-one
candidate_predictors <- c(
  "BGE_prop",
  "elo",
  "AlarmMP_prop",
  "Vig_prop",
  "Crs_prop"
)

# prepare data
df_model_all <- df %>%
  filter(PeriodCalc %in% c("fourmonths", "sevenmonths", "oneyear")) %>%
  mutate(
    log_testosterone = log(Testosterone + 0.1),
    DartingSeason = as.factor(DartingSeason),
    AnimalCode = as.factor(AnimalCode)
  ) %>%
  group_by(PeriodCalc) %>%
  mutate(
    across(
      where(is.numeric) & !matches("^log_testosterone$"),
      ~ as.numeric(scale(.x))
    )
  ) %>%
  ungroup()

# function to fit one model
predictor = "Crs_prop"
fit_one_model <- function(data, predictor) {

  vars_needed <- c(
    "log_testosterone",
    predictor,
    "DartingSeason",
    "DaysPresent",
    "AnimalCode"
  )

  data_mod <-# data %>%
    df_model_all %>%
  filter(PeriodCalc == "fourmonths") %>%
    dplyr::select(all_of(vars_needed)) %>%
    tidyr::drop_na()

  form <- as.formula(
    paste0(
      "log_testosterone ~ ",
      predictor,
      " + DartingSeason + DaysPresent + (1 | AnimalCode)"
    )
  )

  aa <- lmer(form, data = data_mod, REML = FALSE)
}

summary(aa)
plot(allEffects(aa))
# null model per period
fit_null_model <- function(data) {

  data_mod <- data %>%
    dplyr::select(
      log_testosterone,
      DartingSeason,
      DaysPresent,
      AnimalCode
    ) %>%
    tidyr::drop_na()

  lmer(
    log_testosterone ~ DartingSeason + DaysPresent + (1 | AnimalCode),
    data = data_mod,
    REML = FALSE
  )
}

# fit all models
model_table <- df_model_all %>%
  group_by(PeriodCalc) %>%
  nest() %>%
  mutate(
    null_model = map(data, fit_null_model),
    models = map(
      data,
      ~ set_names(
        map(candidate_predictors, \(x) fit_one_model(.x, x)),
        candidate_predictors
      )
    )
  )

# comparative table
comparison_table <- model_table %>%
  mutate(
    model_summaries = map2(models, null_model, \(mods, null_mod) {
      all_mods <- c(list(null = null_mod), mods)

      tibble(
        Model = names(all_mods),
        AICc = map_dbl(all_mods, MuMIn::AICc),
        R2_marginal = map_dbl(all_mods, ~ performance::r2_nakagawa(.x)$R2_marginal),
        R2_conditional = map_dbl(all_mods, ~ performance::r2_nakagawa(.x)$R2_conditional)
      ) %>%
        mutate(
          Delta_AICc = AICc - min(AICc, na.rm = TRUE),
          Akaike_weight = exp(-0.5 * Delta_AICc) / sum(exp(-0.5 * Delta_AICc)),
          Rank = rank(AICc, ties.method = "first")
        ) %>%
        arrange(AICc)
    })
  ) %>%
  dplyr::select(PeriodCalc, model_summaries) %>%
  tidyr::unnest(model_summaries)

comparison_table

safe_r2_marginal <- function(mod) {
  out <- suppressWarnings(performance::r2_nakagawa(mod))
  as.numeric(out$R2_marginal)
}

comparison_table <- model_table %>%
  mutate(
    model_summaries = map2(models, null_model, \(mods, null_mod) {

      all_mods <- c(list(null = null_mod), mods)

      tibble(
        Model = names(all_mods),
        n = map_int(all_mods, nobs),
        AICc = map_dbl(all_mods, MuMIn::AICc),
        R2_marginal = map_dbl(all_mods, safe_r2_marginal),
        singular = map_lgl(all_mods, lme4::isSingular)
      ) %>%
        mutate(
          Delta_AICc = AICc - min(AICc, na.rm = TRUE),
          Akaike_weight = exp(-0.5 * Delta_AICc) / sum(exp(-0.5 * Delta_AICc)),
          Rank = rank(AICc, ties.method = "first")
        ) %>%
        arrange(AICc)
    })
  ) %>%
  dplyr::select(PeriodCalc, model_summaries) %>%
  tidyr::unnest(model_summaries)

comparison_table

comparison_table <- model_table %>%
  mutate(
    model_summaries = map2(models, null_model, \(mods, null_mod) {

      all_mods <- c(list(null = null_mod), mods)

      tibble(
        Model = names(all_mods),
        n = map_int(all_mods, nobs),
        AICc = map_dbl(all_mods, MuMIn::AICc),
        R2_marginal = map_dbl(all_mods, safe_r2_marginal),
        singular = map_lgl(all_mods, lme4::isSingular)
      ) %>%
        mutate(
          Delta_AICc = AICc - min(AICc, na.rm = TRUE),
          Akaike_weight = exp(-0.5 * Delta_AICc) / sum(exp(-0.5 * Delta_AICc)),
          Rank = rank(AICc, ties.method = "first")
        ) %>%
        arrange(AICc)
    })
  ) %>%
  dplyr::select(PeriodCalc, model_summaries) %>%
  tidyr::unnest(model_summaries)

comparison_table

Anova(m_cross_4months)