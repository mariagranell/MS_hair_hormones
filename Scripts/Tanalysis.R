# ---------------
# Title: statistical analysis of MS-hair Testosterone
# Date: 10-may 2026
# Author: mgranellruiz
# Goal: analyze the samples
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

# path ------------------------
setwd("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair")

# data ------------------------
df <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/HairHormonesMSdf.csv")

# complete separation of testosterone by DartingSeason
# thus we will model the data separated by seasons
ggplot(df, aes(x = DartingSeason, y = Testosterone, fill = DartingSeason, colour = DartingSeason)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(aes(shape = Condition), size = 8, alpha = 0.8) +
  theme_classic() +
  labs(x = "Darting season", y = "Testosterone") +
  theme(legend.position = "right", text = element_text(size = 18))

df %>% group_by(DartingSeason) %>%
  summarize(mean = mean(Testosterone), sd = sd(Testosterone))

# modeled data
df_model <- df %>% dplyr::select(Testosterone, zCSI, elo12, Tenure, Father, DartingSeason, AnimalCode, DartingGroup,
                                     BGE_prop, AlarmMP_prop,Vig_prop, MaleCarrer, elot, rank_trajectory) %>%
   # scale all numeric variables
    mutate(
    log_testosterone = as.character(log(Testosterone))) %>%
    mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>%
    mutate(
    log_testosterone = as.numeric(log_testosterone)) %>% drop_na() %>% distinct()

# data transformed to fit the normal distribution, ok fit
plot(fitdist(df_model$log_testosterone, "norm"))

# exploratory model
m0 <- lmer(log_testosterone ~ elot * DartingSeason + Tenure + zCSI + DartingGroup + (1|AnimalCode), data = df_model)
summary(m0)
plot_model(m0, vline.color = "darkred", show.values = TRUE, show.p = F); Anova(m0)
print(standardized_effects(m0), n = Inf)     # effect sizes
plot(allEffects(m0))
plot(simulateResiduals(m0))


  emtrends(m0, ~ DartingSeason, var = "elot") %>% summary(infer = TRUE)
  predT <- ggpredict(m0, terms = c("elot [all]", "DartingSeason")) %>% as.data.frame() # for plot
  #write.csv(predT, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predicted_elotSeason_T.csv", row.names = F)

# null models
null_model <- lmer(log_testosterone ~ elot + DartingSeason + DartingGroup + (1|AnimalCode), data = df_model)
anova(m0, null_model)
{ summary(null_model)
  plot_model(null_model, vline.color="darkred", show.values=TRUE, show.p=F); Anova(null_model)
  plot(allEffects(null_model))
  print(standardized_effects(null_model), n = Inf)     # effect sizes

  # model checks look good
  res<-simulateResiduals(null_model)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

# service models
alarm_model <- lmer(log_testosterone ~  AlarmMP_prop + elot + DartingSeason + DartingGroup + (1|AnimalCode), data = df_model)
anova(null_model, alarm_model) # no addition
{ summary(alarm_model)
  plot_model(alarm_model, vline.color="darkred", show.values=TRUE, show.p=F); Anova(alarm_model)
  plot(allEffects(alarm_model))
  print(standardized_effects(alarm_model), n = Inf)     # effect sizes

  predTalarm <- ggpredict(alarm_model, terms = "AlarmMP_prop") %>% as.data.frame() %>% mutate( Cortisol_pred = exp(predicted), Cortisol_low = exp(conf.low), Cortisol_high = exp(conf.high))
  write.csv(predTalarm, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTalarm", row.names = F)


  # model checks look good
  res<-simulateResiduals(alarm_model)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

bge_model <- lmer(log_testosterone ~  BGE_prop + elot + DartingSeason + DartingGroup + (1|AnimalCode), data = df_model)
anova(null_model, bge_model) # todo trend, but model not supported
{ summary(bge_model)
  plot_model(bge_model, vline.color="darkred", show.values=TRUE, show.p=F); Anova(bge_model)
  plot(allEffects(bge_model))
  plot(Effect("BGE_prop", bge_model))
  plot(simulateResiduals(bge_model))
  print(standardized_effects(bge_model), n = Inf)     # effect sizes

  predTbge <- ggpredict(bge_model, terms = "BGE_prop") %>% as.data.frame() %>% mutate( Cortisol_pred = exp(predicted), Cortisol_low = exp(conf.low), Cortisol_high = exp(conf.high))
  write.csv(predTbge, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTbge", row.names = F)


  # model checks look good
  res<-simulateResiduals(bge_model)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

vig_model <- lmer(log_testosterone ~  Vig_prop + elot + DartingSeason + DartingGroup + (1|AnimalCode), data = df_model)
anova(null_model, vig_model) # no addition
{ summary(vig_model)
  plot_model(vig_model, vline.color="darkred", show.values=TRUE, show.p=F); Anova(vig_model)
  plot(allEffects(vig_model))
  print(standardized_effects(vig_model), n = Inf)     # effect sizes

  predTvig <- ggpredict(vig_model, terms = "Vig_prop") %>% as.data.frame() %>% mutate( Cortisol_pred = exp(predicted), Cortisol_low = exp(conf.low), Cortisol_high = exp(conf.high))
  write.csv(predTvig, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTvig", row.names = F)


  # model checks look good
  res<-simulateResiduals(vig_model)
  plot(res)
  testUniformity(res)
  testDispersion(res) }


# c for crossing, there is one group AK that do not cross the river so we cannot calculate crossing behaviour for them
df_model_c <- df %>% dplyr::select(Testosterone, zCSI, elo12, Tenure, Father, DartingSeason, AnimalCode, DartingGroup,
                                     BGE_prop, AlarmMP_prop,Vig_prop, MaleCarrer, elot, rank_trajectory,
                                     Crs_prop) %>%
   # scale all numeric variables
    mutate(
    log_testosterone = as.character(log(Testosterone))) %>%
    mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>%
    mutate(
    log_testosterone = as.numeric(log_testosterone)) %>% drop_na() %>% distinct()

m0_c <- lmer(log_testosterone ~ elot * DartingSeason + Tenure + zCSI + DartingGroup + (1|AnimalCode), data = df_model_c)
summary(m0_c)
plot_model(m0_c, vline.color = "darkred", show.values = TRUE, show.p = F); Anova(m0_c)
plot(allEffects(m0_c))
print(standardized_effects(m0_c), n = Inf)     # effect sizes

null_model_c <- lmer(log_testosterone ~ elot + DartingSeason + DartingGroup + (1|AnimalCode), data = df_model_c)
anova(m0_c, null_model_c)
{ summary(null_model_c)
  plot_model(null_model_c, vline.color="darkred", show.values=TRUE, show.p=F); Anova(null_model_c)
  plot(allEffects(null_model_c))
  print(standardized_effects(null_model_c), n = Inf)     # effect sizes

  # model checks look good
  res<-simulateResiduals(null_model_c)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

crs_model_c <- lmer(log_testosterone ~ Crs_prop + elot + DartingSeason + DartingGroup + (1|AnimalCode), data = df_model_c)
anova(null_model_c, crs_model_c) # no addition
{ summary(crs_model_c)
  plot_model(crs_model_c, vline.color="darkred", show.values=TRUE, show.p=F); Anova(crs_model_c)
  plot(allEffects(crs_model_c))
  print(standardized_effects(crs_model_c), n = Inf)     # effect sizes

  predTcrs <- ggpredict(crs_model_c, terms = "Crs_prop") %>% as.data.frame() %>% mutate( Cortisol_pred = exp(predicted), Cortisol_low = exp(conf.low), Cortisol_high = exp(conf.high))
  write.csv(predTcrs, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTcrs", row.names = F)

  # model checks look good
  res<-simulateResiduals(crs_model_c)
  plot(res)
  testUniformity(res)
  testDispersion(res) }





# analysis separating by season
{ ### MATING SEASON
  df_mating<-df %>% filter(DartingSeason == "dartingMating")

  # model data
{ # abs stands for alarm, bgc (bge, between-group encounters and sentinelling (vigilance)
  df_model_mating_abs<-df_mating %>%
    dplyr::select(Testosterone, zCSI, elo12, Tenure, Father, DartingSeason, AnimalCode, DartingGroup,
                  BGE_prop, AlarmMP_prop, Vig_prop, MaleCarrer, elot, rank_trajectory) %>%
    # scale all numeric variables
    mutate(
      log_testosterone=as.character(log(Testosterone))) %>%
    mutate(across(where(is.numeric), ~as.numeric(scale(.)))) %>%
    mutate(
      log_testosterone=as.numeric(log_testosterone)) %>%
    drop_na() %>%
    distinct()

  # c for crossing, there is one group AK that do not cross the river so we cannot calculate crossing behaviour for them
  df_model_mating_c<-df_mating %>%
    dplyr::select(Testosterone, zCSI, elo12, Tenure, Father, DartingSeason, AnimalCode, DartingGroup,
                  BGE_prop, AlarmMP_prop, Vig_prop, MaleCarrer, elot, rank_trajectory,
                  Crs_prop) %>%
    # scale all numeric variables
    mutate(
      log_testosterone=as.character(log(Testosterone))) %>%
    mutate(across(where(is.numeric), ~as.numeric(scale(.)))) %>%
    mutate(
      log_testosterone=as.numeric(log_testosterone)) %>%
    drop_na() %>%
    distinct()

  nrow(df_model_mating_abs); nrow(df_model_mating_c) } # 18 and 16 males each

  # data transformed to fit the normal distribution, ok fit
  plot(fitdist(df_model_mating_abs$log_testosterone, "norm"))
  plot(fitdist(df_model_mating_c$log_testosterone, "norm"))

  # exploratory model
  m0_mating<-lm(log_testosterone ~ elot + Tenure + zCSI + DartingGroup, data=df_model_mating_abs)
  summary(m0_mating)
  plot_model(m0_mating, vline.color="darkred", show.values=TRUE, show.p=F); Anova(m0_mating)
  plot(allEffects(m0_mating))

  m0_mating_crs<-lm(log_testosterone ~ elot + Tenure + zCSI + DartingGroup, data=df_model_mating_c)
  summary(m0_mating_crs)
  plot_model(m0_mating_crs, vline.color="darkred", show.values=TRUE, show.p=F); Anova(m0_mating_crs)
  plot(allEffects(m0_mating_crs))

  # null models
  null_model_mating_abs<-lm(log_testosterone ~ elot + DartingGroup, data=df_model_mating_abs)
  anova(m0_mating, null_model_mating_abs)
{ summary(null_model_mating_abs)
  plot_model(null_model_mating_abs, vline.color="darkred", show.values=TRUE, show.p=F); Anova(null_model_mating_abs)
  plot(allEffects(null_model_mating_abs))

  # model checks look good
  res<-simulateResiduals(null_model_mating_abs)
  plot(res)
  testUniformity(res)
  testDispersion(res) }
  null_model_mating_c<-lm(log_testosterone ~ elot + DartingGroup, data=df_model_mating_c)
  anova(m0_mating_crs, null_model_mating_c)
{ summary(null_model_mating_c)
  plot_model(null_model_mating_c, vline.color="darkred", show.values=TRUE, show.p=F); Anova(null_model_mating_c)
  plot(allEffects(null_model_mating_c))

  # model checks look good
  res<-simulateResiduals(null_model_mating_c)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

  # service models
  alarm_model_mating_abs<-lm(log_testosterone ~ AlarmMP_prop + elot + DartingGroup, data=df_model_mating_abs)
  anova(null_model_mating_abs, alarm_model_mating_abs) # no addition
{ summary(alarm_model_mating_abs)
  plot_model(alarm_model_mating_abs, vline.color="darkred", show.values=TRUE, show.p=F); Anova(alarm_model_mating_abs)
  plot(allEffects(alarm_model_mating_abs))

  # model checks look good
  res<-simulateResiduals(alarm_model_mating_abs)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

  bge_model_mating_abs<-lm(log_testosterone ~ BGE_prop + elot + DartingGroup, data=df_model_mating_abs)
  anova(null_model_mating_abs, bge_model_mating_abs) # todo trend, but model not supported
{ summary(bge_model_mating_abs)
  plot_model(bge_model_mating_abs, vline.color="darkred", show.values=TRUE, show.p=F); Anova(bge_model_mating_abs)
  plot(allEffects(bge_model_mating_abs))
  plot(Effect("BGE_prop", bge_model_mating_abs))

  # model checks look good
  res<-simulateResiduals(bge_model_mating_abs)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

  vig_model_mating_abs<-lm(log_testosterone ~ Vig_prop + elot + DartingGroup, data=df_model_mating_abs)
  anova(null_model_mating_abs, vig_model_mating_abs) # no addition
{ summary(vig_model_mating_abs)
  plot_model(vig_model_mating_abs, vline.color="darkred", show.values=TRUE, show.p=F); Anova(vig_model_mating_abs)
  plot(allEffects(vig_model_mating_abs))

  # model checks look good
  res<-simulateResiduals(vig_model_mating_abs)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

  crs_model_mating_c<-lm(log_testosterone ~ Crs_prop + elot + DartingGroup, data=df_model_mating_c)
  anova(null_model_mating_c, crs_model_mating_c) # no addition
{ summary(crs_model_mating_c)
  plot_model(crs_model_mating_c, vline.color="darkred", show.values=TRUE, show.p=F); Anova(crs_model_mating_c)
  plot(allEffects(crs_model_mating_c))

  # model checks look good
  res<-simulateResiduals(crs_model_mating_c)
  plot(res)
  testUniformity(res)
  testDispersion(res) }


  ### BABY SEASON
  df_baby<-df %>% filter(DartingSeason == "dartingBaby")

  # model data
{ # abs stands for alarm, bgc (bge, between-group encounters and sentinelling (vigilance)
  df_model_baby_abs<-df_baby %>%
    dplyr::select(Testosterone, Cortisol, zCSI, elo12, Tenure, Father, DartingSeason,
                  AnimalCode, DartingGroup, Condition,
                  BGE_prop, AlarmMP_prop, Vig_prop, MaleCarrer, elot, rank_trajectory) %>%
    # scale all numeric variables
    mutate(
      log_testosterone=as.character(log(Testosterone + 1)),
      log_cortisol=as.character(log(Cortisol + 1))) %>%
    mutate(across(where(is.numeric), ~as.numeric(scale(.)))) %>%
    mutate(
      log_testosterone=as.numeric(log_testosterone),
      log_cortisol=as.numeric(log_cortisol),) %>%
    drop_na() %>%
    distinct()

  # c for crossing, there is one group AK that do not cross the river so we cannot calculate crossing behaviour for them
  df_model_baby_c<-df_baby %>%
    dplyr::select(Testosterone, Cortisol, zCSI, elo12, Tenure, Father, DartingSeason,
                  AnimalCode, DartingGroup, Condition,
                  BGE_prop, AlarmMP_prop, Vig_prop, MaleCarrer, elot, rank_trajectory,
                  Crs_prop) %>%
    # scale all numeric variables
    mutate(
      log_testosterone=as.character(log(Testosterone + 1)),
      log_cortisol=as.character(log(Cortisol + 1))) %>%
    mutate(across(where(is.numeric), ~as.numeric(scale(.)))) %>%
    mutate(
      log_testosterone=as.numeric(log_testosterone),
      log_cortisol=as.numeric(log_cortisol),) %>%
    drop_na() %>%
    distinct()

  nrow(df_model_baby_abs); nrow(df_model_baby_c)
} # 17 and 15 males each

  # data transformed to fit the normal distribution, ok fit
  plot(fitdist(df_model_baby_abs$log_testosterone, "norm"))
  plot(fitdist(df_model_baby_c$log_testosterone, "norm"))

  # exploratory model
  m0_baby<-lm(log_testosterone ~ elot + Tenure + zCSI + DartingGroup, data=df_model_baby_abs)
  summary(m0_baby)
  plot_model(m0_baby, vline.color="darkred", show.values=TRUE, show.p=F); Anova(m0_baby)
  plot(allEffects(m0_baby))

  m0_baby_c<-lm(log_testosterone ~ elot + Tenure + zCSI + DartingGroup, data=df_model_baby_c)
  summary(m0_baby_c)
  plot_model(m0_baby_c, vline.color="darkred", show.values=TRUE, show.p=F); Anova(m0_baby_c)
  plot(allEffects(m0_baby_c))


  # null models
  null_model_baby_abs<-lm(log_testosterone ~ elot + DartingGroup, data=df_model_baby_abs)
  anova(m0_baby, null_model_baby_abs)
{ summary(null_model_baby_abs)
  plot_model(null_model_baby_abs, vline.color="darkred", show.values=TRUE, show.p=F); Anova(null_model_baby_abs)
  plot(allEffects(null_model_baby_abs))

  # model checks look good
  res<-simulateResiduals(null_model_baby_abs)
  plot(res)
  testUniformity(res)
  testDispersion(res) }
  null_model_baby_c<-lm(log_testosterone ~ elot + DartingGroup, data=df_model_baby_c)
  anova(m0_baby_c, null_model_baby_c)
{ summary(null_model_baby_c)
  plot_model(null_model_baby_c, vline.color="darkred", show.values=TRUE, show.p=F); Anova(null_model_baby_c)
  plot(allEffects(null_model_baby_c))

  # model checks look good
  res<-simulateResiduals(null_model_baby_c)
  plot(res)
  testUniformity(res)
  testDispersion(res) } # todo terrible fit

  # service models
  alarm_model_baby_abs<-lm(log_testosterone ~ AlarmMP_prop + elot + DartingGroup, data=df_model_baby_abs)
  anova(null_model_baby_abs, alarm_model_baby_abs) # alarm is relevant in the baby season
{ summary(alarm_model_baby_abs)
  plot_model(alarm_model_baby_abs, vline.color="darkred", show.values=TRUE, show.p=F); Anova(alarm_model_baby_abs)
  plot(allEffects(alarm_model_baby_abs))

  # model checks look good
  res<-simulateResiduals(alarm_model_baby_abs)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

  bge_model_baby_abs<-lm(log_testosterone ~ BGE_prop + elot + DartingGroup, data=df_model_baby_abs)
  anova(null_model_baby_abs, bge_model_baby_abs) # todo no addition, model iffi
{ summary(bge_model_baby_abs)
  plot_model(bge_model_baby_abs, vline.color="darkred", show.values=TRUE, show.p=F); Anova(bge_model_baby_abs)
  plot(allEffects(bge_model_baby_abs))
  plot(Effect("BGE_prop", bge_model_baby_abs))

  # model checks look good
  res<-simulateResiduals(bge_model_baby_abs)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

  vig_model_baby_abs<-lm(log_testosterone ~ Vig_prop + elot + DartingGroup, data=df_model_baby_abs)
  anova(null_model_baby_abs, vig_model_baby_abs) # todo no addition, model iffi
{ summary(vig_model_baby_abs)
  plot_model(vig_model_baby_abs, vline.color="darkred", show.values=TRUE, show.p=F); Anova(vig_model_baby_abs)
  plot(allEffects(vig_model_baby_abs))

  # model checks look good
  res<-simulateResiduals(vig_model_baby_abs)
  plot(res)
  testUniformity(res)
  testDispersion(res) }

  crs_model_baby_c<-lm(log_testosterone ~ Crs_prop + elot + DartingGroup, data=df_model_baby_c)
  anova(null_model_baby_c, crs_model_baby_c) # no addition
{ summary(crs_model_baby_c)
  plot_model(crs_model_baby_c, vline.color="darkred", show.values=TRUE, show.p=F); Anova(crs_model_baby_c)
  plot(allEffects(crs_model_baby_c))

  # model checks look good
  res<-simulateResiduals(crs_model_baby_c)
  plot(res)
  testUniformity(res)
  testDispersion(res) }
}


