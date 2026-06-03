# ---------------
# Title: statistical analysis of MS-hair Cortisol
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

# no major effect of DartingSeason
# thus we will NOT model the data separated by seasons
ggplot(df, aes(x = DartingSeason, y = Cortisol, fill = DartingSeason, colour = DartingSeason)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(aes(shape = Condition ),size = 8, alpha = 0.8) +
  theme_classic() +
  labs(x = "Darting season", y = "Cortisol") +
  theme(legend.position = "right", text = element_text(size = 18))

df %>% group_by(DartingSeason) %>%
  summarize(mean = mean(Cortisol), sd = sd(Cortisol))

# modeled data
df_model <- df %>% dplyr::select(Cortisol, zCSI, elo12, Tenure, Father, DartingSeason, AnimalCode, DartingGroup,
                                     BGE_prop, AlarmMP_prop,Vig_prop, MaleCarrer, elot, rank_trajectory) %>%
   # scale all numeric variables
    mutate(log_cortisol = as.character(log(Cortisol ))) %>%
    mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>%
    mutate(log_cortisol = as.numeric(log_cortisol)) %>% drop_na() %>% distinct()

# data transformed to fit the normal distribution, ok fit
plot(fitdist(df_model$log_cortisol, "norm"))

# exploratory model
m0 <- lmer(log_cortisol ~ elot * DartingSeason + Tenure + zCSI + DartingGroup + (1|AnimalCode), data = df_model)
summary(m0)
Anova(m0); plot(allEffects(m0))
plot(simulateResiduals(m0))
print(standardized_effects(m0), n = Inf)     # effect sizes

  emtrends(m0, ~ DartingSeason, var = "elot") %>% summary(infer = TRUE)
  predC <- ggpredict(m0, terms = c("elot [all]", "DartingSeason")) %>% as.data.frame() # for plot
  #write.csv(predC, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predicted_elotSeason_C.csv", row.names = F)


null_model <- lmer(log_cortisol ~ elot + zCSI +DartingSeason + DartingGroup+ (1|AnimalCode), data = df_model)

anova(m0, null_model)
summary(null_model)
plot_model(null_model, vline.color = "darkred", show.values = TRUE, show.p = F); Anova(null_model)
plot(allEffects(null_model))
print(standardized_effects(null_model), n = Inf)     # effect sizes
plot(simulateResiduals(null_model))

# Condition and darting season were not included in the final models because these variables
# were partially confounded in the dataset (reshave samples occurred predominantly during the baby season),
# limiting independent estimation of their effects given the modest sample size.
# Including these controls increased model complexity substantially without qualitatively
# changing the direction of the main effects

# now lets test services
# ALARM, no effect
alarm_model <- lmer(log_cortisol ~ AlarmMP_prop + elot + zCSI + DartingSeason + DartingGroup +(1|AnimalCode),
           data = df_model)

anova(null_model,alarm_model)
Anova(alarm_model); summary(alarm_model)
print(standardized_effects(alarm_model), n = Inf)     # effect sizes
plot(allEffects(alarm_model))
ggplot(df_model, aes(x = AlarmMP_prop, y = Cortisol)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  ggpubr::stat_cor( label.x.npc = "left") +
  theme_classic()
predCalarm <- ggpredict(alarm_model, terms = "AlarmMP_prop") %>% as.data.frame() %>% mutate( Cortisol_pred = exp(predicted), Cortisol_low = exp(conf.low), Cortisol_high = exp(conf.high))
write.csv(predCalarm, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCalarm", row.names = F)

# BGEE, more BGE less cortisol
bge_model <- lmer(log_cortisol ~ BGE_prop + elot + zCSI + DartingSeason + DartingGroup +(1|AnimalCode),
           data = df_model)

anova(null_model,bge_model)
Anova(bge_model)
summary(bge_model)
print(standardized_effects(bge_model), n = Inf)     # effect sizes
plot(allEffects(bge_model))
plot(simulateResiduals(bge_model))

predCbge <- ggpredict(bge_model, terms = "BGE_prop") %>% as.data.frame() %>% mutate( Cortisol_pred = exp(predicted), Cortisol_low = exp(conf.low), Cortisol_high = exp(conf.high))
write.csv(predCbge, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCbge", row.names = F)

ggplot(df_model, aes(x = BGE_prop, y = log_cortisol,  colour = DartingSeason)) +
  geom_point(aes(shape= DartingSeason),size =6, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  ggpubr::stat_cor( label.x.npc = "left") +
  #scale_colour_viridis_c(option = "D", name = "elot") +
  theme_classic() +
  theme(legend.position = "right", text = element_text(size = 18))

# SENTINELLING, no effect
vig_model <- lmer(log_cortisol ~ Vig_prop + elot + zCSI + DartingSeason + DartingGroup +(1|AnimalCode),
                  data = df_model)

anova(null_model,vig_model)
Anova(vig_model); summary(vig_model)
print(standardized_effects(vig_model), n = Inf)     # effect sizes
plot(allEffects(vig_model))

predCvig <- ggpredict(vig_model, terms = "Vig_prop") %>% as.data.frame() %>% mutate( Cortisol_pred = exp(predicted), Cortisol_low = exp(conf.low), Cortisol_high = exp(conf.high))
write.csv(predCvig, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCvig", row.names = F)

ggplot(df_model, aes(x = Vig_prop, y = Cortisol, colour = elot)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  ggpubr::stat_cor( label.x.npc = "left") +
  theme_classic()

# CRS, excluding group AK
# data
df_model_crs <- df %>% dplyr::select(Cortisol, zCSI, elo12, Tenure, Father, DartingSeason, AnimalCode, DartingGroup, Condition,
                                     BGE_prop, AlarmMP_prop,Vig_prop, Crs_prop, MaleCarrer, elot, rank_trajectory) %>%
   # scale all numeric variables
    mutate(log_cortisol = as.character(log(Cortisol + 1))) %>%
    mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>%
    mutate(log_cortisol = as.numeric(log_cortisol)) %>% drop_na() %>% distinct()

# exploratory model
m0_c <- lmer(log_cortisol ~ elot * DartingSeason + Tenure + zCSI + DartingGroup + (1|AnimalCode), data = df_model_crs)
summary(m0_c)
Anova(m0_c)
plot(simulateResiduals(m0_c))
print(standardized_effects(m0_c), n = Inf)     # effect sizes
null_model_c <- lmer(log_cortisol ~ elot + DartingSeason + DartingGroup + (1|AnimalCode), data = df_model_crs)

anova(m0_c, null_model_c)
summary(null_model_c)
plot_model(null_model_c, vline.color = "darkred", show.values = TRUE, show.p = F); Anova(null_model_c)
plot(allEffects(null_model_c))
plot(simulateResiduals(null_model_c))
print(standardized_effects(null_model_c), n = Inf)     # effect sizes


crs_model <- lmer(log_cortisol ~ Crs_prop + elot + DartingSeason + DartingGroup +(1|AnimalCode),
           data = df_model_crs)

anova(null_model_c,crs_model)
Anova(crs_model); summary(crs_model)
print(standardized_effects(crs_model), n = Inf)     # effect sizes
plot(allEffects(crs_model))
plot(simulateResiduals(crs_model))

predCcrs <- ggpredict(crs_model, terms = "Crs_prop") %>% as.data.frame() %>% mutate( Cortisol_pred = exp(predicted), Cortisol_low = exp(conf.low), Cortisol_high = exp(conf.high))
write.csv(predCcrs, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCcrs", row.names = F)


ggplot(df_model_crs, aes(x = Crs_prop, y = Cortisol)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  ggpubr::stat_cor( label.x.npc = "left") +
  theme_classic()

# Basically iy seems that you can add the control variables or not. And then the story is, we tested for all this. We also investigated fathers and non-fathers in
# both cases and there is robust hints for male carrers but the sample size is not enough to be certain about this
# it seems that in both cases rank is really relevant for a males phyisology, specially when considering the trajectory
# but is intresting that ladies do not care about rank. On the other hand, BGC seems to be mediated by cortisol in where
# maybe only the males with nice psiotion /calmnes/health can provide such services and that is why females indeed choose them
# this puts to question once mrpe what is the benefit of boing a top male in vervets. becuase they definetly care about it!