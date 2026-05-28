# ---------------
# Title:
# Date: 
# Author: mgranellruiz
# Goal: 
# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')


# path ------------------------
setwd()

# data ------------------------

# data ------------------------
# hormone data of 2022 and 2023
horm_df_base <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/horm_df_base.csv")
# bring the male service data done with /Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/Scripts/MSIndex_automated.R
# equivalent to /Users/mariagranell/Repositories/male_services_index/MSpublication_beforecleanup/Scripts/MSIndex_automated.R the indvidual function and the com output
ms <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/MSIndex_individual_com_shortterm.csv")
# rank data
rank <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/ELO_dartingdate_maleservices_hair.csv") %>% rename(AnimalCode = IDIndividual1)
rankt <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/ELO_dartingdate_maleservices_hair_trial.csv") %>% rename(AnimalCode = IDIndividual1)

# csi data
csi <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/CSI_maleservices_hair.csv")


# combine df
df <- horm_df_base %>%
  #left_join(rank, by= c("AnimalCode", "Sex", "DartingGroup" = "Group", "DartingDate")) %>%
  left_join(rankt, by= c("AnimalCode", "DartingDate")) %>%
  mutate(DartingSeason = ifelse(DartingSeason == "darting2022", "dartingMating", "dartingBaby"),
         tracker = paste0(AnimalCode, DartingSeason)) %>%
  # select only adult males
  filter(Sex == "M") %>%
  left_join(.,lh %>% dplyr::select(AnimalCode, Tenure_type, Group_mb) %>% filter(Tenure_type== "BirthGroup"),
                                                              by = c("AnimalCode", "DartingGroup"="Group_mb")) %>%
  filter(is.na(Tenure_type)) %>% select(-Tenure_type) %>%
  # add if borned in IVP
  left_join(.,lh %>% dplyr::select(AnimalCode, BornedIVP = Tenure_type) %>% filter(BornedIVP== "BirthGroup"), by = "AnimalCode") %>%
  # add tenure leght to the group
  left_join(.,lh %>% dplyr::select(AnimalCode, StartDate_mb, EndDate_mb, Group_mb, DOB_estimate), by = c("AnimalCode", "DartingGroup"="Group_mb"), relationship = "many-to-many") %>%
  filter(StartDate_mb < DartingDate & EndDate_mb > DartingDate) %>%
  mutate(Tenure = as.numeric(difftime(DartingDate, StartDate_mb)), Age = add_age(DOB_estimate, DartingDate, "Years")#, elo = ELO, elo12 = ELO6
  ) %>%
  distinct() %>%
  left_join(.,ms, by = c("AnimalCode", "DartingDate")) %>%
  filter(!is.na(elo12))
  #pivot_wider(names_from = hormones, values_from = concentration, values_fn = ~ mean(.x, na.rm = TRUE)) %>% distinct()

# calculate first dates fathers
first_father_dates <- df %>%
  filter(Sex == "M", ) %>%
  distinct(AnimalCode, StartDate_mb, EndDate_mb) %>%
  mutate(FirstMatingSeason = case_when(
           month(StartDate_mb) <= 7 ~ ymd(paste0(year(StartDate_mb), "-03-01")),
           month(StartDate_mb) > 7 ~ ymd(paste0(year(StartDate_mb) + 1, "-03-01"))
         ),
         FirstBabySeason = ymd(paste0(year(FirstMatingSeason), "-10-01"))
  ) %>% distinct()

# calculate number of males to asses amount of male-male competition
n_males <- horm_df_base %>% dplyr::select(AnimalCode, DartingDate, DartingGroup) %>%
  left_join(.,lh %>% filter(Tenure_type != "BirthGroup", Sex == "M") %>% dplyr::select(Male = AnimalCode, StartDate_mb, EndDate_mb, Group_mb),
              by = c("DartingGroup" = "Group_mb"), relationship = "many-to-many") %>%
  mutate(across(contains("Date"), ymd)) %>%
  filter(StartDate_mb <= DartingDate, EndDate_mb >= DartingDate - months(3)) %>%
  group_by(AnimalCode, DartingDate, DartingGroup) %>%
  summarise(n_males = n_distinct(Male)) %>% mutate(DartingDate = as.character(DartingDate))

# add fathers and n_males
df <- left_join(df, first_father_dates, by =c("AnimalCode", "StartDate_mb", "EndDate_mb")) %>%
  mutate(Father = ifelse(DartingDate > FirstBabySeason, "Yes", "No"),
         MaleCarrer = case_when(
           elo12 > 0.9 & Father == "No" ~ "Dominant non-father",
           elo12 > 0.9 & Father == "Yes" ~ "Dominant potential father",
           elo12 < 0.9 & Father == "No" ~ "Subordinate non-father",
           elo12 < 0.9 & Father == "Yes" ~ "Subordinate potentinal-father",
         )) %>%
  left_join(., n_males, by=c("AnimalCode", "DartingDate", "DartingGroup")) %>%
  # add the rew rank
  mutate(elot = scale(elo12) - scale(rank_trajectory),
         topMale = ifelse(elo12 == 1, "Dominant", "Subordinate"),
         trajectory = case_when(
           rank_trajectory > 0 ~ "decreasing",
           rank_trajectory == 0 ~ "stable",
           rank_trajectory < 0 ~"increasing",
           TRUE ~ NA
         )
  )

# to calculate MS again
df_ms <- df %>% dplyr::select(-BGE_prop, -AlarmMP_prop, -Crs_prop, -Vig_prop)

ggplot(df, aes(x = DartingSeason, y = Testosterone, fill = DartingSeason, colour = DartingSeason)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(aes(shape = Condition), size = 8, alpha = 0.8) +
  theme_classic() +
  labs(x = "Darting season", y = "Testosterone") +
  theme(legend.position = "right", text = element_text(size = 18))

ggplot(df, aes(x = DartingDate, y = Testosterone, color = DartingSeason)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE)
elot<- ggplot(df, aes(x = elot, y = Testosterone, color = DartingSeason)) +
  geom_point(size = 8, alpha = 0.7, aes(shape = trajectory)) +
  geom_smooth(method = "lm", se = TRUE) + theme_classic() +
  theme(legend.position = "right", text = element_text(size = 18))
ggplot(df, aes(x = Age, y = Testosterone, color = DartingSeason)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  ggpubr::stat_cor(aes(group = DartingSeason), label.x.npc = "left") +
  theme_classic()
ggplot(df, aes(x = elot, y = Testosterone)) +
  geom_point(aes(shape = DartingGroup, colour = Father), alpha = 0.7, size = 9) +
  geom_smooth(aes(color = DartingSeason),method = "lm", se = TRUE) +
  ggpubr::stat_cor(aes(group = DartingSeason), label.x.npc = "left") +
  theme_classic()
ggplot(df, aes(x = elot, y = Testosterone)) +
  geom_point(aes(shape = topMale, colour = DartingSeason, size = -rank_trajectory), alpha = 0.7) +
  geom_smooth(aes(color = DartingSeason),method = "lm", se = TRUE) +
  ggpubr::stat_cor(aes(group = DartingSeason), label.x.npc = "left") +
  theme_classic()

df_model <- df %>% dplyr::select(Testosterone, Cortisol, elo12, Age, Tenure, Father, DartingSeason, AnimalCode, DartingGroup,
                                 BGE_prop, AlarmMP_prop,Vig_prop, MaleCarrer, elot, rank_trajectory,
                                 Crs_prop
) %>%
    mutate(
    log_testosterone = as.character(log(Testosterone +1)),
    log_cortisol = as.character(log(Cortisol + 1))
  ) %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>%
  mutate(
    log_testosterone = as.numeric(log_testosterone),
    log_cortisol = as.numeric(log_cortisol),
  )

m0 <- lmer(log_testosterone ~ (elo12 * Father) : DartingSeason + (1|AnimalCode),
           data = df_model)
m0 <- lmer(log_testosterone ~ elo12 * rank_trajectory * DartingSeason + (1|AnimalCode),
           data = df_model)
m0 <- lm(log_testosterone ~ (elo12 * Father),
           data = df_model %>% filter(DartingSeason == "dartingMating"))
m0 <- lmer(log_testosterone ~ elot * DartingSeason + (1|AnimalCode),
           data = df_model)
m0 <- lmer(log_testosterone ~ (BGE_prop * MaleCarrer) : DartingSeason + (1|AnimalCode),
           data = df_model)

Anova(m0)

plot(allEffects(m0))
plot(Effect(c("elo12","DartingSeason"), m0))
plot(Effect(c("elo12","Father"), m0))
plot(Effect(c("Father","DartingSeason"), m0))
plot(Effect(c("DartingSeason","elo12","Father"), m0))

ggplot(df_model, aes(x = elo12, y = Testosterone, color = Tenure)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  facet_wrap(~ DartingSeason) +
  scale_color_viridis_c() +
  theme_classic()

ggplot(df_model, aes(x = elot, y = Testosterone, color = Father)) +
  geom_point(size = 8) + scale_colour_manual(values = c(   "Yes" = "#1B9E77", "No"  = "#D95F02" ) ) +
  geom_smooth(method = "lm") +
  facet_wrap(~ DartingSeason) +
  theme_classic() + theme(legend.position="right", text=element_text(size=18))

ggplot(df, aes(x = Father, y = Testosterone, fill = Father, colour = Father)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(size = 8, alpha = 0.8) +
  theme_classic() +
  labs(x = "Potential Father", y = "Testosterone") +
  facet_wrap(~ DartingSeason) +
  theme(legend.position = "right", text = element_text(size = 18))

ggplot(df_model, aes(x = Crs_prop, y = Testosterone, color = elot)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  facet_wrap(~ DartingSeason) +
  theme_classic()

{
library(dplyr)
library(lme4)
library(car)
library(performance)
library(broom.mixed)
library(purrr)
library(tibble)

# Null/base model: rank-by-season only
m_null <- lmer(
  log_testosterone ~ elot * DartingSeason + (1 | AnimalCode),
  data = df_model,
  REML = FALSE
)

# Service models: add one male-service predictor at a time
m_bge <- lmer(
  log_testosterone ~ (elot * BGE_prop) : DartingSeason  + (1 | AnimalCode),
  data = df_model,
  REML = FALSE
)

  model <- m_crs
summary(model)
plot_model(model, vline.color = "darkred", show.values = TRUE, show.p = F); Anova(model)
plot(allEffects(model))

m_alarm <- lmer(
  log_testosterone ~ (elot*AlarmMP_prop) : DartingSeason + (1 | AnimalCode),
  data = df_model,
  REML = FALSE
)

m_crs <- lmer(
  log_testosterone ~ (elot * Crs_prop) : DartingSeason + (1 | AnimalCode),
  data = df_model,
  REML = FALSE
)

m_vig <- lmer(
  log_testosterone ~ (elot * Vig_prop) : DartingSeason + (1 | AnimalCode),
  data = df_model,
  REML = FALSE
)

models <- list(
  null = m_null,
  BGE = m_bge,
  Alarm = m_alarm,
  #Crossing = m_crs,
  Vigilance = m_vig
)

# Compare each service model against the null/base model
comparison_table <- bind_rows(
  lapply(names(models)[-1], function(x) {
    test <- anova(m_null, models[[x]])

    tibble(
      model = x,
      Chisq = test$Chisq[2],
      Df = test$`Chi Df`[2],
      p_value = test$`Pr(>Chisq)`[2],
      AIC_null = AIC(m_null),
      AIC_model = AIC(models[[x]]),
      delta_AIC = AIC(models[[x]]) - AIC(m_null),
      BIC_null = BIC(m_null),
      BIC_model = BIC(models[[x]]),
      R2_marginal = performance::r2(models[[x]])$R2_marginal,
      R2_conditional = performance::r2(models[[x]])$R2_conditional
    )
  })
) %>%
  arrange(AIC_model)

comparison_table
}

# Type-II Wald tests for each model
anova_table <- bind_rows(
  lapply(names(models), function(x) {
    car::Anova(models[[x]], type = 2) %>%
      as.data.frame() %>%
      rownames_to_column("term") %>%
      mutate(model = x)
  })
) %>%
  dplyr::select(model, term, Chisq, Df, `Pr(>Chisq)`)

boxplot(AlarmMP_prop ~ DartingSeason, data = df_model)
boxplot(Vig_prop ~ DartingSeason, data = df_model)
 boxplot(BGE_prop ~ DartingSeason, data = df_model)

##### LETS FIT AN INVESTIGATE ALL MODELS YEY! ######
df_model_abs <- df_model %>% dplyr::select(Testosterone, Cortisol, elo12, Age, Tenure, Father, DartingSeason, AnimalCode, DartingGroup,
                                 BGE_prop, AlarmMP_prop,Vig_prop, MaleCarrer, elot, rank_trajectory)
df_model_c <- df_model %>% dplyr::select(Testosterone, Cortisol, elo12, Age, Tenure, Father, DartingSeason, AnimalCode, DartingGroup,
                                 BGE_prop, AlarmMP_prop,Vig_prop, Crs_prop, MaleCarrer, elot, rank_trajectory) %>%
  mutate(Crs_prop = ifelse(DartingGroup == "AK", NA, Crs_prop)) %>%
  drop_na() %>% distinct()
table(df_model_abs$DartingSeason)
table(df_model_c$DartingSeason)

# MATING
null_mating_abs <- lm(Testosterone ~ elo12 + DartingGroup,
                  data = df_model_abs %>% filter(DartingSeason == "dartingMating"))
#plot(simulateResiduals(null_mating))
#plot_model(null_mating, vline.color = "darkred", show.values = TRUE, show.p = F);
summary(null_mating_abs); Anova(null_mating_abs)
print(standardized_effects(null_mating_abs), n = Inf)
#plot(allEffects(null_mating))

alarm_mating <- lm(Testosterone ~ (elot * AlarmMP_prop) + DartingGroup,
                  data = df_model_abs %>% filter(DartingSeason == "dartingMating"))
#plot(simulateResiduals(alarm_mating))
#plot_model(alarm_mating, vline.color = "darkred", show.values = TRUE, show.p = F);
summary(alarm_mating);Anova(alarm_mating); print(standardized_effects(alarm_mating), n = Inf)
anova(null_mating_abs, alarm_mating); AIC(null_mating_abs, alarm_mating)

bge_mating <- lm(Testosterone ~ (elot * BGE_prop) + DartingGroup,
                  data = df_model_abs %>% filter(DartingSeason == "dartingMating"))
#plot(simulateResiduals(bge_mating))
#plot_model(bge_mating, vline.color = "darkred", show.values = TRUE, show.p = F)
summary(bge_mating); Anova(bge_mating); print(standardized_effects(bge_mating), n = Inf)
anova(null_mating_abs, bge_mating); AIC(null_mating_abs, bge_mating)

vig_mating <- lm(Testosterone ~ (elot * Vig_prop) + DartingGroup,
                  data = df_model_abs %>% filter(DartingSeason == "dartingMating"))
#plot(simulateResiduals(vig_mating))
#plot_model(vig_mating, vline.color = "darkred", show.values = TRUE, show.p = F)
summary(vig_mating); Anova(vig_mating); print(standardized_effects(vig_mating), n = Inf)
anova(null_mating, vig_mating); AIC(null_mating, vig_mating)

null_mating_c <- lm(Testosterone ~ elot + DartingGroup,
                  data = df_model_c %>% filter(DartingSeason == "dartingMating"))
#plot(simulateResiduals(null_mating))
#plot_model(null_mating, vline.color = "darkred", show.values = TRUE, show.p = F);
summary(null_mating_c); Anova(null_mating_c)
print(standardized_effects(null_mating_c), n = Inf)
#plot(allEffects(null_mating))

crs_mating <- lm(Testosterone ~ (elot * Crs_prop) + DartingGroup,
                  data = df_model_c %>% filter(DartingSeason == "dartingMating"))
#plot(simulateResiduals(crs_mating))
#plot_model(crs_mating, vline.color = "darkred", show.values = TRUE, show.p = F)
summary(crs_mating); Anova(crs_mating); print(standardized_effects(crs_mating), n = Inf)
anova(null_mating_c, crs_mating); AIC(null_mating_c, crs_mating)

# BABY
null_baby_abs <- lm(Testosterone ~ elot + DartingGroup,
                  data = df_model_abs %>% filter(DartingSeason == "dartingBaby"))
#plot(simulateResiduals(null_baby_abs))
#plot_model(null_baby_abs, vline.color = "darkred", show.values = TRUE, show.p = F)
summary(null_baby_abs); Anova(null_baby_abs); print(standardized_effects(null_baby_abs), n = Inf)
#plot(allEffects(null_baby_abs))

alarm_baby <- lm(Testosterone ~ (elot * AlarmMP_prop) + DartingGroup,
                  data = df_model_abs %>% filter(DartingSeason == "dartingBaby"))
#plot(simulateResiduals(alarm_baby))
#plot_model(alarm_baby, vline.color = "darkred", show.values = TRUE, show.p = F)
summary(alarm_baby); Anova(alarm_baby); print(standardized_effects(alarm_baby), n = Inf)
anova(null_baby_abs, alarm_baby); AIC(null_baby_abs, alarm_baby)
#plot(allEffects(alarm_baby))

bge_baby <- lm(Testosterone ~ (elot * BGE_prop) + DartingGroup,
                  data = df_model_abs %>% filter(DartingSeason == "dartingBaby"))
#plot(simulateResiduals(bge_baby))
#plot_model(bge_baby, vline.color = "darkred", show.values = TRUE, show.p = F)
summary(bge_baby); Anova(bge_baby); print(standardized_effects(bge_baby), n = Inf)
anova(null_baby_abs, bge_baby); AIC(null_baby_abs, bge_baby)

vig_baby <- lm(Testosterone ~ (elot * Vig_prop) + DartingGroup,
                  data = df_model_abs %>% filter(DartingSeason == "dartingBaby"))
#plot(simulateResiduals(vig_baby))
#plot_model(vig_baby, vline.color = "darkred", show.values = TRUE, show.p = F)
summary(vig_baby); Anova(vig_baby); print(standardized_effects(vig_baby), n = Inf)
anova(null_baby_abs, vig_baby); AIC(null_baby_abs, vig_baby)

null_baby_c <- lm(Testosterone ~ elot + DartingGroup,
                  data = df_model_c %>% filter(DartingSeason == "dartingBaby"))
#plot(simulateResiduals(null_baby_c))
#plot_model(null_baby_c, vline.color = "darkred", show.values = TRUE, show.p = F)
summary(null_baby_c); Anova(null_baby_c); print(standardized_effects(null_baby_c), n = Inf)
#plot(allEffects(null_baby_c))

crs_baby <- lm(Testosterone ~ (elot * Crs_prop) + DartingGroup,
                  data = df_model_c %>% filter(DartingSeason == "dartingBaby"))
#plot(simulateResiduals(crs_baby))
#plot_model(crs_baby, vline.color = "darkred", show.values = TRUE, show.p = F)
summary(crs_baby); Anova(crs_baby); print(standardized_effects(crs_baby), n = Inf)
anova(null_baby_c, crs_baby); AIC(null_baby_c, crs_baby)

# plots
{ aa<-ggplot(df_model_abs, aes(x=elot, y=Testosterone, colour=DartingSeason)) +
  geom_point(aes(shape=DartingGroup), size=8, alpha=0.8) +
  geom_smooth(method="lm") +
  scale_shape_manual(
    values=c(
      "AK"=16,  # filled circle
      "BD"=17,  # filled triangle
      "KB"=15,  # filled square
      "LT"=18,  # filled diamond
      "NH"=8    # star
    )
  ) +
  theme_classic() +
  labs(x="Darting season", y="Testosterone", title="Crossing dataset") +
  theme_classic() +
  theme(legend.position="right", text=element_text(size=18))
  bb<-ggplot(df_model_c, aes(x=elot, y=Testosterone, colour=DartingSeason)) +
    geom_point(aes(shape=DartingGroup), size=8, alpha=0.8) +
    geom_smooth(method="lm") +
    scale_shape_manual(
      values=c(
        #"AK"=16,  # filled circle
        "BD"=17,  # filled triangle
        "KB"=15,  # filled square
        "LT"=18,  # filled diamond
        "NH"=8    # star
      )
    ) +
    theme_classic() +
    labs(x="Darting season", y="Testosterone", title="Crossing dataset") +
    theme_classic() +
    theme(legend.position="right", text=element_text(size=18))

  aa + bb + plot_annotation(tag_levels="A") } # elot and group

{a <- ggplot(df_model_abs, aes(x = AlarmMP_prop, y = Testosterone, colour = DartingSeason)) +
  geom_point( size = 8, alpha = 0.8) +
  geom_smooth(method = "lm")+
  theme_classic() +
  labs(x = "Alarm proportion", y = "Testosterone", title = "All dataset - Alarm") + theme_classic() +
  theme(legend.position = "right", text = element_text(size = 18))

b <- ggplot(df_model_abs, aes(x = BGE_prop, y = Testosterone, colour = DartingSeason)) +
  geom_point( size = 8, alpha = 0.8) +
  geom_smooth(method = "lm")+
  theme_classic() +
  labs(x = "BGC proportion", y = "Testosterone", title = "All dataset - BGC") + theme_classic() +
  theme(legend.position = "right", text = element_text(size = 18))

c <- ggplot(df_model_c, aes(x = Crs_prop, y = Testosterone, colour = DartingSeason)) +
  geom_point( size = 8, alpha = 0.8) +
  geom_smooth(method = "lm")+
  theme_classic() +
  labs(x = "Crosing proportion", y = "Testosterone", title = "Crs dataset - Crossing") + theme_classic() +
  theme(legend.position = "right", text = element_text(size = 18))

s <- ggplot(df_model_abs, aes(x = Vig_prop, y = Testosterone, colour = DartingSeason)) +
  geom_point( size = 8, alpha = 0.8) +
  geom_smooth(method = "lm")+
  theme_classic() +
  labs(x = "Sentinelling proportion", y = "Testosterone", title = "All dataset - Sentinelling") + theme_classic() +
  theme(legend.position = "right", text = element_text(size = 18))

a + b+ c +s + plot_annotation(tag_levels = "A")} # services

{ a<-ggplot(df_model_abs %>% filter(DartingSeason == "dartingBaby"),
            aes(x=AlarmMP_prop, y=Testosterone)) +
  geom_smooth(method="lm") +
  geom_point(aes(colour=elot), size=8, alpha=0.8) +
  scale_colour_viridis_c(option="D") +
  theme_classic() +
  labs(x="Alarm proportion", y="Testosterone", title="All dataset baby season - Alarm") +
  theme_classic() +
  theme(legend.position="right", text=element_text(size=18))
  b<-ggplot(df_model_abs %>% filter(DartingSeason == "dartingBaby"),
            aes(x=BGE_prop, y=Testosterone)) +
    geom_smooth(method="lm") +
    geom_point(aes(colour=elot), size=8, alpha=0.8) +
    scale_colour_viridis_c(option="D") +
    theme_classic() +
    labs(x="BGC proportion", y="Testosterone", title="All dataset baby season - BGC") +
    theme_classic() +
    theme(legend.position="right", text=element_text(size=18))
  a + b + plot_annotation(tag_levels="A") }

