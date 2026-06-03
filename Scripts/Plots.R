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
# plotting
library(patchwork)
library(ggplot2)
library(ggside)
library(ggpubr)
library(gridExtra)
library(ggtext)

# path ------------------------
setwd("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/Scripts")

# colours ---------------------
# nice yellow = #fcba03
baby = "#03befc"; mating = "#fc034e"
#baby   = "#F781BF"; mating = "#08306B"

point_size = 6
font_size = 18

# data ------------------------
df <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/HairHormonesMSdf.csv") %>%
  mutate(DartingSeason = str_remove(DartingSeason, "darting"))

# seasons and hormones
df %>% group_by(DartingSeason) %>%
  summarize(meanT = mean(Testosterone), sdT = sd(Testosterone), meanC = mean(Cortisol), sdC = sd(Cortisol))

tseason <- ggplot(df, aes(x = DartingSeason, y = Testosterone, fill = DartingSeason, colour = DartingSeason)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(size = 8, alpha = 0.8) +
  theme_classic() +
  scale_fill_manual(values = c("Baby" = baby, "Mating" = mating)) +
  scale_colour_manual(values = c("Baby" = baby, "Mating" = mating)) +
  labs(x = "Darting season", y = "Testosterone (ng/g)") +
  theme(legend.position = "none", text = element_text(size = 18))

cseason <- ggplot(df, aes(x = DartingSeason, y = Cortisol, fill = DartingSeason, colour = DartingSeason)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter( size = 8, alpha = 0.8) +
  scale_fill_manual(values = c("Baby" = baby, "Mating" = mating)) +
  scale_colour_manual(values = c("Baby" = baby, "Mating" = mating)) +
  theme_classic() +
  labs(x = "Darting season", y = "Cortisol (ng/g)") +
  theme(legend.position = "none", text = element_text(size = 18))

tseason + cseason + plot_annotation(tag_levels = "A")


# as a trend with elot raw data
tseason <- ggplot(df, aes(x = elot, y = Testosterone, colour = DartingSeason)) +
  geom_point(alpha = 0.7, size = point_size) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  #scale_fill_manual(values = c("Baby" = baby, "Mating" = mating)) +
  scale_colour_manual(values = c("Baby" = baby, "Mating" = mating)) +
  labs(x = "Competitive status", y = "Testosterone (ng/g)") +
  theme(legend.position = "none", text = element_text(size = font_size))

cseason <- ggplot(df, aes(x = elot, y = Cortisol, colour = DartingSeason)) +
  geom_point(alpha = 0.7, size = point_size) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  scale_colour_manual(values = c("Baby" = baby, "Mating" = mating)) +
  labs(x = "Competitive status", y = "Cortisol (ng/g)") +
  theme(legend.position = "right", text = element_text(size = font_size))

tseason + cseason + plot_annotation(tag_levels = "A")

# as a trend with elot model
predT <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predicted_elotSeason_T.csv") %>%
  as.data.frame() %>% mutate( Testosterone_pred = exp(predicted), Testosterone_low = exp(conf.low), Testosterone_high = exp(conf.high))
predC <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predicted_elotSeason_C.csv") %>%
  as.data.frame() %>% mutate( Cortisol_pred = exp(predicted), Cortisol_low = exp(conf.low), Cortisol_high = exp(conf.high))

df_model <- df %>% dplyr::select(Testosterone, Cortisol, zCSI, elo12, Tenure, Father, DartingSeason, AnimalCode, DartingGroup, BGE_prop, AlarmMP_prop, Vig_prop, MaleCarrer, elot, rank_trajectory ) %>%
  mutate( Testosterone_raw = Testosterone, log_testosterone = log(Testosterone), Cortisol_raw = Cortisol, log_cortisol = log(Cortisol) ) %>%
  mutate(across( where(is.numeric) & !c(Testosterone_raw, log_testosterone, Cortisol_raw, log_cortisol), ~ as.numeric(scale(.)))) %>%
  drop_na() %>% distinct()

tseason <- ggplot() +
  geom_point(data = df_model, aes(x = elot, y = Testosterone_raw, colour = DartingSeason), alpha = 0.7, size = point_size) +
  geom_ribbon(data = predT, aes(x = x, ymin = Testosterone_low, ymax = Testosterone_high, fill = group), alpha = 0.2, colour = NA) +
  geom_line(data = predT, aes(x = x, y = Testosterone_pred, colour = group), linewidth = 1.5) +
  theme_classic() +
  annotate("text", x = 0.5, y = 10, label = "***", size = 10) +
  scale_colour_manual(values = c("Baby" = baby, "Mating" = mating)) +
  scale_fill_manual(values = c("Baby" = baby, "Mating" = mating)) +
  labs(x = "Competitive status", y = "Testosterone (ng/g)") +
  theme(legend.position = "none", text = element_text(size = font_size))

cseason <- ggplot() +
  geom_point(data = df_model, aes(x = elot, y = Cortisol_raw, colour = DartingSeason), alpha = 0.7, size = point_size) +
  geom_ribbon(data = predC, aes(x = x, ymin = Cortisol_low, ymax = Cortisol_high, fill = group), alpha = 0.2, colour = NA) +
  geom_line(data = predC, aes(x = x, y = Cortisol_pred, colour = group), linewidth = 1.5) +
  theme_classic() +
  annotate("text", x = 0.5, y = 90, label = "**", size = 10) +
  scale_colour_manual(name = "Darting season", values = c("Baby" = baby, "Mating" = mating)) +
  scale_fill_manual(name = "Darting season", values = c("Baby" = baby, "Mating" = mating)) +
  labs(x = "Competitive status", y = "Cortisol (ng/g)", leyend = "Darting season") +
  theme(legend.position = "top", text = element_text(size = font_size))

tseason + cseason + plot_annotation(tag_levels = "A")


#### MALE SERVICES
{col_alarm = "#F5AF4DFF"; col_alarm_light = "#F5AF4D"; col_alarm_dark = "#DA710A"
col_bge = "#DB4743FF"; col_bge_light = "#E26C69"; col_bge_dark = "#761917"
col_vig =  "#7C873EFF"; col_vig_light = "#9CAA4E"; col_vig_dark = "#556D31"
col_crs =  "#5495CFFF"; col_crs_light = "#629DD3"; col_crs_dark = "#2659A6"} # colors
predTalarm <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTalarm") %>% rename_with(~ gsub("Cortisol", "Testosterone", .x), contains("Cortisol"))
predTbge <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTbge")%>% rename_with(~ gsub("Cortisol", "Testosterone", .x), contains("Cortisol"))
predTvig <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTvig")%>% rename_with(~ gsub("Cortisol", "Testosterone", .x), contains("Cortisol"))
predTcrs <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTcrs")%>% rename_with(~ gsub("Cortisol", "Testosterone", .x), contains("Cortisol"))
predCalarm <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCalarm")
predCbge <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCbge")
predCvig <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCvig")
predCcrs <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCcrs")

df_model <- df %>% dplyr::select(Testosterone, Cortisol, zCSI, elo12, Tenure, Father, DartingSeason, AnimalCode, DartingGroup, BGE_prop, Vig_prop, MaleCarrer, elot, rank_trajectory ) %>%
  mutate( Testosterone_raw = Testosterone, log_testosterone = log(Testosterone), Cortisol_raw = Cortisol, log_cortisol = log(Cortisol) ) %>%
  mutate(across( where(is.numeric) & !c(Testosterone_raw, log_testosterone, Cortisol_raw, log_cortisol), ~ as.numeric(scale(.)))) %>%
  drop_na() %>% distinct()
df_model_c <- df %>% dplyr::select(Testosterone, Cortisol, zCSI, elo12, Tenure, Father, DartingSeason, AnimalCode, DartingGroup, Crs_prop, BGE_prop, Vig_prop, MaleCarrer, elot, rank_trajectory ) %>%
  mutate( Testosterone_raw = Testosterone, log_testosterone = log(Testosterone), Cortisol_raw = Cortisol, log_cortisol = log(Cortisol) ) %>%
  mutate(across( where(is.numeric) & !c(Testosterone_raw, log_testosterone, Cortisol_raw, log_cortisol), ~ as.numeric(scale(.)))) %>%
  drop_na() %>% distinct()

{ alarmC<-ggplot() +
  geom_ribbon(data=predCalarm, aes(x=x, ymin=Cortisol_low, ymax=Cortisol_high), alpha=0.2, fill=col_alarm_dark) +
  geom_line(data=predCalarm, aes(x=x, y=Cortisol_pred), colour=col_alarm_dark, linewidth=1.5) +
  geom_point(data=df_model, aes(x=AlarmMP_prop, y=Cortisol_raw), alpha=0.7, size=point_size, colour=col_alarm_light) +
  theme_classic() +
  labs(x="Alarm", y="Cortisol (ng/g)") +
  theme(legend.position="none", text=element_text(size=font_size))

  bgeC<-ggplot() +
    geom_ribbon(data=predCbge, aes(x=x, ymin=Cortisol_low, ymax=Cortisol_high), alpha=0.2, fill=col_bge_dark) +
    geom_line(data=predCbge, aes(x=x, y=Cortisol_pred), linewidth=1.5, colour=col_bge_dark) +
    geom_point(data=df_model, aes(x=BGE_prop, y=Cortisol_raw), alpha=0.7, size=point_size, colour=col_bge_light) +
    theme_classic() +
    labs(x="BGC", y="Cortisol (ng/g)") +
    annotate("text", x=1, y = 60, label="*", size=font_size) +
    theme(legend.position="none", text=element_text(size=font_size))

  vigC<-ggplot() +
    geom_ribbon(data=predCvig, aes(x=x, ymin=Cortisol_low, ymax=Cortisol_high), fill=col_vig_dark, alpha=0.2, colour=NA) +
    geom_line(data=predCvig, aes(x=x, y=Cortisol_pred), colour=col_vig_dark, linewidth=1.5) +
    geom_point(data=df_model, aes(x=Vig_prop, y=Cortisol_raw), alpha=0.7, size=point_size, colour=col_vig_light) +
    theme_classic() +
    labs(x="Sentinelling", y="Cortisol (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  crsC<-ggplot() +
    geom_ribbon(data=predCcrs, aes(x=x, ymin=Cortisol_low, ymax=Cortisol_high), fill=col_crs_dark, alpha=0.2, colour=NA) +
    geom_line(data=predCcrs, aes(x=x, y=Cortisol_pred), colour=col_crs_dark, linewidth=1.5) +
    geom_point(data=df_model_c, aes(x=Crs_prop, y=Cortisol_raw), colour=col_crs_light, alpha=0.7, size=point_size) +
    theme_classic() +
    labs(x="Crossing", y="Cortisol (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  alarmT<-ggplot() +
    geom_ribbon(data=predTalarm, aes(x=x, ymin=Testosterone_low, ymax=Testosterone_high), fill=col_alarm_dark, alpha=0.2, colour=NA) +
    geom_line(data=predTalarm, aes(x=x, y=Testosterone_pred), colour=col_alarm_dark, linewidth=1.5) +
    geom_point(data=df_model, aes(x=AlarmMP_prop, y=Testosterone_raw), alpha=0.7, size=point_size, colour=col_alarm_light) +
    theme_classic() +
    labs(x="Alarm", y="Testosterone (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  bgeT<-ggplot() +
    geom_ribbon(data=predTbge, aes(x=x, ymin=Testosterone_low, ymax=Testosterone_high), fill=col_bge_dark, alpha=0.2, colour=NA) +
    geom_line(data=predTbge, aes(x=x, y=Testosterone_pred), colour=col_bge_dark, linewidth=1.5) +
    geom_point(data=df_model, aes(x=BGE_prop, y=Testosterone_raw), colour=col_bge_light, alpha=0.7, size=point_size) +
    theme_classic() +
    annotate("text", x=1, y=10, label=".", size=font_size) +
    labs(x="BGC", y="Testosterone (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  vigT<-ggplot() +
    geom_ribbon(data=predTvig, aes(x=x, ymin=Testosterone_low, ymax=Testosterone_high), fill=col_vig_dark, alpha=0.2, colour=NA) +
    geom_line(data=predTvig, aes(x=x, y=Testosterone_pred), colour=col_vig_dark, linewidth=1.5) +
    geom_point(data=df_model, aes(x=Vig_prop, y=Testosterone_raw), alpha=0.7, size=point_size, colour=col_vig_light) +
    theme_classic() +
    labs(x="Sentinelling", y="Testosterone (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  crsT<-ggplot() +
    geom_ribbon(data=predTcrs, aes(x=x, ymin=Testosterone_low, ymax=Testosterone_high), fill=col_crs_dark, alpha=0.2, colour=NA) +
    geom_line(data=predTcrs, aes(x=x, y=Testosterone_pred), colour=col_crs_dark, linewidth=1.5) +
    geom_point(data=df_model_c, aes(x=Crs_prop, y=Testosterone_raw), alpha=0.7, size=point_size, colour=col_crs_light) +
    theme_classic() +
    annotate("text", x=1, y=10, label=".", size=font_size) +
    labs(x="Crossing", y="Testosterone (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  wrap_plots(
    alarmT, bgeT, vigT, crsT,
    alarmC, bgeC, vigC, crsC,
    nrow=2,
    ncol=4,
    guides="collect"
  ) +
    plot_annotation(tag_levels="A") }
