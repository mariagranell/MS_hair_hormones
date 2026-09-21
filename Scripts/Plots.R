# ---------------
# Title: Plots
# Date: 20-may
# Author: mgranellruiz
# Goal: Create the plots to visualize the fundings
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
baby = "#03befc"; mating = "#fc034e"

point_size = 6
font_size = 18

# data ------------------------
df <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/HairHormonesMSdf.csv") %>%
  mutate(DartingSeason = str_remove(DartingSeason, "darting"))

summary(df$rank_trajectory)

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
  scale_colour_manual(name = "Darting period", values = c("Baby" = baby, "Mating" = mating), labels = c("Baby" = "Winter", "Mating" = "Mating")) +
  scale_fill_manual(name = "Darting period", values = c("Baby" = baby, "Mating" = mating), labels = c("Baby" = "Winter", "Mating" = "Mating")) +
  labs(x = "Competitive status", y = "Testosterone (ng/g)") +
  theme(legend.position = "top", text = element_text(size = font_size))

cseason <- ggplot() +
  geom_point(data = df_model, aes(x = elot, y = Cortisol_raw, colour = DartingSeason), alpha = 0.7, size = point_size) +
  geom_ribbon(data = predC, aes(x = x, ymin = Cortisol_low, ymax = Cortisol_high, fill = group), alpha = 0.2, colour = NA) +
  geom_line(data = predC, aes(x = x, y = Cortisol_pred, colour = group), linewidth = 1.5) +
  theme_classic() +
  annotate("text", x = 0.5, y = 90, label = "**", size = 10) +
  scale_colour_manual(name = "Darting period", values = c("Baby" = baby, "Mating" = mating),labels = c("Baby" = "Winter", "Mating" = "Mating")) +
  scale_fill_manual(name = "Darting period", values = c("Baby" = baby, "Mating" = mating),labels = c("Baby" = "Winter", "Mating" = "Mating")) +
  labs(x = "Competitive status", y = "Cortisol (ng/g)", leyend = "Darting period") +
  theme(legend.position = "none", text = element_text(size = font_size))

tseason + cseason + plot_annotation(tag_levels = "A")

#### MALE SERVICES
{col_alarm = "#F5AF4DFF"; col_alarm_light = "#F5AF4D"; col_alarm_dark = "#DA710A"
col_bge = "#DB4743FF"; col_bge_light = "#E26C69"; col_bge_dark = "#761917"
col_vig =  "#7C873EFF"; col_vig_light = "#9CAA4E"; col_vig_dark = "#556D31"
col_crs =  "#5495CFFF"; col_crs_light = "#629DD3"; col_crs_dark = "#2659A6"} # colors
line_baby = "longdash"; line_mating = "dotted"; method_line = "lm"
predTalarm <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTalarm") %>% rename_with(~ gsub("Cortisol", "Testosterone", .x), contains("Cortisol"))
predTbge <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTbge")%>% rename_with(~ gsub("Cortisol", "Testosterone", .x), contains("Cortisol"))
predTvig <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTvig")%>% rename_with(~ gsub("Cortisol", "Testosterone", .x), contains("Cortisol"))
predTcrs <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predTcrs")%>% rename_with(~ gsub("Cortisol", "Testosterone", .x), contains("Cortisol"))
predCalarm <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCalarm")
predCbge <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCbge")
predCvig <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCvig")
predCcrs <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/predCcrs")

df_model <- df %>% dplyr::select(Testosterone, Cortisol, DartingSeason, AlarmMP_prop, BGE_prop, Vig_prop) %>%
  mutate( Testosterone_raw = Testosterone, log_testosterone = log(Testosterone), Cortisol_raw = Cortisol, log_cortisol = log(Cortisol) ) %>%
  mutate(across( where(is.numeric) & !c(Testosterone_raw, log_testosterone, Cortisol_raw, log_cortisol), ~ as.numeric(scale(.)))) %>%
  drop_na() %>% distinct()
df_model_c <- df %>% dplyr::select(Testosterone, Cortisol, DartingSeason, Crs_prop) %>%
  mutate( Testosterone_raw = Testosterone, log_testosterone = log(Testosterone), Cortisol_raw = Cortisol, log_cortisol = log(Cortisol) ) %>%
  mutate(across( where(is.numeric) & !c(Testosterone_raw, log_testosterone, Cortisol_raw, log_cortisol), ~ as.numeric(scale(.)))) %>%
  drop_na() %>% distinct()

{ alarmC<-ggplot() +
  geom_ribbon(data=predCalarm, aes(x=x, ymin=Cortisol_low, ymax=Cortisol_high), alpha=0.2, fill=col_alarm_dark) +
  geom_line(data=predCalarm, aes(x=x, y=Cortisol_pred), colour=col_alarm_dark, linewidth=1.5) +
  geom_point(data=df_model, aes(x=AlarmMP_prop, y=Cortisol_raw), alpha=0.7, size=point_size, colour=col_alarm_light) +
  #geom_smooth(data = df_model, aes(x = AlarmMP_prop, y = Cortisol_raw, linetype = DartingSeason),
  #  colour = col_alarm_dark, linewidth = 1.5, method = method_line, se = FALSE) +
  #scale_linetype_manual(values = c("Baby" = line_baby, "Mating" = line_mating)) +
  theme_classic() +
  labs(x="Alarm", y="Cortisol (ng/g)") +
  theme(legend.position="none", text=element_text(size=font_size))

  bgeC<-ggplot() +
    geom_ribbon(data=predCbge, aes(x=x, ymin=Cortisol_low, ymax=Cortisol_high), alpha=0.2, fill=col_bge_dark) +
    geom_line(data=predCbge, aes(x=x, y=Cortisol_pred), linewidth=1.5, colour=col_bge_dark) +
    geom_point(data=df_model, aes(x=BGE_prop, y=Cortisol_raw), alpha=0.7, size=point_size, colour=col_bge_light) +
    #geom_smooth(data = df_model, aes(x = BGE_prop, y = Cortisol_raw, linetype = DartingSeason),
    #colour = col_bge_dark, linewidth = 1.5, method = method_line, se = FALSE) +
    #scale_linetype_manual(values = c("Baby" = line_baby, "Mating" = line_mating)) +
    theme_classic() +
    labs(x="BGC", y="Cortisol (ng/g)") +
    annotate("text", x=1, y = 60, label="*", size=font_size) +
    theme(legend.position="none", text=element_text(size=font_size))

  vigC<-ggplot() +
    geom_ribbon(data=predCvig, aes(x=x, ymin=Cortisol_low, ymax=Cortisol_high), fill=col_vig_dark, alpha=0.2, colour=NA) +
    geom_line(data=predCvig, aes(x=x, y=Cortisol_pred), colour=col_vig_dark, linewidth=1.5) +
    geom_point(data=df_model, aes(x=Vig_prop, y=Cortisol_raw), alpha=0.7, size=point_size, colour=col_vig_light) +
    #geom_smooth(data = df_model, aes(x = Vig_prop, y = Cortisol_raw, linetype = DartingSeason),
    #colour = col_vig_dark, linewidth = 1.5, method = method_line, se = FALSE) +
    #scale_linetype_manual(values = c("Baby" = line_baby, "Mating" = line_mating)) +
    theme_classic() +
    labs(x="Sentinelling", y="Cortisol (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  crsC<-ggplot() +
    geom_ribbon(data=predCcrs, aes(x=x, ymin=Cortisol_low, ymax=Cortisol_high), fill=col_crs_dark, alpha=0.2, colour=NA) +
    geom_line(data=predCcrs, aes(x=x, y=Cortisol_pred), colour=col_crs_dark, linewidth=1.5) +
    geom_point(data=df_model_c, aes(x=Crs_prop, y=Cortisol_raw), colour=col_crs_light, alpha=0.7, size=point_size) +
    #geom_smooth(data = df_model_c, aes(x = Crs_prop, y = Cortisol_raw, linetype = DartingSeason),
    #colour = col_crs_dark, linewidth = 1.5, method = method_line, se = FALSE) +
    #scale_linetype_manual(values = c("Baby" = line_baby, "Mating" = line_mating)) +
    theme_classic() +
    labs(x="Crossing", y="Cortisol (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  alarmT<-ggplot() +
    geom_ribbon(data=predTalarm, aes(x=x, ymin=Testosterone_low, ymax=Testosterone_high), fill=col_alarm_dark, alpha=0.2, colour=NA) +
    geom_line(data=predTalarm, aes(x=x, y=Testosterone_pred), colour=col_alarm_dark, linewidth=1.5) +
    geom_point(data=df_model, aes(x=AlarmMP_prop, y=Testosterone_raw), alpha=0.7, size=point_size, colour=col_alarm_light) +
    #geom_smooth(data = df_model, aes(x = AlarmMP_prop, y = Testosterone_raw, linetype = DartingSeason),
    #colour = col_alarm_dark, linewidth = 1.5, method = method_line, se = FALSE) +
    #scale_linetype_manual(values = c("Baby" = line_baby, "Mating" = line_mating)) +
    theme_classic() +
    ylim(min(df_model$Testosterone_raw) -0.4, max(df_model$Testosterone_raw)) +
    labs(x="Alarm", y="Testosterone (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  bgeT<-ggplot() +
    geom_ribbon(data=predTbge, aes(x=x, ymin=Testosterone_low, ymax=Testosterone_high), fill=col_bge_dark, alpha=0.2, colour=NA) +
    geom_line(data=predTbge, aes(x=x, y=Testosterone_pred), colour=col_bge_dark, linewidth=1.5) +
    geom_point(data=df_model, aes(x=BGE_prop, y=Testosterone_raw), colour=col_bge_light, alpha=0.7, size=point_size) +
    #geom_smooth(data = df_model, aes(x = BGE_prop, y = Testosterone_raw, linetype = DartingSeason),
    #colour = col_bge_dark, linewidth = 1.5, method = method_line, se = FALSE) +
    #scale_linetype_manual(values = c("Baby" = line_baby, "Mating" = line_mating)) +
    theme_classic() +
    ylim(min(df_model$Testosterone_raw) -0.4, max(df_model$Testosterone_raw)) +
    annotate("text", x=1, y=7.8, label=".", size=font_size) +
    labs(x="BGC", y="Testosterone (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  vigT<-ggplot() +
    geom_ribbon(data=predTvig, aes(x=x, ymin=Testosterone_low, ymax=Testosterone_high), fill=col_vig_dark, alpha=0.2, colour=NA) +
    geom_line(data=predTvig, aes(x=x, y=Testosterone_pred), colour=col_vig_dark, linewidth=1.5) +
    geom_point(data=df_model, aes(x=Vig_prop, y=Testosterone_raw), alpha=0.7, size=point_size, colour=col_vig_light) +
    #geom_smooth(data = df_model, aes(x = Vig_prop, y = Testosterone_raw, linetype = DartingSeason),
    #colour = col_vig_dark, linewidth = 1.5, method = method_line, se = FALSE) +
    #scale_linetype_manual(values = c("Baby" = line_baby, "Mating" = line_mating)) +
    theme_classic() +
    ylim(min(df_model$Testosterone_raw) -0.4, max(df_model$Testosterone_raw)) +
    labs(x="Sentinelling", y="Testosterone (ng/g)") +
    theme(legend.position="none", text=element_text(size=font_size))

  crsT<-ggplot() +
    geom_ribbon(data=predTcrs, aes(x=x, ymin=Testosterone_low, ymax=Testosterone_high), fill=col_crs_dark, alpha=0.2, colour=NA) +
    geom_line(data=predTcrs, aes(x=x, y=Testosterone_pred), colour=col_crs_dark, linewidth=1.5) +
    geom_point(data=df_model_c, aes(x=Crs_prop, y=Testosterone_raw), alpha=0.7, size=point_size, colour=col_crs_light) +
    #geom_smooth(data = df_model_c, aes(x = Crs_prop, y = Testosterone_raw, linetype = DartingSeason),
    #colour = col_crs_dark, linewidth = 1.5, method = method_line, se = FALSE) +
    #scale_linetype_manual(values = c("Baby" = line_baby, "Mating" = line_mating)) +
    theme_classic() +
    ylim(min(df_model$Testosterone_raw) -0.4, max(df_model$Testosterone_raw)) +
    annotate("text", x=1, y=7.5, label="*", size=font_size) +
    labs(x="Crossing", y="Testosterone (ng/g)") +
    theme(legend.position="left", text=element_text(size=font_size))

  wrap_plots(
    alarmT, bgeT, vigT, crsT,
    alarmC, bgeC, vigC, crsC,
    nrow=2,
    ncol=4,
    guides="collect"
  ) +
    plot_annotation(tag_levels="A")
}

#### Correlation matrix
{
df_cor <- df %>% dplyr::select(Testosterone, Cortisol, AlarmMP_prop, BGE_prop, Crs_prop, Vig_prop,
                               zCSI, Tenure,elot)

names(df_cor) <- c("Testosterone", "Cortisol", "Alarm", "BGC", "Sentinelling", "Crossing",
                   "zCSI", "Tenure", "Competitive status")

cor_mat <- df_cor %>% cor(use = "pairwise.complete.obs")
cor_order <- hclust(as.dist(1 - abs(cor_mat)))$order
cor_levels <- colnames(cor_mat)[cor_order]

cor_df <- cor_mat %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(cols = -var1, names_to = "var2", values_to = "r") %>%
  dplyr::mutate(var1 = factor(var1, levels = cor_levels), var2 = factor(var2, levels = rev(cor_levels)), label = sprintf("%.2f", r))

ggplot(cor_df, aes(x = var1, y = var2, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = label, colour = abs(r) > 0.5), size = 7, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white")) +
  scale_fill_gradient2(low = "#4575B4", mid = "white", high = "#D73027", midpoint = 0, limits = c(-1, 1), name = "Pearson r") +
  coord_equal() +
  theme_classic(base_size = font_size) +
  labs(x = NULL, y = NULL) +
  theme(
    text = element_text(size = font_size),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "right"
  )} # no seasons
{library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

df_cor <- df %>%
  dplyr::select(
    DartingSeason,
    Testosterone, Cortisol,
    AlarmMP_prop, BGE_prop, Crs_prop, Vig_prop,
    zCSI, Tenure, elot
  )

names(df_cor) <- c(
  "DartingSeason",
  "Testosterone", "Cortisol",
  "Alarm", "BGC", "Sentinelling", "Crossing",
  "zCSI", "Tenure", "Competitive status"
)

# One shared order based on the full dataset
cor_mat_all <- df_cor %>%
  dplyr::select(-DartingSeason) %>%
  cor(use = "pairwise.complete.obs")

cor_order <- hclust(as.dist(1 - abs(cor_mat_all)))$order
cor_levels <- colnames(cor_mat_all)[cor_order]

# Correlations by season
cor_df <- df_cor %>%
  split(.$DartingSeason) %>%
  purrr::imap_dfr(~{
    cor_mat <- .x %>%
      dplyr::select(-DartingSeason) %>%
      cor(use = "pairwise.complete.obs")

    cor_mat %>%
      as.data.frame() %>%
      tibble::rownames_to_column("var1") %>%
      tidyr::pivot_longer(
        cols = -var1,
        names_to = "var2",
        values_to = "r"
      ) %>%
      dplyr::mutate(
        DartingSeason = .y,
        label = sprintf("%.2f", r)
      )
  }) %>%
  dplyr::mutate(
    var1 = factor(var1, levels = cor_levels),
    var2 = factor(var2, levels = rev(cor_levels))
  )

ggplot(cor_df, aes(x = var1, y = var2, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(
    aes(label = label, colour = abs(r) > 0.5),
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white")) +
  scale_fill_gradient2(
    low = "#4575B4",
    mid = "white",
    high = "#D73027",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson r"
  ) +
  facet_wrap(~DartingSeason) +
  coord_equal() +
  theme_classic(base_size = font_size) +
  labs(x = NULL, y = NULL) +
  theme(
    text = element_text(size = font_size),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )} # with seasons


## Father plots, not enough data to pursue this testing.

# seasons and hormones
df %>% filter(DartingSeason == "Mating") %>% group_by(Father) %>%
  summarize(meanT = mean(Testosterone), sdT = sd(Testosterone), meanC = mean(Cortisol), sdC = sd(Cortisol))

tfather <- ggplot(df %>% filter(DartingSeason == "Mating"), aes(x = elot, y = Testosterone, fill = Father, colour = Father)) +
  geom_smooth(method = "lm", linewidth = 1.5) +
  geom_point(alpha=0.7, size=point_size) +
  theme_classic() +
  scale_fill_manual(name= "Potential father", values = c("No" = "#4F4789", "Yes" = "#FCE762")) +
  scale_colour_manual(name= "Potential father", values = c("No" = "#4F4789", "Yes" = "#FCE762")) +
  labs(x = "Competitive status", y = "Testosterone (ng/g)") +
  theme(legend.position = "none", text = element_text(size = 18))

cfather <- ggplot(df %>% filter(DartingSeason == "Mating"), aes(x = elot, y = Cortisol, fill = Father, colour = Father)) +
  geom_smooth(method = "lm", linewidth = 1.5) +
  geom_point(alpha=0.7, size=point_size) +
  scale_fill_manual(name= "Potential father", values = c("No" = "#4F4789", "Yes" = "#FCE762")) +
  scale_colour_manual(name= "Potential father", values = c("No" = "#4F4789", "Yes" = "#FCE762")) +
  theme_classic() +
  labs(x = "Competitive status", y = "Cortisol (ng/g)") +
  theme(legend.position = "right", text = element_text(size = 18))

tfather + cfather + plot_annotation(tag_levels = "A", title = "Mating season",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold")))