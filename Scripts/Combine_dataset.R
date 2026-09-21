# ---------------
# Title: Combine dataset
# Date: 18 april 2026
# Author: mgranellruiz
# Goal: Combine differet sources of data to calculate the variables to use in the analysis of hair hormones and male services

# I calculated MS with the MSI: /Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/Scripts/MSIndex_hair_hormones.R with the indvidual function
# using data for the 3 months prior darting date.
# I calculated CSI with the social indices package, full script: /Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/Scripts/CSI_calculation_for_MShair.R
# using data for the 3 months prior the darting date.
# I calculated rank with the elorating package, full script: /Users/mariagranell/Repositories/elo-sociality/elo/Hierarchies_for_all_groups_best.R
# using 12 months of data prior the darting date, the week before and the week after, for a reliable hierarchy.
# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')

# path ------------------------
setwd("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/")

# data ------------------------
# hormone data of 2022 and 2023
horm_df_base <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/horm_df_base.csv")
# weight extraction
weight_extraction <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/Data/weight_extraction.csv")
# male services data
ms <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/MSIndex_individual_com_shortterm.csv")
# rank data, that includes information of rank reversals
rank <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/ELO_dartingdate_maleservices_hair_reversals.csv") %>% rename(AnimalCode = IDIndividual1)
# csi data
csi <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/CSI_maleservices_hair_3months.csv")

# combine df
df <- horm_df_base %>%
  left_join(weight_extraction, by = c("DartingID" = "Sample_ID", "DartingSeason")) %>%
  left_join(csi %>% dplyr::select(AnimalCode, DartingDate,zCSI ), by=c("AnimalCode", "DartingDate")) %>%
  left_join(rank, by= c("AnimalCode", "DartingDate")) %>%
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
  filter(!is.na(elo12)) %>%
  # transform hormonal data from ng/ml to ng/g. value ng/ml multiplied by final resuspention volume (100ul of methanol) divided by grams of hair weighted
  mutate(across(all_of(c("Cortisone", "Cortisol", "DHEA", "Androstedione", "Testosterone", "Progesterone")), ~ (.x * 0.1) / (weight / 1000)))

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
  mutate(elot = scale(elo12) + scale(rank_trajectory),
         topMale = ifelse(elo12 == 1, "Dominant", "Subordinate"),
         trajectory = case_when(
           rank_trajectory > 0 ~ "decreasing",
           rank_trajectory == 0 ~ "stable",
           rank_trajectory < 0 ~"increasing",
           TRUE ~ NA
         )
  )

#write.csv(df, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/HairHormonesMSdf.csv", row.names = F)

# demographic table
table(df$DartingSeason)
ggscatterstats(df %>% distinct(), x=Testosterone, y = Cortisol)