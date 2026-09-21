# ---------------
# Title: Male service indices for hair hormone analyses
# Date: 25 feb 2025
# Author: mgranellruiz
# Goal: Calculate individual male service indices over the three months preceding darting,
# accounting for group membership, migration and sampling effort.
# ---------------


# library ---------------------
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
source('/Users/mariagranell/Repositories/data/functions.R')

# path ------------------------
setwd("/Users/mariagranell/Repositories/male_services_index/MSpublication/Scripts")

# parameters ------------------
MSGroups <- c("NH", "AK", "BD", "KB", "LT", "IF", "CR")

# male services are calculated over the three months preceding darting
# using the final df
darting <- df_ms %>%
   mutate(DartingDate=ymd(DartingDate),
           MSStartDate=DartingDate - months(3),
           MSEndDate=DartingDate,
           year=ifelse(DartingSeason == "darting2022", 2022, 2023)
    ) %>%
    mutate(PeriodCalc="threemonths",
           MSEndDate = case_when( # available data
             AnimalCode == "Rat" & year(DartingDate) == 2023 ~ MSEndDate + months(2),
             AnimalCode == "Rat" & year(DartingDate) == 2022 ~ MSEndDate + months(1),
             AnimalCode == "Sta" & year(DartingDate) == 2023 ~ MSEndDate + months(2),
             AnimalCode == "Vry" & year(DartingDate) == 2022 ~ MSEndDate + months(2),
             AnimalCode == "War" & year(DartingDate) == 2022 ~ MSEndDate + months(2),
             TRUE ~ MSEndDate
           ))


# data ------------------------
# required files are encoded in each section. Fact-checked life-history data.
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv")
# data
{
  ALARM_org <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/alarm_allmyfiles.csv")
  FOCAL_org <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/vigilance_allmyfiles.csv") %>%
    filter(Total > 0)
  CROSSING_org <- read.csv("/Users/mariagranell/Repositories/data/Jakobcybertrackerdatafiles/CleanFiles/crossing_cybertracker.csv")
  BGE_org <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/bge_interactions_allmyfiles.csv")
}

# FUNCTIONS ---------
calc_MSIndex_by_individual <- function(MSStartDate, MSEndDate, Indv) {

# data ------------------------
# there are 4 different types of behavioural data but for the darting analysis we will calculate all:

  # select from life history in which groups this individul was present in during this period of time
  indv_groups <- lh %>% filter(AnimalCode == Indv) %>%
    filter(StartDate_mb <= MSEndDate & EndDate_mb >= MSStartDate) %>%
    dplyr::select(AnimalCode, StartDate_mb, EndDate_mb, Tenure_type, Group_mb)
  gps_present <- (indv_groups$Group_mb)

# ALARM INDEX MORE THAN PREDATORS (MP)----
{ALARM_MP <- ALARM_org %>%
  filter(!(Date <= MSStartDate | Date >= MSEndDate), Group %in% MSGroups)
# Select alarm events for MS, that is
ALARM_MP_base <- ALARM_MP %>%
  filter(Group %in% MSGroups) %>%
  mutate(NbUnkInd = ifelse(is.na(NbUnkInd), 0,NbUnkInd)) %>%
  mutate(threat_predator = case_when(
    species %in% c("Bushpig", "Duiker", "Giraffe", "Hare", "5 kudu","Impala","Wildebeest","Warthog","Nyala","Kudu","Not predator", "Vulture", "Non-Raptor Bird", "Ibis","Duck/Goose", "Nightjar", "Not raptor") ~ "Not predator",
    otherspecies %in% c("Blesbok","Cormorant","car","cormorant","Crowned lapwing","Turraco", "terraco", "Porcupine", "pigeon lol","crow","mangose","Hornbill","heroine", "glossy starling","cheeky","cattle","Blesbok", "bees", "antelope") ~ "Not predator",
    species %in% c("1raptor", "2eagles", "2yellow bilked kite followed by a third bigger rapto", "Jackal",
                   "Raptor", "African Harrier Hawk", "African Hawk Eagle", "Caracal", "Crowned Eagle", "Khayalami Dogs", "Martial Eagle",
                   "Owl", "Poacher Dogs", "Spotted Eagle Owl", "Predator", "Spotted Eagle Owl", "Verraux's Eagle Owl", "Genet", "Serval") ~ "Predator",
    otherspecies %in% c("Dogs from the village, not necessarily poacher's dogs as we were next to somebody's yard", "Verraux's eagle", "either aigle or owl",
                        "fish eagle","Some type of eagle", "leopard model", "tawny eagle", "unknown, black dog like body with white tail - could it be a wild dog?",
                        "eagle", "juvenile fish eagle", "crested eagle?", "eagle, exact species unknown", "eagle but not sure which one",
                        "village dog") ~ "Predator",
    OtherContext %in% c("Lukas caracal", "poachers") ~ "Predator",
    otherspecies %in% c("Thickknee", "cow") ~"Not predator",
    otherthreat %in% c("3 dogs", "Eagle model", "Fake Caracal", "eagle expirment", "jongo", "khayalami person running back on main road", "close to the house, see humans and dogs") ~"Predator",
    otherthreat %in% c( "unhabituated group", "bird", "touch screen", "bge", "BD", "helicopter",
                        "Helicopter", "dix (bd male)", "field assistant", "LT") ~"Not predator",
    Threat %in% c("Distant Monkeys Calling", "Carcass", "New Male") ~ "Not predator",
    Remarks %in% c("reaction to a contact call (probably) from another group", "answer to a monkey calling far away") ~ "Not predator",
    TRUE ~ "Unk"
  )) %>%
  group_by(EventID) %>%
  # filter for the events in where there was more action than only chuttering, since is not a service and
  # filter also for events in where there was no unk callers
  mutate(MoreThanChutter = if_else(any(CallType != "Chutter"), "yes", "no"),
         IsThereUnk = if_else(any(str_detect(IDActors, "Unk")), "yes", "no")) %>%
  ungroup() %>%
  filter(MoreThanChutter == "yes",
         IsThereUnk == "no",
         Context != "BGE" #not intrested in BGE
  ) %>% distinct() %>%
  # only selected the predators
  mutate(threat_type = case_when(
      Threat == "Distant Monkeys Calling" ~ "competition",
      Threat == "New Male" ~ "competition",
      Threat == "Aerial" ~ "predator",
      Threat == "Terrestrial" ~"predator",
      Threat == "Carcass" ~"predator",
      Threat == "Reptile" ~"predator",
      Threat == "Humans (non-researchers)" ~ "predator",
      TRUE ~ "Unk"
    )) #%>%
    #  filter(
    #  Threat %in% c( "Terrestrial", "Aerial"),
    #  #threat_predator %in% c("Predator", "Not predator")
    #  threat_predator == "Predator"
    #)


# select the events you are intrested in
  eventsMP <- ALARM_MP_base %>%
    dplyr::select(EventID, Date, Data, Group, threat_type, threat_predator, Threat) %>% distinct() %>%
    # check if there are duplicated events
    add_count(EventID) %>%
    # make sure the information of the events is consisent by removing duplicated with less info
    filter(!(n == 2 & threat_predator == "Unk"), !(EventID == 3723 & is.na(Threat)),
           !(n == 3 & threat_predator == "Not predator" & Threat == "Terrestrial" ),
           !(n == 3 & threat_predator == "Unk"),
    ) %>%
    mutate(Threat = ifelse(is.na(Threat), "Unk", Threat)) %>%
    add_group_composition("Group", "Date") %>%
    add_season("Date")%>%

    # add the individuals information
    left_join(indv_groups, by = c("Group"="Group_mb"), relationship = "many-to-many") %>%
    filter(Date > StartDate_mb & Date < EndDate_mb) %>% distinct() %>%
    group_by(Group) %>%
    dplyr::summarize(N_AlarmsMP = n(), .groups = "drop")

# select the participation
  indv_thatparticipated_MP <- ALARM_MP_base %>%
    separate_rows(IDActors, sep =";") %>%
    mutate(AnimalCode = str_remove(IDActors , " "), Date = ymd(Date)) %>% distinct() %>%
    # add the individuals information
    left_join(indv_groups, by = c("Group"="Group_mb", "AnimalCode"), relationship = "many-to-many") %>%
    filter(Date > StartDate_mb & Date < EndDate_mb) %>% distinct() %>%
    mutate(participation_alarm = 1) %>%
    dplyr::select(EventID, AnimalCode, Group, CallType, threat_type, participation_alarm) %>% distinct() %>%
    group_by(Group) %>%
  dplyr::summarize(N_AlarmServiceMP = n(), .groups = "drop")

# merge both dataframes --------------------------
MSalarmMP <- indv_groups %>%
  full_join(eventsMP, by =c("Group_mb"="Group")) %>%
  full_join(indv_thatparticipated_MP, by =c("Group_mb"="Group")) %>%
  mutate(N_AlarmsMP = as.numeric(N_AlarmsMP), N_AlarmServiceMP = as.numeric(N_AlarmServiceMP)) %>%
  mutate(across(where(is.numeric), ~tidyr::replace_na(.x, 0)))
# errors alarms
MSalarm_errorsMP <- MSalarmMP %>%
  filter(N_AlarmsMP < N_AlarmServiceMP)

rm(ALARM_MP_base, indv_thatparticipated_MP, eventsMP, MSalarm_errorsMP)
}

# VIGILANCE INDEX --------
# no need to have a MSvigilance_errors object
{# You need to run the Clean_focal_data.r that Jos made. You can find it here: /Users/mariagranell/Repositories/data/data2022-06_2023-12/Cleaning_focal_data.r
FOCAL <- FOCAL_org %>%
  filter(!(Date <= MSStartDate | Date >= MSEndDate), Group %in% MSGroups, IDIndividual1 == Indv)

MSvigilance <- FOCAL %>% # calculate the age of each Ind when the focal was done
  left_join(lh[,c("AnimalCode", "Sex", "DOB_estimate", "Group_mb", "StartDate_mb", "EndDate_mb", "Tenure_type")],
            by = c("IDIndividual1" = "AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
  filter(Date > StartDate_mb & Date < EndDate_mb) %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"), # calculate their age based on the date of the focal
         Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
  filter(Age_class %in% c("adult","sub-adult")) %>%
  group_by(IDIndividual1, Group) %>%
  dplyr::summarize(Total_vigilant_time = sum(Vigilant), .groups = "drop", Tota_focal_time = sum(Total))

  rm(FOCAL)}

# BGE INDEX --------
# not possible to have a MSbge_errors object
{
BGE <- BGE_org %>% filter(!(Date <= MSStartDate | Date >= MSEndDate)) %>% mutate(Remarks = NA)

# I am going to make two dataframes one for the focal and one for the encounter --------------
# with virtually the same information.Since we don´t mind the behaviour of the reciever. They simply did a behaviour
# from the perspective of the focal
# ignore the warnings
{

# Focal
dFocal <- BGE %>%
  dplyr::select(Date, Time, Group, Initiators, ActorsBehaviour, OtherBehaviour, Remarks, BGE_id_interactions) %>%
  # include remarks
  mutate(Initiators = case_when(
    grepl("Kno", Remarks, ignore.case = T) ~ "Kno",
    grepl("UnkADM is Yazoo", Remarks, ignore.case = T) ~ "Yaz",
    grepl("UnkAM is Kno", Remarks, ignore.case = T) ~ "Eis; Nge; Nuk; Pie; Pix; Rid; Sig; Kno",
    TRUE ~ Initiators))
# Split behaviours, ignore warning
dFocal <- suppressWarnings(split_behaviours(dFocal, c("Initiators","ActorsBehaviour"), ";"))
# cleaning
dFocal <- dFocal %>%
  # integrate Other Behaviours, the rest I will remove
  mutate(ActorsBehaviour = as.character(ifelse(is.na(ActorsBehaviour), NA_character_, as.character(ActorsBehaviour)))) %>% # problem with the function
  mutate(ActorsBehaviour = case_when(
    ActorsBehaviour == "Other" & grepl("Chirp|bark", OtherBehaviour, ignore.case = T) ~ "Alarm calls",
    ActorsBehaviour == "Other" & grepl("ap", OtherBehaviour, ignore.case = T) ~ "Advance slow",
    ActorsBehaviour == "Other" & grepl("Head flick", OtherBehaviour, ignore.case = T) ~ "Head bob",
    ActorsBehaviour == "Other" & grepl("fi", OtherBehaviour, ignore.case = T) ~ "Fight",
    ActorsBehaviour == "Other" & grepl("at", OtherBehaviour, ignore.case = T) ~ "Attack",
    ActorsBehaviour == "Attack; Other" & grepl("fi", OtherBehaviour, ignore.case = T) ~ "Attack; Fight",
    ActorsBehaviour == "Head bob; Stare; Other" & grepl("at", OtherBehaviour, ignore.case = T) ~ "Head bob; Stare; Advance slow",
    TRUE ~ ActorsBehaviour
  ))

# Encountered
dEncountered <- BGE %>%
  dplyr::select(Date, Time, EncounterGp, IDReceivers, BehaviourReceivers, OtherRecBehaviour, Remarks, BGE_id_interactions) %>%
  rename(Group = EncounterGp, Initiators = IDReceivers, ActorsBehaviour = BehaviourReceivers, OtherBehaviour = OtherRecBehaviour) %>%
  # include remarks
  mutate(Initiators = case_when(
    grepl("Pom", Remarks, ignore.case = T) ~ "Pom",
    grepl("Rey", Remarks, ignore.case = T) ~ "Rey",
    grepl("UnkAM is Bab", Remarks, ignore.case = T) ~ "Bab",
    grepl("Unknown Male is Skh Skhumbuzo)", Remarks, ignore.case = T) ~ "Skh",
    grepl("War alarming", Remarks, ignore.case = T) ~ "War",
    TRUE ~ Initiators))
# Split behaviours, ignore warning
dEncountered <- suppressWarnings(split_behaviours(dEncountered, c("Initiators","ActorsBehaviour"), ";"))
# cleaning
dEncountered <- dEncountered %>%
  # integrate Other Behaviours, the rest I will remove
  mutate(ActorsBehaviour = case_when(
    ActorsBehaviour == "Other" & grepl("ch", OtherBehaviour, ignore.case = T) ~ "Advance fast",
    TRUE ~ ActorsBehaviour
  ))

dat0 <- rbind(dFocal, dEncountered) %>% rename(Individual =Initiators, Behaviour = ActorsBehaviour) %>%
  # remove the space that creates split behaviours in the names
  mutate(
    Individual = str_trim(Individual, side = "left"),
    Behaviour = str_trim(Behaviour, side = "left")
  ) %>%
  filter(Group %in% MSGroups,
         # remove if behaviour is other or if there is no Behaviour
         Behaviour != "Other", !is.na(Behaviour),
         # remove when the actor is unk
         !grepl("Unk|ZZ_All", Individual), !is.na(Individual),
         # remove Other behaviours that are not intresting
         !grepl("cross the", OtherBehaviour)
         )

rm(dFocal,dEncountered)}

# select the BGE in where there was an Unk to later remove
  bge_list_unk <- dat0 %>% mutate(Individual = tolower(Individual)) %>%
    filter(str_detect(Individual, "unk")) %>% distinct(BGE_id_interactions) %>%pull()

# change behaviours names so they stay the same as in the cybertracker system
dat0_clean <- dat0 %>%
  filter(!(BGE_id_interactions %in% bge_list_unk)) %>%
  dplyr::mutate(
    Behaviour = dplyr::case_when(
      str_detect(Behaviour, "^(hb|st\\.hb|hb\\.st|rt|ap5 hb|hb\\.rt)$") ~ "Head bob",
      str_detect(Behaviour, "^(advance \\(slow\\)|ap|ap5|ap2)$") ~ "Advance slow",
      str_detect(Behaviour, "^alarm call$") ~ "Alarm calls",
      str_detect(Behaviour, "^(contact call \\(cc\\)|ap\\.vo|vo|ap2\\.vo)$") ~ "Contact calls",
      str_detect(Behaviour, "^(chorus cc|aggression call|chorus aggression call|cc)$") ~ "Chorus calls",
      str_detect(Behaviour, "^(ap\\.st|ap5 st|ap5\\.vo\\.st|st|st\\.ss|sc\\.st)$") ~ "Stare",
      str_detect(Behaviour, "^advance \\(fast\\)$") ~ "Advance fast",
      str_detect(Behaviour, "^(ap1 fl ch|ch\\.vo|ch|vo\\.st\\.ch|sc\\.ch|ch\\.vo\\.ba|ch fl|nr ch|ap ch|fl\\.vo\\.ch|ap ap ch|st\\.sc\\.lo ch)$") ~ "Chase",
      str_detect(Behaviour, "^ch\\.fi fl$") ~ "Fight",
      str_detect(Behaviour, "^(is\\.at|at|at ch)$") ~ "Attack",
      str_detect(Behaviour, "^face-off$") ~ "Face offs",
      TRUE ~ Behaviour
    )
  )

# cateogrize individual behaviours in levels of services
{minor <- c( "Advance slow" , "Contact calls", "Alarm calls", "Vocalise", "Vigilant", "Chorus calls", "Stand-up")
medium <-  c("Advance fast", "Front line (ind at the interface)", "Face offs", "Stare", "Head bob")
high <-  c("Attack", "Fight", "Bite", "Chase")
}

dat2 <- dat0_clean %>%
  mutate(Behaviour_spe = Behaviour,
         Individual = str_remove(Individual, "#4")) %>%
  mutate(Behaviour = case_when(
    Behaviour %in%  minor ~ 'minor',
    Behaviour %in%  medium ~ 'medium',
    Behaviour %in%  high ~ 'high',
    TRUE ~ Behaviour
  )) %>%
  filter(Behaviour %in% c("minor", "medium", "high"))

rm(minor, medium, high)

# clean the names of individuals
dat2_cleannames <- dat2  %>%
    filter(!grepl("[0-9]|Most", Individual)) %>%
    separate_rows(Individual, sep = ",| ") %>%
    mutate(Individual = str_to_title(Individual))%>%
    mutate(Individual = case_when(
      Individual %in% c("Plainjane", "Plain") ~ "PlainJane",
      Individual == "Pointyears" ~ "PointyEars",
      Individual == "Wavyears" ~ "WavyEars",
      TRUE ~ Individual
    )) %>%
    filter(Individual != " ", Individual != "") %>%
    integrate_otherid(., Individual)

# number of BGE in binary, i.e. whether they participated in a BGE or not. ---
n_bge_serv_binary <- dat2_cleannames %>% # do it from dat2 because we remove those bge in where the data was too low for us to determine the interactions or paticipation.
  # i.e. not fair to consider a bge in where only one interaction was considered and they were unk indiv recorded, that is why we work on a datafrmae with no unk
  filter(Individual == Indv) %>%
  dplyr::select(BGE_id_interactions, Individual, Group) %>% distinct() %>%
  group_by(Group) %>% summarize(number_bge_they_participated_binary =n())

# number of opportunities an individual has had ------------
# number of times there was a bge an an individual was seen using BGE_id_interactions
n_bge <- dat2_cleannames %>% # do it from dat2 because we remove those bge in where the data was too low for us to determine the interactions or paticipation.
  # i.e. not fair to consider a bge in where only one interaction was considered and they were unk indiv recorded
  dplyr::select(BGE_id_interactions, Group, Date) %>% distinct() %>%
  group_by(Group) %>%
  dplyr::summarize(N_Bge = n(), .groups = "drop")

# merge both dataframes --------------------------
  MSBge <- n_bge %>% filter(Group %in% gps_present) %>%
    left_join(n_bge_serv_binary, by="Group") %>%
    mutate(across(where(is.numeric), ~tidyr::replace_na(.x, 0)))

rm(dat0,dat2,BGE, n_bge,n_bge_serv_binary)
}

# CROSSING INDEX adjusted--------
{
CROSSING <- CROSSING_org %>%
  filter(!(Date <= MSStartDate | Date >= MSEndDate), Group %in% MSGroups)

CROSSING_base <- CROSSING %>%
  # select only dangerous river crosings
  filter(CrossingType %in% c("Fence", "River - Ground Level", "River - Swimming"),
         Behaviour == "First Crosser",
         Group %in% c("AK", "NH", "BD", "KB", "LT")
  ) %>%
  # select only the crossing in where an adult male was seen crossing
  left_join(lh %>% dplyr::select(AnimalCode, Sex, DOB_estimate, Tenure_type, StartDate_mb, EndDate_mb) %>% distinct(),
            by = c("IDIndividual1" = "AnimalCode"), relationship = "many-to-many") %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"),
         AgeClass = get_age_class_w_tenuretype(Sex,Age, Tenure_type)) %>%
  filter(StartDate_mb < Date & EndDate_mb > Date,
         #AgeClass == "AM"
  ) %>% distinct() %>%
  filter(CrossingType != "Fence")

# extra filter. Only select crossing in where at least 10% of the group had crossed
crs_atleast_tenpercent <-
  CROSSING %>%
  filter(Obs.nr %in% CROSSING_base$Obs.nr,
         Behaviour %in% c("Crossing","First Crosser","Last Crosser")) %>%
  count(Obs.nr, Date, Group, name = "n_cross") %>%                      # crossers per event
  left_join(
    lh %>% dplyr::select(Group_mb, StartDate_mb, EndDate_mb, AnimalCode),
    by = join_by(Group == Group_mb, Date > StartDate_mb, Date > EndDate_mb)  # members present at that date
  ) %>%
  group_by(Obs.nr, Date, Group, n_cross) %>%
  summarise(n_members = n_distinct(AnimalCode), .groups = "drop") %>%
  mutate(prop_crossed = n_cross / n_members) %>%
  filter(prop_crossed >= 0.10)

crs_keep <- CROSSING_base %>% filter(Obs.nr %in% crs_atleast_tenpercent$Obs.nr, Obs.nr != 835) %>% # 835 has two frist crossers
  dplyr::select(Date, Obs.nr, Group, CrossingType, IDIndividual1, Season) %>%
  distinct() %>%
  # add the list of AM that were present int he crossings.
  left_join(
    lh %>% dplyr::select(Group_mb, StartDate_mb, EndDate_mb, AnimalCode, Sex, DOB_estimate, Tenure_type),
    by = join_by(Group == Group_mb, Date > StartDate_mb, Date < EndDate_mb)  # members present at that date
  ) %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"),
         AgeClass = get_age_class_w_tenuretype(Sex,Age, Tenure_type),
         FirstCrosser = ifelse(IDIndividual1 == AnimalCode, 1, 0)
  ) %>%
  filter(AgeClass == "AM") %>%
  add_group_composition("Group", "Date") %>%
  # create the n_males in case is an empty df
  { if (!"n_males" %in% names(.)) { dplyr::mutate(., n_males = NA_real_) } else { . } }

  # to calculate the proportion, I did the expected and observed proportion for each event
complex_calculation <- crs_keep %>%
  group_by(AnimalCode, Group, Obs.nr, n_males) %>%
  summarise(          # remove the warning if the dataframe is empty
    Obs = max(FirstCrosser),           # was this male first in this event?
    Exp = 1/unique(n_males),           # expected probability in this event
    .groups = "drop"
  ) %>% suppressWarnings()
  # here I did the main summary
simple_calculation <- crs_keep %>%
  group_by(AnimalCode, Group) %>%
  reframe(
    N_Crossings = n_distinct(Obs.nr),   # number of unique events
    N_CrsService = sum(FirstCrosser),
    mean_n_males = mean(n_males)
  )


# calculate MScrossing by dividing the observed proportion (N_Crossings/N_CrsService) by the expected proportion (1/n_males)
MScrossing <- complex_calculation %>%
  group_by(AnimalCode, Group) %>%
  summarise(
    ObservedSum = sum(Obs, na.rm = TRUE),
    ExpectedSum = sum(Exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(simple_calculation, by = c("AnimalCode", "Group"))

rm(complex_calculation, CROSSING_base, crs_atleast_tenpercent, crs_keep, simple_calculation)
}

# MERGE ALL INDEX
{MSIndex <- indv_groups %>%
  left_join(MSalarmMP, by=c("AnimalCode", "StartDate_mb", "EndDate_mb", "Tenure_type", "Group_mb")) %>%
  left_join(., MSBge, by=c("Group_mb"="Group")) %>%
  left_join(., MSvigilance, by=c("AnimalCode" = "IDIndividual1", "Group_mb"="Group")) %>%
  left_join(., MScrossing, by=c("AnimalCode" , "Group_mb"="Group"))
  }

  return(MSIndex)
} # consider here making a dataframe that considers group in migration

# RESULTS --------
# includes migration. For individuals that migrated between groups
# we calculated the pooled proportion (i.e., weighted by number of opportunities/events per group). This
# allows us to control for differences in opportunities a male could have had because of being in different groups
{
# we are going to update the initial MSStartDate and MSEndDate calculations so we adjust for migration
indv_df_possible_consideringGp <- darting %>%
    dplyr::select(AnimalCode, MSStartDate, MSEndDate, PeriodCalc, DartingDate) %>%
    left_join(lh %>% dplyr::select(AnimalCode, StartDate_mb, EndDate_mb, Group_mb) %>% distinct(), by = "AnimalCode", relationship ="many-to-many") %>%
     filter(StartDate_mb <= MSEndDate & EndDate_mb >= MSStartDate) %>% distinct() %>%
    # update the male services timeline considering migration for each group
    # so that while accepting the overal timeline we calcualte before, adjusting it to the actual presence of the monkey in each group
    mutate(MSStartDate = ifelse(MSStartDate < StartDate_mb, as.character(StartDate_mb), as.character(MSStartDate)),
           MSEndDate = ifelse(MSEndDate > EndDate_mb, as.character(EndDate_mb), as.character(MSEndDate)),
           DaysPresent = (ymd(MSEndDate) - ymd(MSStartDate)) +1
    )

# consider how much actual data sampling was done during the period of data collection I am asking
logbook <- read.csv("/Users/mariagranell/Repositories/data/acess_data/OutputData/logbook_access.csv")
sampling_effort <- indv_df_possible_consideringGp %>%
  left_join(logbook %>% mutate(present = 1) %>% dplyr::select(Date, Group, present), by= c("Group_mb" = "Group"), relationship = "many-to-many") %>%
  filter(between(Date, MSStartDate, MSEndDate)) %>% dplyr::select(MSStartDate, MSEndDate, AnimalCode, Group_mb, present, Date) %>% distinct() %>%
  group_by(MSStartDate, MSEndDate, AnimalCode, Group_mb) %>%
  summarize(days_present_researchers = sum(present))

# simplify dataframe so you can use it in the function
indv_df_possible_updated <- indv_df_possible_consideringGp %>%  filter(as.numeric(DaysPresent) > 6) %>% # remove individuals that are present waay to little
  dplyr::select(MSStartDate, MSEndDate, Indv =AnimalCode) %>% distinct() # there are duplicated values for some individuals becuae of migration and calculations of rank

results_list_all <- pmap(indv_df_possible_updated, function(MSStartDate, MSEndDate, Indv) {
  tryCatch({
    calc_MSIndex_by_individual(MSStartDate, MSEndDate, Indv) %>%
      mutate(MSStartDate = MSStartDate,
             MSEndDate = MSEndDate,
             Indv = Indv
      )
  }, error = function(e) {
    message("Error for Indv ", Indv, " (", MSStartDate, " to ", MSEndDate, "): ", e$message)
    NULL  # Return NULL for this iteration if an error occurs.
  })
})

  # bring back into the dataframe
 com <- indv_df_possible_consideringGp %>%
   left_join(bind_rows(results_list_all), by = c(
     "AnimalCode", "MSStartDate", "MSEndDate", "StartDate_mb", "EndDate_mb", "Group_mb"), relationship = "many-to-many") %>%
   left_join(.,sampling_effort, by=c("AnimalCode", "MSStartDate", "MSEndDate", "Group_mb")) %>% # add the sampling effort
   mutate(sampling_effort = (as.numeric(days_present_researchers)*100/ as.numeric(DaysPresent) )) %>% # sampling effort proportion
   # filtering results. I consider filtering over sampling effort but if you only are there 4/4 days is still considers good coverage when is not, bge is better
   #filter(!is.na(N_Bge)) %>% # if no bge have happened when they were there a pretty good sign that there was no real sampling
   mutate(number_bge_they_participated_binary = replace_na(number_bge_they_participated_binary, 0)) %>%
   group_by(AnimalCode, PeriodCalc, DartingDate) %>%
   summarize(DaysPresent = sum(DaysPresent),
             mean_sampling_effort = mean(sampling_effort),
             # calcualte the pooled proportions
             AlarmMP_prop = sum(N_AlarmServiceMP, na.rm = TRUE) / sum(N_AlarmsMP, na.rm = TRUE),
             BGE_prop =  sum(number_bge_they_participated_binary, na.rm = TRUE) / sum(N_Bge, na.rm = TRUE),
             Vig_prop = sum(Total_vigilant_time, na.rm = TRUE) / sum(Tota_focal_time, na.rm = TRUE),
             Crs_prop = sum(ObservedSum, na.rm = TRUE) / sum(ExpectedSum, na.rm = TRUE),
             ) %>%
   # put NA if there was no events
   mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA_real_, .)))

  # Visualize the number of events
  summary_table <- indv_df_possible_consideringGp %>%
   left_join(bind_rows(results_list_all), by = c(
     "AnimalCode", "MSStartDate", "MSEndDate", "StartDate_mb", "EndDate_mb", "Group_mb"), relationship = "many-to-many") %>%
   left_join(.,sampling_effort, by=c("AnimalCode", "MSStartDate", "MSEndDate", "Group_mb")) %>% # add the sampling effort
   mutate(sampling_effort = (as.numeric(days_present_researchers)*100/ as.numeric(DaysPresent) )) %>% # sampling effort proportion
   # filtering results. I consider filtering over sampling effort but if you only are there 4/4 days is still considers good coverage when is not, bge is better
   #filter(!is.na(N_Bge)) %>% # if no bge have happened when they were there a pretty good sign that there was no real sampling
   mutate(number_bge_they_participated_binary = replace_na(number_bge_they_participated_binary, 0),
          DartingSeason = ifelse(year(DartingDate) == 2022, "Mating", "Baby")) %>%
        filter(!(AnimalCode== "Nak" & Tenure_type == "BirthGroup")) %>%
    group_by(DartingSeason, Group_mb) %>%
    summarize(n = dplyr::n(),
              DaysPresent = sprintf("%.1f ± %.1f",mean(days_present_researchers, na.rm = TRUE),sd(days_present_researchers, na.rm = TRUE)),
              SamplingEffort = sprintf("%.1f ± %.1f",mean(sampling_effort, na.rm = TRUE),sd(sampling_effort, na.rm = TRUE)),
              FocalHours = sprintf("%.1f ± %.1f",mean(Tota_focal_time, na.rm = TRUE),sd(Tota_focal_time, na.rm = TRUE)),
              Alarms = sprintf("%.1f ± %.1f",mean(N_AlarmsMP, na.rm = TRUE),sd(N_AlarmsMP, na.rm = TRUE)),
              BGC = sprintf("%.1f ± %.1f",mean(N_Bge, na.rm = TRUE),sd(N_Bge, na.rm = TRUE)),
              Crossings = sprintf("%.1f ± %.1f",mean(N_Crossings, na.rm = TRUE),sd(N_Crossings, na.rm = TRUE)),
              .groups = "drop")

}

# EXPORT ------
#write.csv(com, "/Users/mariagranell/Repositories/hormones/hormone_hair/MS-hair/OutputFiles/MSIndex_individual_com_shortterm.csv", row.names = F)