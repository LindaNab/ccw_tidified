################################################################################
# TIDIFICATION OF IJE CCW CODE
# TRIAL EMULATION: SURGERY WITHIN 6 MONTHS AMONG OLDER LUNG CANCER PATIENTS

# Author: Linda Nab (from Clemence Leyrat's R code) [lindanab4@gmail.com]
# Content: Tidification of Clemence Leyrat's code from the IJE tutorial

################################################################################

################################################################################
# Packages
################################################################################
library(survival)
library(boot) # To obtain bootstrapped CIs
library(here)
library(dplyr) # tidyverse
library(readr)

################################################################################
# Load data
################################################################################
# VARIABLES
# id: patient identifier
# fup_obs: observed follow-up time (time to death or 1 year if censored alive)
# death: observed event of interest (all-cause death) 1: dead, 0: alive
# timetosurgery: time to surgery (NA if no surgery)
# surgery: observed treatment 1 if the patient received surgery within 6 month, 
#          0 otherwise
# age: age at diagnosis
# sex: patient's sex (1 or 2)
# perf: performance status at diagnosis (0, 1, or 2)
# stage: stage at diagnosis (1 or 2)
# deprivation: deprivation score (ranging between 0-1)
# charlson: Charlson's comorbidity index (0, 1 or 2)
# emergency: route to diagnosis (0 or 1)
################################################################################
tab <- 
  read_csv(file = here("data", "ije-2019-08-1035-File008.csv"),
           col_types = cols(id = col_character(),
                            surgery = col_integer(),
                            timetosurgery = col_double(),
                            death = col_integer(),
                            sex = col_integer(),
                            charlson = col_integer(),
                            perf = col_integer(),
                            stage = col_integer(),
                            emergency = col_integer(),
                            fup_obs = col_double(),
                            age = col_double(),
                            deprivation = col_double()))

################################################################################
# Add outcome and fup
################################################################################
# Note: Letters A, B... refer to the scenarios identified in our graph
# We will create three new variables:
# - arm: arm of emulated trial ("Control" or "Surgery")
# - fup: the follow-up time in the emulated trial 
#        (which can be different from the observed follow-up time)
# - outcome: the outcome in the emulated trial 
#            (which can be different from the observed follow-up time)
# THESE VARIABLES WILL BE THE OUTCOME AND FOLLOW UP TIME IN THE OUTCOME MODEL
################################################################################
# Arm "Control": no surgery within 6 months
################################################################################
tab_control <- # We create a copy of the dataset: 
  tab %>%      # "clones" assigned to the control (no surgery) arm
  mutate(arm = "Control")
# Case 1: Patients receive surgery within 6 months (scenarii A to E): 
# --> they are still alive and followed-up until surgery
# Case 2: Patients do not receive surgery within 6 months (scenarii F to M)
# (either no surgery or surgery after 6 months): 
# --> we keep their observed outcomes and follow-up times
tab_control <-
  tab_control %>%
  mutate(outcome = 
           case_when(
             # case 1
             (surgery == 1 & timetosurgery <= 182.62) ~ 0L,
             # case 2
             (surgery == 0 | (surgery == 1 & timetosurgery > 182.62)) ~ death,
             TRUE ~ NA_integer_),
         fup = 
           case_when(
             # case 1
             (surgery == 1 & timetosurgery <= 182.62) ~ timetosurgery,
             # case 2
             (surgery == 0 | (surgery == 1 & timetosurgery > 182.62)) ~ fup_obs,
             TRUE ~ NA_real_))
################################################################################
# Arm "Surgery": surgery within 6 months
################################################################################
tab_surgery <- # We create a copy of the dataset: 
  tab %>%      # "clones" assigned to the surgery arm
  mutate(arm = "Surgery")
# Case 1: Patients receive surgery within 6 months (scenarii A to E): 
# --> we keep their observed outcomes and follow-up times
# Case 2: Patients die or are lost to follow-up before 6 months without having 
# surgery(scenarii K and L):
# --> we keep their observed outcomes and follow-up times
# Case 3: Patients do not receive surgery within 6 months and are still alive or 
# at risk at 6 months (scenarii F-J and M): 
# --> they are considered alive and their follow-up time is 6 months
tab_surgery <-
  tab_surgery %>%
  mutate(outcome = 
           case_when(
             # case 1
             (surgery == 1 & timetosurgery <= 182.62) ~ death,
             # case 2
             (surgery == 0 & fup_obs <= 182.62) ~ death,
             # case 3
             ((surgery == 0 & fup_obs > 182.62) |
                (surgery == 0 & timetosurgery > 182.62)) ~ 0L,
             TRUE ~ NA_integer_),
         fup = 
           case_when(
             # case 1
             (surgery == 1 & timetosurgery <= 182.62) ~ fup_obs,
             # case 2
             (surgery == 0 & fup_obs <= 182.62) ~ fup_obs,
             # case 3
             ((surgery == 0 & fup_obs > 182.62) |
                (surgery == 0 & timetosurgery > 182.62)) ~ 182.62,
             TRUE ~ NA_real_))

################################################################################
# Add censoring and fup_uncensored
################################################################################
# Note: Letters A, B... refer to the scenarios identified in our graph
# We will create two new variables:
# - fup_uncensored: the follow-up time uncensored in the trial arm 
#                   (can be shorter than the follow-up time in the outcome 
#                   model)
# - censoring: a binary variable indicating whether the patient was censored in 
#              a given arm (either because they receive surgery in the control 
#              arm or they didn't receive surgery in the surgery arm)
#THESE VARIABLES WILL BE THE OUTCOME AND FOLLOW UP TIME IN THE WEIGHT MODEL
################################################################################
# Arm "Control": no surgery within 6 months
################################################################################
# Case 1: Patients receive surgery within 6 months (scenarii A to E): 
# --> they are censored in the control group at time of surgery
# Case 2: Patients die or are lost to follow-up before 6 months without having 
# surgery (scenarii K and L):
# --> we keep their follow-up time but they are uncensored
# Case 3: Patients do not receive surgery within 6 months and are still alive or 
# at risk at 6 months (scenarii F-J and M): 
# --> they are considered uncensored and their follow-up time is 6 months
tab_control <-
  tab_control %>%
  mutate(censoring = 
           case_when(
             # case 1
             (surgery == 1 & timetosurgery <= 182.62) ~ 1L,
             # case 2
             (surgery == 0 & fup_obs <= 182.62) ~ 0L,
             # case 3
             ((surgery == 0 & fup_obs > 182.62) |
                (surgery == 0 & timetosurgery > 182.62)) ~ 0L,
             TRUE ~ NA_integer_),
         fup_uncensored = 
           case_when(
             # case 1
             (surgery == 1 & timetosurgery <= 182.62) ~ timetosurgery,
             # case 2
             (surgery == 0 & fup_obs <= 182.62) ~ fup_obs,
             # case 3
             ((surgery == 0 & fup_obs > 182.62) |
                (surgery == 0 & timetosurgery > 182.62)) ~ 182.62,
             TRUE ~ NA_real_))
################################################################################
# Arm "Surgery": surgery within 6 months
################################################################################
# Case 1: Patients receive surgery within 6 months (scenarii A to E): 
# --> they are uncensored in the surgery arm and remain at risk of censoring 
# until time of surgery
# Case 2: Patients die or are lost to follow-up before 6 months without having 
# surgery (scenarii K and L):
# --> we keep their follow-up time but they are uncensored
# Case 3: Patients do not receive surgery within 6 months and are still alive or 
# at risk at 6 months (scenarii F-J and M): 
# --> they are considered censored and their follow-up time is 6 months
tab_surgery <-
  tab_surgery %>%
  mutate(censoring = 
           case_when(
             # case 1
             (surgery == 1 & timetosurgery <= 182.62) ~ 0L,
             # case 2
             (surgery == 0 & fup_obs <= 182.62) ~ 0L,
             # case 3
             ((surgery == 0 & fup_obs > 182.62) |
                (surgery == 0 & timetosurgery > 182.62)) ~ 1L,
             TRUE ~ NA_integer_),
         fup_uncensored = 
           case_when(
             # case 1
             (surgery == 1 & timetosurgery <= 182.62) ~ timetosurgery,
             # case 2
             (surgery == 0 & fup_obs <= 182.62) ~ fup_obs,
             # case 3
             ((surgery == 0 & fup_obs > 182.62) |
                (surgery == 0 & timetosurgery > 182.62)) ~ 182.62,
             TRUE ~ NA_real_))
################################################################################
# Combine to one 'tab'
################################################################################
tab <- bind_rows(tab_control, tab_surgery)

################################################################################
# Splitting the data set at each time of event
################################################################################
# create vector of unique time points in 'tab'
t_events <- 
  tab %>% pull(fup) %>% unique() %>% sort()
################################################################################
# Arm "Control": no surgery within 6 months
################################################################################
# split the data set at each time of an event until the event happens
# the vector 't_events' is used to split the data, for each individual a row
# is added starting at 0 - t_events[1], to t_events[1] - t_events[2] etc. to
# the end of 'fup' (equal or less than max(t_events))
# the start of the interval is saved in column 'tstart', the end of the interval
# is saved in column 'fup', and the indicator of 
# whether or not an event occurred in a time interval is saved in column 
# 'outcome'
tab_c_long <-
  tab_control %>%
  survSplit(cut = t_events,
            end = "fup",
            event = "outcome")
# splitting the original data set at each time of event and sorting it
# until censoring happens. This is to have the censoring status at each time of
# event
tab_c_long_cens <-
  tab_control %>%
  survSplit(cut = t_events,
            end = "fup",
            event = "censoring") %>%
  select(id, tstart, fup, censoring)
tab_c_long <-
  tab_c_long %>%
  select(-censoring) %>%
  left_join(tab_c_long_cens) # no ID_t added now not sure where it's used for
################################################################################
# Arm "Surgery": surgery within 6 months
################################################################################
# split the data set at each time of an event until the event happens
# the vector 't_events' is used to split the data, for each individual a row
# is added starting at 0 - t_events[1], to t_events[1] - t_events[2] etc. to
# the end of 'fup' (equal or less than max(t_events))
# the start of the interval is saved in column 'tstart', the end of the interval
# is saved in column 'fup', and the indicator of 
# whether or not an event occurred in a time interval is saved in column 
# 'outcome'
tab_s_long <-
  tab_surgery %>%
  survSplit(cut = t_events,
            end = "fup",
            event = "outcome")
# splitting the original data set at each time of event and sorting it
# until censoring happens. This is to have the censoring status at each time of
# event
tab_s_long_cens <-
  tab_surgery %>%
  survSplit(cut = t_events,
            end = "fup",
            event = "censoring") %>%
  select(id, tstart, fup, censoring)
tab_s_long <-
  tab_s_long %>%
  select(-censoring) %>%
  left_join(tab_s_long_cens) # no ID_t added now

################################################################################
# Estimating the censoring weights
################################################################################
################################################################################
# Arm "Control": no surgery within 6 months
################################################################################
# Cox model
ms_c_cens <-
  coxph(Surv(tstart, fup, censoring) ~ 
          age + sex + emergency + stage + deprivation + charlson + perf,
        ties = "efron",
        data = tab_c_long)
# calculate baseline hazard (0 for time = 0)
basehazard <- 
  basehaz(ms_c_cens, centered = F) %>%
  add_row(hazard = 0, time = 0, .before = 1)
# preferably would do this using predict but predict(type = "lp") does not give
# the linear predictor that is needed here (probably because it's centered?)
lp <-
  {tab_c_long %>%
      select(age, sex, emergency, stage, deprivation, charlson, perf) %>%
      as.matrix} %*% coef(ms_c_cens) %>% as.vector()
tab_c_long <-
  tab_c_long %>%
  mutate(lin_pred = lp) %>%
  left_join(basehazard, by = c("tstart" = "time")) %>%
  mutate(p_uncens = exp(-(hazard)*exp(lin_pred)))
################################################################################
# Arm "Surgery": surgery within 6 months
################################################################################
# Cox model
ms_s_cens <-
  coxph(Surv(tstart, fup, censoring) ~ 
          age + sex + emergency + stage + deprivation + charlson + perf,
        ties = "efron",
        data = tab_s_long)
# calculate baseline hazard (0 for time = 0)
basehazard <- 
  basehaz(ms_s_cens, centered = F) %>%
  add_row(hazard = 0, time = 0, .before = 1)
# preferably would do this using predict but predict(type = "lp") does not give
# the linear predictor that is needed here (probably because it's centered?)
lp <-
  {tab_s_long %>%
      select(age, sex, emergency, stage, deprivation, charlson, perf) %>%
      as.matrix} %*% coef(ms_s_cens) %>% as.vector()
tab_s_long <-
  tab_s_long %>%
  mutate(lin_pred = lp) %>%
  left_join(basehazard, by = c("tstart" = "time")) %>%
  mutate(p_uncens = exp(-(hazard)*exp(lin_pred)))


################################################################################
# Computing the IPC weights
################################################################################
tab_long <- 
  bind_rows(tab_c_long, tab_s_long) %>%
  mutate(weight = 1 / p_uncens)

################################################################################
# Estimating the survivor function
################################################################################
# Emulated trial with Cox weights (Cox model)
Cox_w <- coxph(Surv(tstart, fup, outcome) ~ arm,
               data = tab_long, weights = weight)
HR <- exp(Cox_w$coefficients) #Hazard ratio
