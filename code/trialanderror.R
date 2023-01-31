data <- 
tibble(patient_id = c(1, 2, 3),
       any_treatment_strategy_cat = c("Treated", "Untreated", "Untreated"),
       tb_postest_treat = c(2, NA_real_, 26),
       status_primary_simple = c("1", "1", "0") %>% factor(levels = c("0", "1")),
       fu_primary = c(1, 20, 27))

data_control <- 
  data %>%
  mutate(arm = "Control",
         # ADD VARIABLES OUTCOME AND FUP
         outcome = case_when(
           # Case 1: patients receive treatment within 5 days (scenarios A to E)
           # --> they are still alive and followed up until treatment
           any_treatment_strategy_cat != "Untreated" &
             tb_postest_treat <= 4 ~ "0" %>% 
             factor(levels = c("0", "1")),
           # Case 2: patients do not receive treatment within 5 days (scenarios F to M)
           # [either no treatment or treatment after five days]
           # --> we keep their observed outcomes and follow-up times
           any_treatment_strategy_cat == "Untreated" |
             (any_treatment_strategy_cat != "Untreated" &
                tb_postest_treat > 4) ~ status_primary_simple,
         ),
         fup = case_when(
           # Case 1: patients receive treatment within 5 days (scenarios A to E)
           # --> they are still alive and followed up until treatment
           any_treatment_strategy_cat != "Untreated" &
             tb_postest_treat <= 4 ~ tb_postest_treat,
           # Case 2: patients do not receive treatment within 5 days (scenarios F to M)
           # [either no treatment or treatment after five days]
           # --> we keep their observed outcomes and follow-up times
           any_treatment_strategy_cat == "Untreated" |
             (any_treatment_strategy_cat != "Untreated" &
                tb_postest_treat > 4) ~ fu_primary,
         ),
         # ADD VARIABLES CENSORING AND FUP_UNCENSORED
         censoring = case_when(
           # Case 1: Patients receive treatment within 5 days (scenarios A to E)
           # --> they are censored in the control group at time of treatment
           any_treatment_strategy_cat != "Untreated" &
             tb_postest_treat <= 4 ~ "1" %>% factor(levels = c("0", "1")),
           # Case 2: Patients die or are lost to follow-up within 5 days 
           # addition by LN: and are not treated within 5 days (scenarios K and L)
           # --> we keep their follow-up time but they are uncensored
           (any_treatment_strategy_cat == "Untreated" & 
              tb_postest_treat <= 4 &
              fu_primary <= 4) ~ "0" %>% factor(levels = c("0", "1")),
           # Case 3: Patients do not receive treatment within 5 days
           # and are still alive or at risk at 5 days (scenarios F-J and M): 
           # --> they are considered uncensored and their follow-up time is 
           #     5 days
           (any_treatment_strategy_cat == "Untreated" &
              fu_primary > 4) |
             (any_treatment_strategy_cat != "Untreated" &
                # NB if people treated > 4 days, fu_primary is > 4 days
                tb_postest_treat > 4) ~ "0" %>% factor(levels = c("0", "1")),
         ),
         fup_uncensored = case_when(
           # Case 1: Patients receive treatment within 5 days (scenarios A to E)
           # --> they are censored in the control group at time of treatment
           any_treatment_strategy_cat != "Untreated" &
             tb_postest_treat <= 4 ~ tb_postest_treat,
           # Case 2: Patients die or are lost to follow-up within 5 days 
           # addition by LN: without being treated (scenarios K and L)
           # --> we keep their follow-up time but they are uncensored
           any_treatment_strategy_cat == "Untreated" & 
             tb_postest_treat <= 4 &
             fu_primary <= 4 ~ fu_primary,
           # Case 3: Patients do not receive treatment within 5 days
           # and are still alive or at risk at 5 days (scenarios F-J and M): 
           # --> they are considered uncensored and their follow-up time is 
           #     5 days
           (any_treatment_strategy_cat == "Untreated" &
              fu_primary > 4) |
             (any_treatment_strategy_cat == "Treated" &
                # NB if people treated > 4 days, fu_primary is > 4 days
                tb_postest_treat > 4) ~ 5,
         ),
  )
###################################################
# ARM TREATMENT: treatment within 5 days
###################################################
data_trt <- 
  data %>%
  mutate(arm = "Treatment",
         # ADD VARIABLES OUTCOME AND FUP
         outcome = case_when(
           # Case 1: Patients receive treatment within 5 days (scenarios A to E)
           # --> we keep their observed outcomes and follow-up times
           any_treatment_strategy_cat != "Untreated" &
             tb_postest_treat <= 4 ~ status_primary_simple,
           # Case 2: Patients die or are lost to follow-up within 5 days
           # without being treated (scenarios K and L)
           # --> we keep their observed outcomes and follow-up times
           any_treatment_strategy_cat == "Untreated" &
             fu_primary <= 4 ~ status_primary_simple,
           # Case 3: Patients do not receive treatment within 5 days
           # and are still alive or at risk at 5 days (scenarios F-J and M)
           # --> they don't experience an event and their follow-up time is 5 
           #     days
           (any_treatment_strategy_cat == "Untreated" &
              fu_primary > 4) |
             (any_treatment_strategy_cat != "Untreated" & 
                tb_postest_treat > 4) ~ "0" %>% 
             factor(levels = c("0", "1", "2")),
         ),
         fup = case_when(
           # Case 1: Patients receive treatment within 5 days (scenarios A to E)
           # --> we keep their observed outcomes and follow-up times
           any_treatment_strategy_cat != "Untreated" &
             tb_postest_treat <= 4 ~ fu_primary,
           # Case 2: Patients die or are lost to follow-up within 5 days
           # without being treated (scenarios K and L)
           # --> we keep their observed outcomes and follow-up times
           any_treatment_strategy_cat == "Untreated" &
             fu_primary <= 4 ~ fu_primary,
           # Case 3: Patients do not receive treatment within 5 days
           # and are still alive or at risk at 5 days (scenarios F-J and M)
           # --> they don't experience an event and their follow-up time is 5 
           #     days
           (any_treatment_strategy_cat == "Untreated" &
              fu_primary > 4) |
             (any_treatment_strategy_cat != "Untreated" & 
                tb_postest_treat > 4) ~ 5,
         ),
         # ADD VARIALBES CENSORING AND FUP_UNCENSORED
         censoring = case_when(
           # Case 1: Patients receive treatment within 5 days (scenarios A to E): 
           # --> they are uncensored in the treatment arm and remain at risk of 
           #     censoring until time of treatment
           any_treatment_strategy_cat != "Untreated" & 
             tb_postest_treat <= 4 ~ "0" %>% 
             factor(levels = c("0", "1")),
           # Case 2: Patients die or are lost to follow-up within 5 days
           # without being treated (scenarios K and L)
           # --> we keep their follow-up times but they are uncensored
           any_treatment_strategy_cat == "Untreated" & 
             fu_primary <= 4 ~ "0" %>%
             factor(levels = c("0", "1")),
           # Case 3: Patients do not receive treatment within 5 days and are 
           # still alive or at risk at 5 days (scenarios F-J and M): 
           # --> they are considered censored and their follow-up time is 5 days
           (any_treatment_strategy_cat == "Untreated" &
              fu_primary > 4) | 
             (any_treatment_strategy_cat != "Untreated" &
                tb_postest_treat > 4) ~ "1" %>% 
             factor(levels = c("0", "1")),
         ),
         fup_uncensored = case_when(
           # Case 1: Patients receive treatment within 5 days (scenarios A to E): 
           # --> they are uncensored in the treatment arm and remain at risk of 
           #     censoring until time of treatment
           any_treatment_strategy_cat != "Untreated" & 
             tb_postest_treat <= 4 ~ tb_postest_treat,
           # Case 2: Patients die or are lost to follow-up within 5 days
           # without being treated (scenarios K and L)
           # --> we keep their follow-up times but they are uncensored
           any_treatment_strategy_cat == "Untreated" & 
             fu_primary <= 4 ~ fu_primary,
           # Case 3: Patients do not receive treatment within 5 days and are 
           # still alive or at risk at 5 days (scenarios F-J and M): 
           # --> they are considered censored and their follow-up time is 5 days
           (any_treatment_strategy_cat == "Untreated" &
              fu_primary > 4) | 
             (any_treatment_strategy_cat != "Untreated" &
                tb_postest_treat > 4) ~ 5,
         ),
  )
###################################################
# CREATION OF THE FINAL DATASET FOR THE ANALYSIS
###################################################
data_cloned <-
  rbind(data_control,
        data_trt)

################################################################################
# STEP 2-SPLITTING THE DATASET AT EACH TIME OF EVENT
################################################################################
# Dataframe 'times' containing the time of events and an ID for the times of 
# events
t_events <- sort(unique(data_cloned$fup))

data_outcome <- 
  data_cloned %>%
  mutate(t_start = 0) %>%
  survSplit(cut = t_events,
            end = "fup",
            start = "t_start",
            event = "outcome") %>%
  mutate(outcome = case_when(
    outcome == "censor" ~ "0",
    TRUE ~ "1",
  ) %>% factor(levels = c("0", "1"))
  )
# LN QU: Don't get why we're using fup and not fup_censoring?????
# fup_censoring is not used in the script...? --> seems odd
data_cnsr <-
  data_cloned %>%
  mutate(t_start = 0) %>%
  survSplit(cut = t_events,
            end = "fup",
            start = "t_start",
            event = "censoring") %>%
  mutate(censoring = case_when(
    censoring == "censor" ~ "0",
    TRUE ~ "1",
  ) %>% factor(levels = c("0", "1"))
  )

data_final <-
  data_outcome %>%
  left_join(data_cnsr,
            by = c("arm", "patient_id", "fup", "t_start")) %>%
  select(-c("outcome.y", "censoring.x")) %>%
  rename(outcome = outcome.x,
         censoring = censoring.y,
         t_end = fup)

data_final %>% select(patient_id, arm, t_start, t_end, any_treatment_strategy_cat.x,
                      outcome, censoring) %>% View()

