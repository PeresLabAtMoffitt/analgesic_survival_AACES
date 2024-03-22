# Import library
library(tidyverse)
library(haven)


###################################################################### I ### Load data
path <- fs::path("", "Volumes", "Peres_Research", "AACES2", "Analgesic medications and survival")
analgesics <-
  read_sas(paste0(path, 
                  "/data/raw data/aaces_analgesics_mar18_24.sas7bdat"))

###################################################################### II ### Data cleaning
str(analgesics)

analgesics <- analgesics %>% 
  # Recode drugs variables
  mutate_at(c("aspirin", "nsaid", "aceta", "neoadj_treat"), 
            ~ case_when(
              . == 1                                          ~ "Yes",
              . == 2                                          ~ "No",
              . == 98                                         ~ "Not asked",
              . == 99                                         ~ NA_character_,
              . == 88                                         ~ NA_character_,
              TRUE                                            ~ as.character(.)
            )
  ) %>%
  mutate_at(c("aspirin", "nsaid", "aceta", "neoadj_treat"),
            ~ factor(., levels = c("No",
                                   "Yes"))) %>% 
  mutate_at(c("aspirin_ind", "nsaid_ind", "aceta_ind"), 
            ~ case_when(
              . == 0                                          ~ "No use",
              . == 1                                          ~ "Arthritis",
              . == 2                                          ~ "Menstrual Cramps",
              . == 3                                          ~ "Injury",
              . == 4                                          ~ "Surgical or dental pain",
              . == 5                                          ~ "Headache",
              . == 6                                          ~ "Other pain",
              . == 7                                          ~ "To prevent heart disease",
              . == 8                                          ~ "Other",
              TRUE                                            ~ as.character(.)
            )) %>% 
  mutate(across(c("aspirin_duration", "aspirin_freq"),
         ~ case_when(
           aspirin == "No"                                    ~ NA_real_,
           !is.na(.)                                          ~ .,
           TRUE                                               ~ NA_real_
  ), .names = "{.col}_for_users")) %>% 
  mutate(across(c("nsaid_duration", "nsaid_freq"),
         ~ case_when(
           nsaid == "No"                                      ~ NA_real_,
           !is.na(.)                                          ~ .,
           TRUE                                               ~ NA_real_
         ), .names = "{.col}_for_users")) %>%
  mutate(across(c("acetaminophen_duration", "acetaminophen_freq"),
         ~ case_when(
           aceta == "No"                                      ~ NA_real_,
           !is.na(.)                                          ~ .,
           TRUE                                               ~ NA_real_
         ), .names = "{.col}_for_users")) %>% 
  mutate(aspirin_duration_cat = case_when(
    aspirin == "No"                                           ~ "Not a user",
    aspirin_duration <= 60                                    ~ "≤5 years",
    !is.na(aspirin_duration)                                  ~ ">5 years",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(nsaid_duration_cat = case_when(
    nsaid == "No"                                             ~ "Not a user",
    nsaid_duration <= 60                                      ~ "≤5 years",
    !is.na(nsaid_duration)                                    ~ ">5 years",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(acetaminophen_duration_cat = case_when(
    aceta == "No"                                             ~ "Not a user",
    acetaminophen_duration <= 60                              ~ "≤5 years",
    !is.na(acetaminophen_duration)                            ~ ">5 years",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(across(ends_with("_duration_cat"), ~ factor(., 
                                                     levels = c(
                                                       "Not a user", 
                                                       "≤5 years", 
                                                       ">5 years"
                                                     ))
  )) %>% 
  mutate(aspirin_freq_cat = case_when(
    aspirin == "No"                                           ~ "Not a user",
    aspirin_duration < 30                                     ~ "<30",
    !is.na(aspirin_duration)                                  ~ "≥30",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(nsaid_freq_cat = case_when(
    nsaid == "No"                                             ~ "Not a user",
    nsaid_duration < 30                                       ~ "<30",
    !is.na(nsaid_duration)                                    ~ "≥30",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(acetaminophen_freq_cat = case_when(
    aceta == "No"                                             ~ "Not a user",
    acetaminophen_duration < 30                               ~ "<30",
    !is.na(acetaminophen_duration)                            ~ "≥30",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(across(ends_with("_freq_cat"), ~ factor(., 
                                                     levels = c(
                                                       "Not a user", 
                                                       "<30", 
                                                       "≥30"
                                                     ))
  )) %>% 
  mutate(aspirin_ind = case_when(
    aspirin == "No"                                           ~ "Not a user",
    aspirin_ind == "To prevent heart disease"                 ~ "Heart disease prevention",
    !is.na(aspirin_ind)                                       ~ "Other uses",
    TRUE                                                      ~ NA_character_
  ), aspirin_ind = factor(aspirin_ind, levels = c("Not a user",
                                                  "Heart disease prevention",
                                                  "Other uses"))
  ) %>%
  mutate(nsaid_ind = case_when(
    nsaid == "No"                                             ~ "Not a user",
    nsaid_ind == "Arthritis"                                  ~ "Arthritis",
    nsaid_ind == "Injury"                                     ~ "Injury",
    !is.na(nsaid_ind)                                         ~ "Other uses",
    TRUE                                                      ~ NA_character_
  ), nsaid_ind = factor(nsaid_ind, levels = c("Not a user",
                                              "Arthritis", "Injury",
                                              "Other uses"))
  ) %>%
  mutate(aceta_ind = case_when(
    aceta == "No"                                             ~ "Not a user",
    aceta_ind == "Arthritis"                                  ~ "Arthritis",
    aceta_ind == "Injury"                                     ~ "Injury",
    !is.na(aceta_ind)                                         ~ "Other uses",
    TRUE                                                      ~ NA_character_
  ), aceta_ind = factor(aceta_ind, levels = c("Not a user",
                                              "Arthritis", "Injury",
                                              "Other uses"))
  ) %>%
  mutate(stage = case_when(
    stage == 1                                                ~ "I",
    stage == 2                                                ~ "II",
    stage == 3                                                ~ "III",
    stage == 4                                                ~ "IV",
    stage == 9                                                ~ NA_character_
  ), stage = factor(stage, levels = c("I", "II", "III", "IV"))
  ) %>%
  mutate(menopause = case_when(
    menopause == 1                                            ~ "Premenopausal",
    menopause == 2                                            ~ "Postmenopausal"
  ), menopause = factor(menopause, levels = c("Premenopausal",
                                              "Postmenopausal"))
  ) %>%
  mutate(bmicat = case_when(
    bmicat == 1                                               ~ "≤18 kg/m²",
    bmicat == 2                                               ~ ">18 - ≤25 kg/m²",
    bmicat == 3                                               ~ ">25 - ≤30 kg/m²",
    bmicat == 4                                               ~ ">30 kg/m²"
  )) %>% 
  mutate(bmicat_2 = case_when(
    bmicat == "≤18 kg/m²" | 
      bmicat == ">18 - ≤25 kg/m²"                             ~ "≤25 kg/m²",
    is.na(bmicat)                                             ~ NA_character_,
    TRUE                                                      ~ bmicat
  ), bmicat_2 = factor(bmicat_2, levels = c("≤25 kg/m²",
                                            ">25 - ≤30 kg/m²",
                                            ">30 kg/m²"))) %>% 
  mutate(BMI_recent_grp = case_when(
    BMI_recent < 25                                           ~ "<25 kg/m²",
    BMI_recent >= 25 & 
      BMI_recent < 30                                         ~ "25-30 kg/m²",
    BMI_recent >= 30                                          ~ "≥30 kg/m²"
  ), BMI_recent_grp = factor(BMI_recent_grp, levels = c("<25 kg/m²",
                                                        "25-30 kg/m²",
                                                        "≥30 kg/m²"))) %>% 
  mutate(histotype = case_when(
    histotype == 1                                            ~ "high-grade serous",
    histotype == 2                                            ~ "low-grade serous",
    histotype == 3                                            ~ "endometrioid",
    histotype == 4                                            ~ "clear cell",
    histotype == 5                                            ~ "mucinous",
    histotype == 6                                            ~ "carcinosarcoma",
    histotype == 7                                            ~ "other epithelial ovarian cancer (e.g. Malignant Brenner, mixed, carcinoma, NOS)"
  )) %>% 
  mutate(histotype2 = case_when(
    histotype == "high-grade serous" | 
      histotype == "carcinosarcoma"                           ~ "HGSC/carcinosarcoma",
    !is.na(histotype)                                         ~ "not HGSC/carcinosarcoma"
  ), histotype2 = factor(histotype2, levels = c("not HGSC/carcinosarcoma",
                                                "HGSC/carcinosarcoma"))) %>%
  mutate(smokcurrent = case_when(
    smokcurrent == 2                                          ~ "Never",
    smokcurrent == 1                                          ~ "Current",
    smokcurrent == 3                                          ~ "Former"
  )) %>% 
  mutate(smokcurrent2 = case_when(
    smokcurrent == "Never"                                    ~ "Never",
    smokcurrent == "Current" | 
    smokcurrent == "Former"                                   ~ "Ever"
  ), smokcurrent2 = factor(smokcurrent2, levels = c("Never",
                                                    "Ever"))
  ) %>%
  mutate(paga = case_when(
    paga == 0                                                 ~ "No",
    paga == 1                                                 ~ "Yes"
  ), paga = factor(paga, levels = c("No",
                                    "Yes"))
  ) %>%
  mutate(site_2 = case_when(
    site == "IL" |
    site == "MI"|
    site == "NJ" |
    site == "OH"                                              ~ "North",
    
    site == "GA" |
    site == "NC" |
    site == "SC" |
    site == "TN"                                              ~ "Southeast",
    
    site == "AL" |
    site == "BA" |
    site == "LA"                                              ~ "Southwest",
  ), site_2 = factor(site_2)) %>% 
  mutate(site = case_when(
    site == "AL"                                              ~ "Alabama",
    site == "BA"                                              ~ "Texas",
    site == "GA"                                              ~ "Georgia",
    site == "IL"                                              ~ "Illinois",
    site == "LA"                                              ~ "Louisiana",
    site == "MI"                                              ~ "Michigan",
    site == "NC"                                              ~ "North Carolina",
    site == "NJ"                                              ~ "New Jersey",
    site == "OH"                                              ~ "Ohio",
    site == "SC"                                              ~ "South Carolina",
    site == "TN"                                              ~ "Tennessee"
  )) %>% 
  mutate(dblk_imp = case_when(
    dblk_imp == 1                                             ~ "Optimal",
    dblk_imp == 2                                             ~ "Suboptimal"
  )) %>% 
  mutate(NEW_dblkstat_treat_CA125 = case_when(
    NEW_dblkstat_treat_CA125 == 1                             ~ "Optimal",
    NEW_dblkstat_treat_CA125 == 2                             ~ "Suboptimal"
  ), NEW_dblkstat_treat_CA125 = factor(NEW_dblkstat_treat_CA125, 
                                       levels = c("Suboptimal",
                                                  "Optimal"))) %>% 
  mutate(CCI_new_Cat = case_when(
    CCI_new_Cat == "CCI=0"                                    ~ "CCI score is 0",
    CCI_new_Cat == "CCI=1"                                    ~ "CCI score is 1",
    CCI_new_Cat == "CCI=2+"                                   ~ "CCI score is 2+"
  ), CCI_new_Cat = factor(CCI_new_Cat, 
                          levels = c("CCI score is 0",
                                     "CCI score is 1",
                                     "CCI score is 2+"))
  ) %>% 
  # OS
  mutate(os_event = vital_status_fin) %>% 
  mutate(vital_status_fin = case_when(
    vital_status_fin == 0                                     ~ "Alive",
    vital_status_fin == 1                                     ~ "Deceased"
  )) %>% 
  rename(os_time = days_int_to_event)
  
check_data <- function(data){

  for(i in 1:length(colnames(data))) {

    if(class(data[[i]]) == "factor" | class(data[[i]]) == "character") {

      # print(data[i])
      print(colnames(data[i]))
      print(table(data[[i]]))
    }

  }
}

check_data(analgesics)

# write_rds(analgesics, paste0(here::here(),
#                              "/Cleaned analgesics medication data_03212024.rds"))
# write_csv(analgesics, paste0(path,
#                   "/data/processed data/Cleaned analgesics medication data_03212024.csv"))
# write_rds(analgesics, paste0(path,
#                              "/data/processed data/Cleaned analgesics medication data_03212024.rds"))

# restrict data to patients with analgesics data
analgesics <- analgesics %>% 
  filter(!is.na(aspirin))

###################################################################### III ### Imputation for debulking
library(mice)
library(survival)

mice_data <- analgesics %>% 
  select(suid, os_time, os_event, 
         refage, stage, 
         histotype2, NEW_dblkstat_treat_CA125,
         BMI_recent_grp, smokcurrent2, menopause,
         CCI_new_Cat, site_2, 
         aspirin, 
         nsaid, 
         aceta, 
         neoadj_treat, paga)

nonimp_cox_model <-
  coxph(Surv(time = analgesics$os_time, 
             event = analgesics$os_event) ~ aspirin + 
          nsaid + aceta + refage + site_2 + stage + histotype2 + 
          NEW_dblkstat_treat_CA125 +
          BMI_recent_grp + smokcurrent2 + CCI_new_Cat +
          neoadj_treat + paga,
        data = mice_data)
nonimp_cox_model

Hazard <- nelsonaalen(mice_data, os_time, os_event)
dataset <- data.frame(mice_data, Hazard)

# quick imputation o look at the predictor matrix
Cox.imp <- mice(dataset, m=1, maxit=0, seed=123, printFlag=F)
Cox.imp
Pred <- Cox.imp$predictorMatrix
# Pred["debulking", "month_at_os_from_treatment"] <- 0
Pred # Warning all categorical var need to be factors

#  Reading across the rows of the predictor matrix, 
# 1 means the predictor in that column is included 
# in the imputation model for that row, 
# else 0 means it is not included. 

# Patient ids must not be used as predictor. But SURGERYDATE IS NOT.
Pred[, "suid"] <- 0
Pred["suid",] <- 0
Pred


# Start imputations using mice
Cox.imp <- mice(dataset, m=5, maxit=50, 
                predictorMatrix=Pred,
                seed=123, printFlag=F)
check1 <- with(data = Cox.imp,
               exp = coxph(
                 Surv(time = analgesics$os_time,
                      event = analgesics$os_event) ~ aspirin + 
                   nsaid + aceta + refage + site_2 + stage + histotype2 + 
                   NEW_dblkstat_treat_CA125 +
                   BMI_recent_grp + smokcurrent2 + CCI_new_Cat +
                   neoadj_treat + paga
               ))
# pool(check1)$pooled
multiple_imputations_number <- round(max(pool(check1)$pooled$fmi),2)*100

# Do m = `r multiple_imputations_number` (how to choose is explained in
# https://www.ebpi.uzh.ch/dam/jcr:dc0cef17-29c7-4e61-8d33-e690561ab7ae/mi_intro20191001.pdf depends of the fmi. 
# The max fmi = `r multiple_imputations_number / 100` that we multiply by 100)
Cox.imp <- mice(dataset, m= multiple_imputations_number, maxit=50,
                predictorMatrix=Pred,
                seed=123, printFlag=F)
write_rds(Cox.imp, "Cox.imp_with m number and pred matrix.rds")
Cox.imp <- read_rds(paste0(here::here(), "/Cox.imp_with m number and pred matrix.rds"))

fit.Cox <-
  with(data = Cox.imp,
       exp = coxph(
         Surv(time = analgesics$os_time,
              event = analgesics$os_event) ~ aspirin + 
           nsaid + aceta + refage + site_2 + stage + histotype2 + 
           NEW_dblkstat_treat_CA125 +
           BMI_recent_grp + smokcurrent2 + CCI_new_Cat +
           neoadj_treat + paga
       ))

summary(pool(fit.Cox), conf.int = TRUE, exponentiate = TRUE) %>%
  kableExtra::kable() %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
# Above results are exponentiated (imputed data).  
# As a reminder before imputation results are :
nonimp_cox_model

# Complete the chemo data and merge with original
imputed_data <- complete(Cox.imp, include = FALSE) %>% 
  `colnames<-`(paste0("imp_", colnames(.)))
  # rename(imp_stage = stage, imp_debulking = NEW_dblkstat_treat_CA125)

analgesics <- 
  full_join(analgesics, imputed_data,# %>% 
              # select(suid, starts_with("imp_")), 
            by = c("suid" = "imp_suid")) 

write_rds(analgesics, paste0(here::here(), 
                             "/Cleaned imputed analgesics medication data_03212024.rds"))
write_csv(analgesics, paste0(path, 
                             "/data/processed data/Cleaned imputed analgesics medication data_03212024.csv"))
write_rds(analgesics, paste0(path, 
                             "/data/processed data/Cleaned imputed analgesics medication data_03212024.rds"))

# End

