# Import library
library(tidyverse)
library(haven)


###################################################################### I ### Load data
path <- fs::path("", "Volumes", "Peres_Research", "AACES2", "Analgesic medications and survival")
# analgesics_phase1 <-
#   read_sas(paste0(path, 
#                   "/data/raw data/aaces_analgesics_mar18_24.sas7bdat"))

analgesics_phase2 <-
  read_sas(paste0(path, 
                  "/data/raw data/analgesics_with_phase2.sas7bdat"))
new_var <-
  read_sas(paste0(path, 
                  "/data/raw data/aaces_analgesics_apr24_24.sas7bdat"))

######################################################################
analgesics <- analgesics_phase2 %>% 
  select(suid, AACES_phase = phase, everything()) %>% 
  full_join(., new_var %>% 
              select(suid, 
                     QV6, num_family, 
                     ins2, education), 
            by = "suid")


###################################################################### II ### Data cleaning
str(analgesics)

analgesics <- analgesics %>% 
  # Recode drugs variables
  mutate(across(where(is.character), ~ na_if(., ""))) %>% 
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
  mutate_at(c("aspirin_ind", "nsaid_ind", "aceta_ind",
              "aspirin_ind_mult1", "aspirin_ind_mult2",
              "nsaid_ind_mult1", "nsaid_ind_mult2",
              "nsaid_ind_mult3", "nsaid_ind_mult4",
              "aceta_ind_mult1", "aceta_ind_mult2",
              "aceta_ind_mult3"), 
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
  mutate(aspirin_ind_init = aspirin_ind) %>% 
  mutate(aspirin_ind = case_when(
    aspirin == "No"                                           ~ "Not a user",
    aspirin_ind == "To prevent heart disease"                 ~ "Heart disease prevention",
    !is.na(aspirin_ind)                                       ~ "Other uses",
    TRUE                                                      ~ NA_character_
  ), aspirin_ind = factor(aspirin_ind, levels = c("Not a user",
                                                  "Heart disease prevention",
                                                  "Other uses"))
  ) %>%
  mutate(nsaid_ind_init = nsaid_ind) %>% 
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
  mutate(aceta_ind_init = aceta_ind) %>% 
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
  mutate(income = case_when(
    QV6 == 1 |
    QV6 == 2 ~ "< $25,000",
    QV6 == 3 |
    QV6 == 4 ~ "$25,000-$74,999",
    QV6 == 5 |
    QV6 == 6 ~ "≥ $75,000"
  ), income = factor(income, 
                     levels = c("< $25,000",
                                "$25,000-$74,999",
                                "≥ $75,000"))) %>% 
  mutate(QV6 = case_when(
    QV6 == 1 ~ "less than $10,000",
    QV6 == 2 ~ "$10,000-$24,999",
    QV6 == 3 ~ "$25,000-$49,999",
    QV6 == 4 ~ "$50,000-$74,999",
    QV6 == 5 ~ "$75,000-$100,000",
    QV6 == 6 ~ "More than $100,000"
  )) %>% 
  
  mutate(num_family = as.character(num_family)) %>% 
  mutate(ins2 = case_when(
    ins2 == 0 ~ "None",
    ins2 == 1 ~ "Medicaid",
    ins2 == 2 ~ "Medicare only",
    ins2 == 3 ~ "Private & medicare",
    ins2 == 4 ~ "Private",
    ins2 == 5 ~ "Other"
  )) %>% 
  mutate(private_insurance = case_when(
    str_detect(ins2, "Private|private")    ~ "Yes",
    !is.na(ins2)                           ~ "No"
  )) %>% 
  mutate(medicare_insurance = case_when(
    str_detect(ins2, "Medicare|medicare")  ~ "Yes",
    !is.na(ins2)                           ~ "No"
  )) %>% 
  mutate(medicaid_insurance = case_when(
    str_detect(ins2, "Medicaid")           ~ "Yes",
    !is.na(ins2)                           ~ "No"
  )) %>% 
  
  mutate(insurance = case_when(
    ins2 == "Private & medicare" |
    ins2 == "Private"               ~ "Private",
    # ins2 == "Medicare only"         ~ "Medicare only",
    # ins2 == "Medicaid" | 
    #   ins2 == "Other"               ~ "Other insurance",
    # ins2 == "None"                  ~ "None"
    TRUE                            ~ ins2
  ), insurance = factor(insurance, 
                        levels = c("None",
                                   "Medicare only", "Medicaid",
                                   "Private",
                                   "Other"))) %>% 
  mutate(insurance_yn = case_when(
    ins2 == "Private & medicare" |
      ins2 == "Private"               ~ "Yes",
    ins2 == "Medicare only"         ~ "Yes",
    ins2 == "Medicaid" | 
      ins2 == "Other"               ~ "Yes",
    ins2 == "None"                  ~ "No"
  )) %>% 
  mutate(across(c(insurance_yn, private_insurance,
                  medicare_insurance, medicaid_insurance), 
                ~ factor(., 
                         levels = c("No",
                                    "Yes")))) %>% 
  
  mutate(education = case_when(
    education == 1 ~ "high school graduate/GED or less",
    education == 2 |# ~ "some college",
    education == 3 |# ~ "college graduate",
    education == 4 ~ "Others" #"graduate/professional school"
  ), education = factor(education, 
                        levels = c("high school graduate/GED or less",
                                   "Others"))) %>% 

  mutate(site_2 = case_when(
    site == "IL" |
      site == "MI"|
      site == "NJ" |
      site == "OH"                                            ~ "North",
    
    site == "GA" |
      site == "NC" |
      site == "SC" |
      site == "TN"                                            ~ "Southeast",
    
    site == "AL" |
      site == "BA" |
      site == "CA" |
      site == "LA"                                            ~ "Southwest",
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
    site == "TN"                                              ~ "Tennessee",
    site == "CA"                                              ~ "California"
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
  filter(AACES_phase == 1) %>% 
  filter(!is.na(aspirin))

###################################################################### III ### Imputation for debulking
library(mice)
library(survival)

# Create a data with the predictor variables and output variables
mice_data <- analgesics %>% 
  select(suid, os_time, os_event, 
         refage, stage, 
         histotype2, NEW_dblkstat_treat_CA125,
         BMI_recent_grp, smokcurrent2, menopause,
         CCI_new_Cat, site_2, 
         aspirin, nsaid, aceta, 
         neoadj_treat, paga,
         private_insurance, medicare_insurance,
         medicaid_insurance, income,
         education)

# Run a first model to have baseline estimate 
# (will compare with them later - imputation shouldn't change estimate)
nonimp_cox_model <-
  coxph(Surv(time = mice_data$os_time, 
             event = mice_data$os_event) ~ aspirin + 
          nsaid + aceta + refage + site_2 + stage + histotype2 + 
          NEW_dblkstat_treat_CA125 +
          BMI_recent_grp + smokcurrent2 + CCI_new_Cat +
          neoadj_treat + paga + private_insurance + medicare_insurance +
          medicaid_insurance + income + education,
        data = mice_data)
nonimp_cox_model

# For cox proportional hazards regression
# include two variables related to the survival endpoint in the imputation models, 
# the Nelson-Aalen estimate of the cumulative hazard (nelsonaalen()) and the event indicator,
# in the imputation process
Hazard <- nelsonaalen(mice_data, os_time, os_event)
dataset <- data.frame(mice_data, Hazard)

# quick imputation o look at the predictor matrix
# Need to make sure of 1) the method used for imputation and 
# the variables used or not used as predictors
# For example, patient ids need to not the used as predictors
Cox.imp <- mice(dataset, m=1, maxit=0, seed=123, printFlag=F)
Cox.imp
Pred <- Cox.imp$predictorMatrix
Pred # Warning all categorical var need to be factors

#  Reading across the rows of the predictor matrix, 
# 1 means the predictor in that column is included 
# in the imputation model for that row, 
# else 0 means it is not included. 

# Patient ids must not be used as predictor. 
Pred[, "suid"] <- 0
Pred["suid",] <- 0
Pred


# Start imputations using mice
Cox.imp <- mice(dataset, m=5, maxit=50, 
                predictorMatrix=Pred,
                seed=123, printFlag=F)
check1 <- with(data = Cox.imp,
               exp = coxph(
                 Surv(time = mice_data$os_time,
                      event = mice_data$os_event) ~ aspirin + 
                   nsaid + aceta + refage + site_2 + stage + histotype2 + 
                   NEW_dblkstat_treat_CA125 +
                   BMI_recent_grp + smokcurrent2 + CCI_new_Cat +
                   neoadj_treat + paga + income + private_insurance + 
                   medicare_insurance + medicaid_insurance + education
               ))
# pool(check1)$pooled
multiple_imputations_number <- round(max(pool(check1)$pooled$fmi),2)*100

# Do m = `r multiple_imputations_number` (how to choose is explained in
# https://www.ebpi.uzh.ch/dam/jcr:dc0cef17-29c7-4e61-8d33-e690561ab7ae/mi_intro20191001.pdf depends of the fmi. 
# The max fmi = `r multiple_imputations_number / 100` that we multiply by 100)
Cox.imp <- mice(dataset, m= multiple_imputations_number, maxit=50,
                predictorMatrix=Pred,
                seed=123, printFlag=F)
write_rds(Cox.imp, "Cox.imp_with m number and pred matrix_phase1 w income 3insurance education_05302024.rds")
Cox.imp <- 
  read_rds(paste0(
    here::here(), 
    "/Cox.imp_with m number and pred matrix_phase1 w income 3insurance education_05302024.rds"))

fit.Cox <-
  with(data = Cox.imp,
       exp = coxph(
         Surv(time = mice_data$os_time,
              event = mice_data$os_event) ~ aspirin + 
           nsaid + aceta + refage + site_2 + stage + histotype2 + 
           NEW_dblkstat_treat_CA125 +
           BMI_recent_grp + smokcurrent2 + CCI_new_Cat +
           neoadj_treat + paga + income + private_insurance + 
           medicare_insurance + medicaid_insurance + education
       ))

summary(pool(fit.Cox), conf.int = TRUE, exponentiate = TRUE) %>%
  kableExtra::kable() %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
# Above results are exponentiated (imputed data).  
# As a reminder before imputation results are :
nonimp_cox_model

# Complete the data and merge with original
imputed_data <- complete(Cox.imp, include = FALSE) %>% 
  `colnames<-`(paste0("imp_", colnames(.)))
  # rename(imp_stage = stage, imp_debulking = NEW_dblkstat_treat_CA125)

analgesics <- 
  full_join(analgesics, imputed_data, #%>% 
              # select(suid, starts_with("imp_")),
            by = c("suid" = "imp_suid")) 

write_rds(analgesics, paste0(here::here(), 
                             "/Cleaned imputed analgesics medication data_phase1 w income insurance education_05302024.rds"))
write_csv(analgesics, paste0(path, 
                             "/data/processed data/Cleaned imputed analgesics medication data_phase1 w income insurance education_05302024.csv"))
write_rds(analgesics, paste0(path, 
                             "/data/processed data/Cleaned imputed analgesics medication data_phase1 w income insurance education_05302024.rds"))

# End

