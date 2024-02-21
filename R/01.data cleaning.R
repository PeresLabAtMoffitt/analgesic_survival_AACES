# Import library
library(tidyverse)
library(haven)


###################################################################### I ### Load data
path <- fs::path("", "Volumes", "Peres_Research", "AACES2", "Analgesic medications and survival")
analgesics <-
  read_sas(paste0(path, 
                  "/data/raw data/aaces_analgesics_feb2024.sas7bdat"))

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
            )) %>% 
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
  mutate(stage = case_when(
    stage == 1                                                ~ "I",
    stage == 2                                                ~ "II",
    stage == 3                                                ~ "III",
    stage == 4                                                ~ "IV",
    stage == 9                                                ~ NA_character_
  )) %>% 
  mutate(menopause = case_when(
    menopause == 1                                            ~ "Premenopausal",
    menopause == 2                                            ~ "Postmenopausal",
  )) %>% 
  mutate(bmicat = case_when(
    bmicat == 1                                               ~ "≤18 kg/m²",
    bmicat == 2                                               ~ ">18 - ≤25 kg/m²",
    bmicat == 3                                               ~ ">25 - ≤30 kg/m²",
    bmicat == 4                                               ~ " >30 kg/m²",
  )) %>% 
  mutate(histotype = case_when(
    histotype == 1                                            ~ "high-grade serous",
    histotype == 2                                            ~ "low-grade serous",
    histotype == 3                                            ~ "endometrioid",
    histotype == 4                                            ~ "clear cell",
    histotype == 5                                            ~ "mucinous",
    histotype == 6                                            ~ "carcinosarcoma",
    histotype == 7                                            ~ "other epithelial ovarian cancer (e.g. Malignant Brenner, mixed, carcinoma, NOS)",
  )) %>% 
  mutate(histotype2 = case_when(
    histotype == "high-grade serous"                          ~ "high-grade serous",
    TRUE                                                      ~ "non-high-grade serous"
  )) %>%
  mutate(smokcurrent = case_when(
    smokcurrent == 2                                          ~ "Never",
    smokcurrent == 1                                          ~ "Current",
    smokcurrent == 3                                          ~ "Former",
  )) %>% 
  mutate(smokcurrent2 = case_when(
    smokcurrent == "Never"                                    ~ "Never",
    smokcurrent == "Current" | 
    smokcurrent == "Former"                                   ~ "Ever",
  )) %>% 
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
  )) %>% 
  mutate(dblk_imp = case_when(
    dblk_imp == 1                                             ~ "Optimal",
    dblk_imp == 2                                             ~ "Suboptimal",
  )) %>% 
  mutate(NEW_dblkstat_treat_CA125 = case_when(
    NEW_dblkstat_treat_CA125 == 1                             ~ "Optimal",
    NEW_dblkstat_treat_CA125 == 2                             ~ "Suboptimal",
  )) %>% 
  mutate(CCI_new_Cat = case_when(
    CCI_new_Cat == "CCI=0"                                    ~ "CCI score is 0",
    CCI_new_Cat == "CCI=1"                                    ~ "CCI score is 1",
    CCI_new_Cat == "CCI=2+"                                   ~ "CCI score is 2+",
  )) %>% 
  # OS
  mutate(os_event = vital_status_fin) %>% 
  mutate(vital_status_fin = case_when(
    vital_status_fin == 0                                     ~ "Alive",
    vital_status_fin == 1                                     ~ "Deceased",
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

write_csv(analgesics, paste0(here::here(), "/Cleaned analgesics medication data_02152024.csv"))
write_csv(analgesics, paste0(path, 
                  "/data/processed data/Cleaned analgesics medication data_02152024.csv"))
# End

