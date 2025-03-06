# For text values not present in tables

# Import library
library(tidyverse)
library(gtsummary)
theme_gtsummary_compact()

# Load data
path <- fs::path("", "Volumes", "Peres_Research", "AACES2", "Analgesic medications and survival")
analgesics <-
  read_rds(paste0(here::here(), "/Cleaned imputed analgesics medication data_phase1 w income insurance education_05012024.rds")) %>% 
  rename(imp_debulking = imp_NEW_dblkstat_treat_CA125)

# First sentence in "Analgesic medication use"
analgesics %>% 
  select(
    nsaid, 
    aspirin, 
    aceta) %>% 
  tbl_summary(
    statistic=list(all_continuous() ~ "{median} ({min}, {max})"
    )) %>% 
  bold_labels() %>% add_stat_label()

# Create cat excluding non users
analgesics_wo_nousers <-
  analgesics %>% 
  mutate(aspirin_duration_cat = case_when(
    aspirin == "No"                                           ~ NA_character_,
    aspirin_duration <= 60                                    ~ "≤5 years",
    !is.na(aspirin_duration)                                  ~ ">5 years",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(nsaid_duration_cat = case_when(
    nsaid == "No"                                             ~ NA_character_,
    nsaid_duration <= 60                                      ~ "≤5 years",
    !is.na(nsaid_duration)                                    ~ ">5 years",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(acetaminophen_duration_cat = case_when(
    aceta == "No"                                             ~ NA_character_,
    acetaminophen_duration <= 60                              ~ "≤5 years",
    !is.na(acetaminophen_duration)                            ~ ">5 years",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(across(ends_with("_duration_cat"), ~ factor(., 
                                                     levels = c(
                                                       ">5 years",
                                                       "≤5 years"
                                                     ))
  )) %>% 
  mutate(aspirin_freq_cat = case_when(
    aspirin == "No"                                           ~ NA_character_,
    aspirin_duration < 30                                     ~ "<30",
    !is.na(aspirin_duration)                                  ~ "≥30",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(nsaid_freq_cat = case_when(
    nsaid == "No"                                             ~ NA_character_,
    nsaid_duration < 30                                       ~ "<30",
    !is.na(nsaid_duration)                                    ~ "≥30",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(acetaminophen_freq_cat = case_when(
    aceta == "No"                                             ~ NA_character_,
    acetaminophen_duration < 30                               ~ "<30",
    !is.na(acetaminophen_duration)                            ~ "≥30",
    TRUE                                                      ~ NA_character_
  )) %>% 
  mutate(across(ends_with("_freq_cat"), ~ factor(., 
                                                 levels = c(
                                                   "≥30",
                                                   "<30"
                                                 ))
  ))

analgesics_wo_nousers %>% 
  select(
    aspirin_freq_cat, 
    nsaid_freq_cat, 
    acetaminophen_freq_cat,
    aspirin_duration_cat, 
    nsaid_duration_cat, 
    acetaminophen_duration_cat
  ) %>% 
  tbl_summary(
    statistic=list(all_continuous() ~ "{median} ({min}, {max})"), missing = "no" 
    ) %>% 
  bold_labels() %>% add_stat_label()







