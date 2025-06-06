# ==========================================================================================================================================
## Script Name: Data Cleaning: Men's Cross-Sectional Data from Ibadan and Kano
## Purpose: Assigns variable names and does data cleaning of cross-sectional men's Ibadan/Kano IBLS data 
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")
install.packages("readstata13")
library(readstata13)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Kano Data
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in data
kano_dry <- read.dta13(file.path(CSMenDataDirDry, "kano_dryseason_wide_data.dta"))
kano_wet <- read.dta13(file.path(CSMenDataDirWet, "kano_wetseason_wide_data_final.dta"))

kano_wet <- kano_wet %>% 
  dplyr::select(-ward, -lga) %>% 
  rename(
    lga = bi1_men,
    ward = bi2_men,
    settlement_type = bi3_men,
    community_name = bi4_men,
    ea = bi5_men,
    household_num = bi6_men,
    longitude = bi7_long_men,
    latitude = bi7_lat_men,
    age = q201a,
    dob = q201b,
    date = bi9_men,
    two_wk_fever = q401,
    test_result = q702
  ) %>%
  dplyr::select(sn, dry_sn, lga, ward, settlement_type, community_name, ea, 
                household_num, age, dob, longitude, latitude, date, two_wk_fever, test_result) %>% 
  dplyr::filter(ward != 3) %>% 
  mutate(
    ward = case_when(
      ward == 1 ~ "Zango",
      ward == 2 ~ "Dorayi",
      ward == 4 ~ "Fagge D2",
      ward == 5 ~ "Gobirawa",
      ward == 6 ~ "Gingiyu",
      TRUE ~ NA_character_)
  ) %>% 
  mutate(
    settlement_type = case_when(
      settlement_type == 1 ~ "formal",
      settlement_type == 2 ~ "informal",
      settlement_type == 3 ~ "slum",
      TRUE ~ NA_character_)
  ) %>% 
  mutate(
    two_wk_fever = case_when(
      two_wk_fever == 1 ~ "yes",
      two_wk_fever == 2 ~ "no",
      TRUE ~ NA_character_)
  ) %>% 
  mutate(
    test_result = case_when(
      test_result == 1 ~ "positive",
      test_result == 2 ~ "negative",
      test_result == 3 ~ "other",
      TRUE ~ NA_character_)
  ) %>% 
  mutate(
    lga = case_when(
      lga %in% c("KANO MUNICIPAL", "KMC", "KANO", "KANO  MUNICIPAL", "MUNICIPAL", "K/MUNICIPAL", "Kmc") ~ "Kano Municipal",
      lga %in% c("NASARAWA", "NASSARAWA", "NASSRAWA") ~ "Nasarawa",
      lga %in% c("GWALE", "Æ\u0093WALE", "GWA", "GAWLE", "Gwale") ~ "Gwale",
      lga %in% c("TARAUNI", "Tarauni") ~ "Tarauni",
      lga %in% c("FAGGE", "FAGE", "Fagge", "FAGGE D2") ~ "Fagge",
      lga %in% c("DALA", "Dala") ~ "Dala",
      lga %in% c("ZANGO", "Zango") ~ "Zango",
      lga %in% c("GIGINYU") ~ "Giginyu",
      lga %in% c("MUHAMMAD AHMAD") ~ "Muhammad Ahmad",
      lga == "" ~ NA_character_,
      TRUE ~ lga
    )
  )

kano_dry <- kano_dry %>% 
  dplyr::select(-ea) %>% 
  rename(
    lga = bi1_men,
    ward = bi2_men,
    settlement_type = bi3_men,
    community_name = bi4_men,
    ea = bi5_men,
    household_num = bi6_men,
    longitude = bi7_long_men,
    latitude = bi7_lat_men,
    age = q201a,
    dob = q201b,
    date = bi9_men,
    two_wk_fever = q401,
    test_result = q702
  ) %>%
    dplyr::select(sn, lga, ward, settlement_type, community_name, ea, 
                  household_num, age, dob, longitude, latitude, date, two_wk_fever, test_result) %>% 
  mutate(
    ward = case_when(
      ward == 1 ~ "Zango",
      ward == 2 ~ "Dorayi",
      ward == 3 ~ "Gingiyu",
      ward == 4 ~ "Fagge D2",
      ward == 5 ~ "Gobirawa",
      TRUE ~ NA_character_)
  ) %>% 
  mutate(
    settlement_type = case_when(
      settlement_type == 1 ~ "formal",
      settlement_type == 2 ~ "informal",
      settlement_type == 3 ~ "slum",
      TRUE ~ NA_character_)
  ) %>% 
  mutate(
    two_wk_fever = case_when(
      two_wk_fever == 1 ~ "yes",
      two_wk_fever == 2 ~ "no",
      TRUE ~ NA_character_)
  ) %>% 
  mutate(
    test_result = case_when(
      test_result == 1 ~ "positive",
      test_result == 2 ~ "negative",
      test_result == 3 ~ "other",
      TRUE ~ NA_character_)
  )


## -----------------------------------------------------------------------------------------------------------------------------------------
### Ibadan Data
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in data
ibadan_wet <- read.dta13(file.path(CSMenDataDirWetIbadan, "all_ibadan_wetseason_dater_wide.dta"))

ibadan_wet <- ibadan_wet %>% 
  rename(
    lga = LGA...644,
    ward = ward_r...645,
    settlement_type = bi3_men,
    two_wk_fever = q401_men,
    test_result = q702_men
  ) %>%
  dplyr::select(sn, lga, ward, settlement_type, ea, 
                two_wk_fever, test_result) %>% 
  mutate(
    settlement_type = case_when(
      settlement_type == 1 ~ "formal",
      settlement_type == 2 ~ "informal",
      settlement_type == 3 ~ "slum",
      TRUE ~ NA_character_)
  ) %>% 
  mutate(
    two_wk_fever = case_when(
      two_wk_fever == 1 ~ "yes",
      two_wk_fever == 2 ~ "no",
      TRUE ~ NA_character_)
  ) %>% 
  mutate(
    test_result = case_when(
      test_result == 1 ~ "positive",
      test_result == 2 ~ "negative",
      test_result == 3 ~ "other",
      TRUE ~ NA_character_)
  )




