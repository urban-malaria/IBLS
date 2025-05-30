# ==========================================================================================================================================
## Script Name: Data Cleaning: Cross-Sectional Data
## Purpose: Assigns variable names and does data cleaning of cross-sectional IBLS data 
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")

# read in data
wet <- read.csv(file.path(CSDataDir, "HF_data_Ibadan_wet_season_with_wards.csv"))
dry <- read.csv(file.path(CSDataDir, "HF_data_merged_kn_ib_dryseason.csv"))

rename_cs_data <- function(data) {
  data <- data %>% 
    rename(
      sn = Serial.Number,
      lga = LOCAL.GOVT..AREA,
      state = State,
      ward = Ward,
      date = Date,
      two_wk_fever = q200..Have.you.been.ill.with.a.fever.in.the.last.2.weeks.,
      test_result = q503..RESULT
    ) %>%
    dplyr::select(sn, lga, state, ward, date, two_wk_fever, test_result) %>%
    mutate(
      test_result = case_when(
        test_result == "POSITIVE" ~ "positive",
        test_result == "NEGATIVE" ~ "negative",
        TRUE ~ NA_character_
      ),
      lga = case_when(
        lga %in% c("IBADAN NORTH", "IB North") ~ "Ibadan North",
        lga %in% c("IBADAN NORTHEAST", "IB NorthEast") ~ "Ibadan Northeast",
        lga %in% c("IBADAN NORTHWEST", "IB Northwest") ~ "Ibadan Northwest",
        lga %in% c("IBADAN SOUTHEAST", "IB SouthEast") ~ "Ibadan Southeast",
        lga %in% c("IBADAN SOUTHWEST", "IB Southwest") ~ "Ibadan Southwest",
        TRUE ~ lga
      )
    )
  return(data)
}

wet <- rename_cs_data(wet)
dry <- rename_cs_data(dry)

library(openxlsx)
wet$date <- convertToDate(wet$date, origin = "1900-01-01")


