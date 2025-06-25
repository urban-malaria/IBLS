# ==========================================================================================================================================
## Script Name: Data Cleaning: Kids' Household Data
## Purpose: Assigns variable names and does data cleaning of kids' household data from wet + dry seasons in Ibadan and Kano
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")
install.packages("readstata13")
library(readstata13)

kano_dry <- read.dta13(file.path(KidsData, "KNHH_kidsmal_data_dry.dta"))
kano_wet <- read.dta13(file.path(KidsData, "KNHH_kidsmal_data_wet.dta"))
ibadan_dry <- read.dta13(file.path(KidsData, "IBHH_kidsmal_data_dry.dta"))
ibadan_wet <- read.dta13(file.path(KidsData, "IBHH_kidsmal_data_wet.dta"))

# recode variables
clean <- function(data) {
  data <- data %>% 
    rename(
      sex = hl4,
      age = hl5,
      dob = hl6,
      test_result = q302,
    ) %>%
    dplyr::select(sn, sex, age, dob, test_result) %>% 
    mutate(
      test_result = case_when(
        test_result == 1 ~ "positive",
        test_result == 2 ~ "negative",
        test_result == 3 ~ "other",
        TRUE ~ NA_character_)
    )
}

kano_dry <- clean(kano_dry)
kano_wet <- clean(kano_wet)
ibadan_dry <- clean(ibadan_dry)
ibadan_wet <- clean(ibadan_wet)

kano_dry_kids <- kano_dry %>% 
  mutate(season = "Dry",
         state = "Kano")
kano_wet_kids <- kano_wet %>% 
  mutate(season = "Wet",
         state = "Kano")
ibadan_dry_kids <- ibadan_dry %>% 
  mutate(season = "Dry",
         state = "Ibadan")
ibadan_wet_kids <- ibadan_wet %>% 
  mutate(season = "Wet",
         state = "Ibadan")

all_kids <- kano_dry_kids %>% 
  rbind(kano_wet_kids, ibadan_dry_kids, ibadan_wet_kids)

# create 5-year age categories up to 30
all_kids$age_cat <- cut(all_kids$age,
                   breaks = c(0, 5, 10, 17, 30, Inf),
                   labels = c("0-5", "6-10", "11-17", "18-30", "30+"),
                   include.lowest = TRUE,
                   right = TRUE)

