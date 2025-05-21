# ==========================================================================================================================================
## Script Name: Data Cleaning
## Purpose: Assigns variable names and does data cleaning of baseline and follow-up IBLS data 
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")

# read in data
base_data <- read.csv(file.path(DataDir, "IBLS_baselineall.csv"))
fup_data <- read.csv(file.path(DataDir, "IBLS_ffwupall.csv"))

# rename important variables
base_data <- base_data %>%
  rename(
    lga = bi1,
    ward = bi2,
    settlement_type = bi3,
    community = bi4,
    ea_cluster = bi5,
    household_id = bi6,
    hh_longitude = bi7_long,
    hh_latitude = bi7_lat,
    interview_date = bi9,
    children_0_10 = q001, # CORRECT - yes 1, no 2
    parent_age_years = q201a, # CORRECT
    parent_sex = q202, # CORRECT - male 1, female 2
    child_age_years = q300iii,
    child_age_months = q300iv,
    two_wk_fever = q501, # CORRECT - yes 1, no 2
    two_wk_suspected_malaria = q502, # CORRECT - yes 1, no 2
    two_wk_took_test = q510, # 
    two_wk_test_result = q511b,
    two_wk_dr_confirmed_malaria = q511,
    num_sleeping_rooms = q105,
    housing_type = q110,
    type_of_work = q209,
    work_in_out = q215,
    test_done = q703,
    test_result = q704
  ) %>% 
  dplyr::select(lga, ward, settlement_type, community, ea_cluster, household_id, hh_longitude, hh_latitude, interview_date,
                children_0_10, parent_age_years, parent_sex, child_age_years, child_age_months, two_wk_fever, two_wk_suspected_malaria,
                two_wk_took_test, two_wk_test_result, two_wk_dr_confirmed_malaria, num_sleeping_rooms, housing_type, type_of_work,
                work_in_out, test_done, test_result)

# NA is no






