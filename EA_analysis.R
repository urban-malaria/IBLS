# ==========================================================================================================================================
## Script Name: TPR by EA
## Purpose: Calculates malaria TPR by enumeration area and merges this with centroids of each EA for Ibadan and Kano.
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")
EADataDir <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Combined Working Data")
CentroidDir <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Field data/cleaned_data")
OutputDir <- file.path(DriveDir, "projects/ChatMRPT/IBLS/tpr_by_ea")
library(readstata13)

kano_dry <- read.dta13(file.path(EADataDir, "Kano/Dry Season Data/Long Data/long_dryseason_household_membersV00.dta"))
kano_wet <- read.dta13(file.path(EADataDir, "Kano/Wet Season Data/Long Data/kano_wetseason_long_data.dta"))
ibadan_dry <- read.dta13(file.path(EADataDir, "Ibadan/Dry Season Data/Long Data/Ibadan_long_dry_season_household_members_records.dta"))
ibadan_wet <- read.dta13(file.path(EADataDir, "Ibadan/Wet Season Data/Long Data/ibadan_long_wetseason_household_members_with_ind_nets.dta")) %>% dplyr::rename(ea_cluster = enumaration_area)
kano_centroids <- read.csv(file.path(CentroidDir, "Kano/kano_final_centroids_data.csv"))
ibadan_centroids <- read.csv(file.path(CentroidDir, "Ibadan/Ibadan_centoids_data.csv")) %>% dplyr::rename(ea_names = ea_numbers_new)

clean_data <- function(data) {
  data <- data %>% 
    rename(
      age = hl5,
      test_result = q302,
      household_weight = overall_hh_weight
    ) %>%
    mutate(
      test_result = case_when(
        test_result == 1 ~ "positive",
        test_result == 2 ~ "negative",
        test_result == 3 ~ "other",
        TRUE ~ NA_character_)
    ) %>% 
    dplyr::select(ea_cluster, age, test_result, household_weight)
}

# some duplicated variable names so make names unique
names(kano_wet) <- make.names(names(kano_wet), unique = TRUE)

kano_dry <- clean_data(kano_dry)
kano_wet <- clean_data(kano_wet)
ibadan_dry <- clean_data(ibadan_dry)
ibadan_wet <- clean_data(ibadan_wet)

# filter datasets for only children under 5
kano_wet <- kano_wet %>% dplyr::filter(age < 5)
kano_dry <- kano_dry %>% dplyr::filter(age < 5)
ibadan_wet <- ibadan_wet %>% dplyr::filter(age < 5)
ibadan_dry <- ibadan_dry %>% dplyr::filter(age < 5)

# combine datasets
kano <- kano_wet %>% rbind(kano_dry)
ibadan <- ibadan_wet %>% rbind(ibadan_dry)

# function to summarize tpr by EA
summarize_tpr <- function(data) {
  data <- data %>%
    filter(!is.na(test_result)) %>%
    group_by(ea_cluster) %>%
    summarise(
      total_tests = n(),  # total number of tests (non-missing after filter)
      total_weight = sum(household_weight, na.rm = TRUE),
      num_positive_tests = sum(test_result == "positive", na.rm = TRUE),
      positive_weight = sum(household_weight[test_result == "positive"], na.rm = TRUE),
      tpr = positive_weight / total_weight, # calculate tpr as the proportion of weighted positives
    ) %>%
    ungroup()
}

kano_tpr_by_ea_dry <- summarize_tpr(kano_dry)
kano_tpr_by_ea_wet <- summarize_tpr(kano_wet)
kano_tpr_by_ea_total <- summarize_tpr(kano)
ibadan_tpr_by_ea_dry <- summarize_tpr(ibadan_dry)
ibadan_tpr_by_ea_wet <- summarize_tpr(ibadan_wet)
ibadan_tpr_by_ea_total <- summarize_tpr(ibadan)

# merge the centroid data in using ea_names column
merge_centroids <- function(data, centroids) {
  data <- data %>%
    left_join(centroids, by = c("ea_cluster" = "ea_names"))
}

kano_tpr_by_ea_dry <- merge_centroids(kano_tpr_by_ea_dry, kano_centroids)
kano_tpr_by_ea_wet <- merge_centroids(kano_tpr_by_ea_wet, kano_centroids)
kano_tpr_by_ea_total <- merge_centroids(kano_tpr_by_ea_total, kano_centroids)
ibadan_tpr_by_ea_dry <- merge_centroids(ibadan_tpr_by_ea_dry, ibadan_centroids)
ibadan_tpr_by_ea_wet <- merge_centroids(ibadan_tpr_by_ea_wet, ibadan_centroids)
ibadan_tpr_by_ea_total <- merge_centroids(ibadan_tpr_by_ea_total, ibadan_centroids)

# function to clean up table
make_table <- function(data) {
  data %>% 
    dplyr::select(ea_cluster, total_tests, num_positive_tests, tpr, latitude, longitude) %>% 
    rename(
      EA = ea_cluster,
      'Total Tests' = total_tests,
      'Positive Tests' = num_positive_tests,
      TPR = tpr,
      Latitude = latitude,
      Longitude = longitude
    )
}

# create the formatted tables
kano_dry_table <- make_table(kano_tpr_by_ea_dry)
kano_wet_table <- make_table(kano_tpr_by_ea_wet)
kano_all_table <- make_table(kano_tpr_by_ea_total)
ibadan_dry_table <- make_table(ibadan_tpr_by_ea_dry)
ibadan_wet_table <- make_table(ibadan_tpr_by_ea_wet)
ibadan_all_table <- make_table(ibadan_tpr_by_ea_total)

# save as CSV files
write.csv(kano_all_table, file = file.path(OutputDir, "kano_all_tpr.csv"), row.names = FALSE)
write.csv(kano_dry_table, file = file.path(OutputDir, "kano_dry_tpr.csv"), row.names = FALSE)
write.csv(kano_wet_table, file = file.path(OutputDir, "kano_wet_tpr.csv"), row.names = FALSE)
write.csv(ibadan_all_table, file = file.path(OutputDir, "ibadan_all_tpr.csv"), row.names = FALSE)
write.csv(ibadan_dry_table, file = file.path(OutputDir, "ibadan_dry_tpr.csv"), row.names = FALSE)
write.csv(ibadan_wet_table, file = file.path(OutputDir, "ibadan_wet_tpr.csv"), row.names = FALSE)
