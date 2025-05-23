# ==========================================================================================================================================
## Script Name: Data Cleaning
## Purpose: Assigns variable names and does data cleaning of baseline and follow-up IBLS data 
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")

# read in data
base_data <- read.csv(file.path(DataDir, "IBLS_baselineall.csv"))
fup_data <- read.csv(file.path(DataDir, "IBLS_ffwupall.csv"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Clean Baseline Data
## -----------------------------------------------------------------------------------------------------------------------------------------

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
    two_wk_took_test = q510, # CORRECT yes 1, no 2
    two_wk_test_result = q511b, # CORRECT - positive 1, negative 2
    two_wk_dr_confirmed_malaria = q512, # CORRECT - yes 1, no 2
    num_sleeping_rooms = q105,
    housing_type = q110,
    type_of_work = q209,
    work_in_out = q215,
    test_done = q704, # CORRECT - yes 1, no 2
    test_result = q705 # CORRECT - positive 1, negative 2, indeterminate 3
  )

base_data <- base_data %>% 
  dplyr::select(X, sn, lga, ward, settlement_type, community, ea_cluster, household_id, hh_longitude, hh_latitude, interview_date,
                children_0_10, parent_age_years, parent_sex, child_age_years, child_age_months, two_wk_fever, two_wk_suspected_malaria,
                two_wk_took_test, two_wk_test_result, two_wk_dr_confirmed_malaria, num_sleeping_rooms, housing_type, type_of_work,
                work_in_out, test_done, test_result)

# recode values
base_data <- base_data %>%
  mutate(
    settlement_type = factor(
      case_when(
        settlement_type == 1 ~ "formal",
        settlement_type == 2 ~ "informal",
        settlement_type == 3 ~ "slum",
        TRUE ~ NA_character_
      )
    ),
    two_wk_fever = factor(
      case_when(
        two_wk_fever == 1 ~ "yes",
        two_wk_fever == 2 ~ "no",
        TRUE ~ NA_character_
      ),
      levels = c("no", "yes")
    ),
    two_wk_suspected_malaria = factor(
      case_when(
        two_wk_suspected_malaria == 1 ~ "yes",
        two_wk_suspected_malaria == 2 ~ "no",
        TRUE ~ NA_character_
      ),
      levels = c("no", "yes")
    ),
    two_wk_took_test = factor(
      case_when(
        two_wk_took_test == 1 ~ "yes",
        two_wk_took_test == 2 ~ "no",
        TRUE ~ NA_character_
      ),
      levels = c("no", "yes")
    ),
    two_wk_test_result = factor(
      case_when(
        two_wk_test_result == 1 ~ "yes",
        two_wk_test_result == 2 ~ "no",
        TRUE ~ NA_character_
      ),
      levels = c("no", "yes")
    ),
    two_wk_dr_confirmed_malaria = factor(
      case_when(
        two_wk_dr_confirmed_malaria == 1 ~ "yes",
        two_wk_dr_confirmed_malaria == 2 ~ "no",
        TRUE ~ NA_character_
      ),
      levels = c("no", "yes")
    ),
    children_0_10 = factor(
      case_when(
        children_0_10 == 1 ~ "yes",
        children_0_10 == 2 ~ "no",
        TRUE ~ NA_character_
      ),
      levels = c("no", "yes")
    ),
    parent_sex = factor(
      case_when(
        parent_sex == 1 ~ "male",
        parent_sex == 2 ~ "female",
        TRUE ~ NA_character_
      ),
      levels = c("female", "male")
    ),
    housing_type = factor(
      case_when(
        housing_type == 1 ~ "face-to-face",
        housing_type == 2 ~ "one-bedroom",
        housing_type == 3 ~ "two-bedroom",
        housing_type == 4 ~ "three-bedroom",
        housing_type == 5 ~ "duplex",
        housing_type == 6 ~ "other",
        TRUE ~ NA_character_
      ),
      levels = c("face-to-face", "one-bedroom", "two-bedroom", "three-bedroom", "duplex", "other")
    ),
    type_of_work = factor(
      case_when(
        type_of_work == 1 ~ "professional/technical/managerial",
        type_of_work == 2 ~ "clerical",
        type_of_work == 3 ~ "sales and services",
        type_of_work == 4 ~ "skilled manual",
        type_of_work == 5 ~ "unskilled manual",
        type_of_work == 6 ~ "agriculture",
        type_of_work == 7 ~ "domestic work",
        type_of_work == 8 ~ "other",
        TRUE ~ NA_character_
      ),
      levels = c("professional/technical/managerial", "clerical", "sales and services", "skilled manual", "unskilled manual", "agriculture", "domestic work", "other")
    ),
    work_in_out = factor(
      case_when(
        work_in_out == 1 ~ "indoors",
        work_in_out == 2 ~ "outdoors",
        work_in_out == 3 ~ "both",
        TRUE ~ NA_character_
      ),
      levels = c("indoors", "outdoors", "both")
    ),
    test_done = factor(
      case_when(
        test_done == 1 ~ "yes",
        test_done == 2 ~ "no",
        TRUE ~ NA_character_
      ),
      levels = c("no", "yes")
    ),
    test_result = factor(
      case_when(
        test_result == 1 ~ "positive",
        test_result == 2 ~ "negative",
        test_result == 3 ~ "indeterminate",
        TRUE ~ NA_character_
      ),
      levels = c("negative", "positive", "indeterminate")
    )
  )

## -----------------------------------------------------------------------------------------------------------------------------------------
### Clean Follow-Up Data
## -----------------------------------------------------------------------------------------------------------------------------------------

fup_data <- fup_data %>%
  rename_with(
    ~ str_replace_all(., 
                      c(
                        "q501_follow_up_(\\d+)_arm_1" = "two_wk_fever_m\\1",
                        "q502_follow_up_(\\d+)_arm_1" = "two_wk_sus_malaria_m\\1"
                      )
    ),
    .cols = matches("q501_follow_up_|q502_follow_up_")
  ) %>% 
  rename_with(
    .fn = ~ paste0("tested_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
    .cols = starts_with("q510_follow_up_")
  ) %>%
  rename_with(
    .fn = ~ paste0("remember_result_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
    .cols = starts_with("q511a_follow_up_")
  ) %>%
  rename_with(
    .fn = ~ paste0("test_result_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
    .cols = starts_with("q511b_follow_up_")
  ) %>% 
  rename_with(
    .fn = ~ paste0("study_rdt_done_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
    .cols = starts_with("q704_follow_up_")
  ) %>%
  rename_with(
    .fn = ~ paste0("study_rdt_result_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
    .cols = starts_with("q705_follow_up_")
  )

# get the renamed fever and malaria variable names
fever_vars <- paste0("two_wk_fever_m", 1:12)
malaria_vars <- paste0("two_wk_sus_malaria_m", 1:12)
tested_vars <- paste0("tested_m", 1:12)
remember_vars <- paste0("remember_result_m", 1:12)
result_var <- paste0("test_result_m", 1:12)
study_test_var <- paste0("study_rdt_done_m", 1:12)
study_test_result_var <- paste0("study_rdt_result_m", 1:12)

# function to recode yes/no
recode_yes_no <- function(x) {
  factor(
    case_when(
      x == 1 ~ "yes",
      x == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    levels = c("no", "yes")
  )
}
recode_pos_neg <- function(x) {
  factor(
    case_when(
      x == 1 ~ "positive",
      x == 2 ~ "negative",
      x == 3 ~ "indeterminate",
      TRUE ~ NA_character_
    ),
    levels = c("negative", "positive", "indeterminate")
  )
}

# apply recoding to all monthly columns
fup_data <- fup_data %>%
  mutate(across(all_of(c(fever_vars, malaria_vars, tested_vars, remember_vars, study_test_var)), recode_yes_no)) %>% 
  mutate(across(all_of(c(result_var, study_test_result_var)), recode_pos_neg))

# select only relevant variables
fup_data <- fup_data %>%
  dplyr::select(sn, starts_with("two_wk_fever_m"), starts_with("two_wk_sus_malaria_m"), starts_with("tested_m"), starts_with("remember_result_m"), starts_with("test_result_m"), starts_with("study_rdt_done_m"), starts_with("study_rdt_result_m"))
fup_data_rdt <- fup_data %>% 
  dplyr::select(sn, starts_with("two_wk_fever_m"), starts_with("study_rdt_done_m"), starts_with("study_rdt_result_m"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Merge Baseline and Follow-Up Data
## -----------------------------------------------------------------------------------------------------------------------------------------

all_df <- base_data %>%
  left_join(fup_data_rdt, by = "sn")

# Reshape fever and RDT result variables to long format
fever_long <- all_df %>%
  dplyr::select(sn, starts_with("two_wk_fever_m")) %>%
  pivot_longer(
    cols = starts_with("two_wk_fever_m"),
    names_to = "month",
    values_to = "fever"
  ) %>%
  mutate(month = as.numeric(gsub("two_wk_fever_m", "", month)))

rdt_long <- all_df %>%
  dplyr::select(sn, starts_with("study_rdt_result_m")) %>%
  pivot_longer(
    cols = starts_with("study_rdt_result_m"),
    names_to = "month",
    values_to = "rdt_result"
  ) %>%
  mutate(month = as.numeric(gsub("study_rdt_result_m", "", month)))

# combine to create an incidence variable
merged_long <- fever_long %>%
  full_join(rdt_long, by = c("sn", "month"))
merged_long <- merged_long %>%
  mutate(both = case_when(
    fever == "yes" & rdt_result == "positive" ~ "yes",
    TRUE ~ "no"
  ))

# fever incidence per month
fever_summary <- fever_long %>%
  group_by(month) %>%
  summarise(
    n = n(),
    fever_cases = sum(fever == "yes", na.rm = TRUE),
    fever_rate = fever_cases / n
  )

# malaria positivity rate per month
rdt_summary <- rdt_long %>%
  group_by(month) %>%
  summarise(
    n = n(),
    positive_cases = sum(rdt_result == "positive", na.rm = TRUE),
    positivity_rate = positive_cases / n
  )

# incident case rate per month
both_summary <- merged_long %>% 
  group_by(month) %>% 
  summarise(
    n = n(),
    both_cases = sum(both == "yes", na.rm = TRUE),
    both_rate = both_cases / n
  )

# fever incidence line plot
ggplot(fever_summary, aes(x = month, y = fever_rate)) +
  geom_line(color = "orange", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Fever incidence over 12 months",
    x = "Month",
    y = "Proportion with fever (past 2 weeks)"
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme_manuscript()

# malaria positivity rate line plot
ggplot(rdt_summary, aes(x = month, y = positivity_rate)) +
  geom_line(color = "red", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "RDT positivity rate over 12 months",
    x = "Month",
    y = "Proportion RDT positive"
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme_manuscript()

# incident case rate line plot
ggplot(both_summary, aes(x = month, y = both_rate)) +
  geom_line(color = "lightblue", size = 1.2) +
  geom_point(color = "lightblue", size = 2) +
  labs(
    title = "Symptomatic malaria prevalence rate over 12 months",
    x = "Month",
    y = "Proportion RDT positive with fever in prior 2 weeks"
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme_manuscript()

# combined line plot
# combine the two summaries into one long-format dataframe
combined_summary <- bind_rows(
  fever_summary %>% mutate(type = "Fever", value = fever_rate),
  rdt_summary %>% mutate(type = "RDT Positivity", value = positivity_rate),
  both_summary %>% mutate(type = "Fever + RDT Positive", value = both_rate)
)
p <- ggplot(combined_summary, aes(x = month, y = value, color = type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Trends in Fever, RDT Positivity, and Concurrent Cases Over 12 Months",
    subtitle = "among surveyed children (aged 0–10)",
    x = "Month",
    y = "Proportion"
  ) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(values = c(
    "Fever" = "orange",
    "RDT Positivity" = "red",
    "Fever + RDT Positive" = "lightblue"
  )) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave(paste0(FigDir,"/", "_rate_line_plot.pdf"), p, width = 8, height = 6)


