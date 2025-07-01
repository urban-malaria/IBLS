# ==========================================================================================================================================
## Script Name: Combine Children and Adult Data
## Purpose: Combines children longitudinal and cross-sectional data and puts it side-by-side with the adult data plots
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")

# load and clean children's longitudinal data
source("data_cleaning_long.R")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Prep Follow-Up Data
## -----------------------------------------------------------------------------------------------------------------------------------------

all_df <- base_data %>%
  left_join(fup_data_rdt, by = "sn")

# split the data by 0 - 5 and then 6 - 10?
# need to calculate age for follow-up data. Baseline was conducted one month before follow-up #1
# add follow up # 1 age variable by adding one month to child's age at baseline
all_df$child_age_years_fup1 <- all_df$child_age_years
all_df$child_age_months_fup1 <- all_df$child_age_months + 1

# handle cases where months overflow (i.e., 12 or more months)
all_df$child_age_years_fup1 <- all_df$child_age_years_fup1 + (all_df$child_age_months_fup1 %/% 12)
all_df$child_age_months_fup1 <- all_df$child_age_months_fup1 %% 12
check <- all_df %>% dplyr::select(child_age_years, child_age_months, child_age_years_fup1, child_age_months_fup1)

# add variable for 0-5 or 6-11
all_df$fup_age_group <- ifelse(all_df$child_age_years_fup1 <= 5, "0–5", "6–11")

# Reshape fever and RDT result variables to long format
fever_long <- all_df %>%
  dplyr::select(sn, settlement_type, fup_age_group, starts_with("two_wk_fever_m")) %>%
  pivot_longer(
    cols = starts_with("two_wk_fever_m"),
    names_to = "month",
    values_to = "fever"
  ) %>%
  mutate(month = as.numeric(gsub("two_wk_fever_m", "", month)))

rdt_long <- all_df %>%
  dplyr::select(sn, settlement_type, fup_age_group, starts_with("study_rdt_result_m")) %>%
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
  group_by(settlement_type, fup_age_group) %>%
  #dplyr::filter(fup_age_group == "0–5") %>% 
  summarise(
    n = n(),
    fever_cases = sum(fever == "yes", na.rm = TRUE),
    fever_rate = fever_cases / n
  )

# malaria positivity rate per month
rdt_summary <- rdt_long %>%
  group_by(settlement_type, fup_age_group) %>%
  #dplyr::filter(fup_age_group == "0–5") %>% 
  summarise(
    n = n(),
    positive_cases = sum(rdt_result == "positive", na.rm = TRUE),
    positivity_rate = positive_cases / n
  )

# incident case rate per month
both_summary <- merged_long %>% 
  group_by(settlement_type.x, fup_age_group.x) %>%
  #dplyr::filter(fup_age_group.x == "0–5") %>% 
  summarise(
    n = n(),
    both_cases = sum(both == "yes", na.rm = TRUE),
    both_rate = both_cases / n
  )

combined_summary <- bind_rows(
  fever_summary %>% mutate(type = "Fever", value = fever_rate),
  rdt_summary %>% mutate(type = "RDT Positivity", value = positivity_rate),
  both_summary %>% mutate(type = "Fever + RDT Positive", value = both_rate)
)

# clean up df
combined_summary <- combined_summary %>%
  mutate(
    # Fill forward key variables for rows that are NA due to bad row splitting
    settlement_type = coalesce(settlement_type, settlement_type.x),
    fup_age_group = coalesce(fup_age_group, fup_age_group.x),
    n = coalesce(n, as.numeric(both_cases)),
    rate = coalesce(fever_rate, positivity_rate, both_rate, value),
    cases = case_when(
      type == "Fever" ~ fever_cases,
      type == "RDT Positivity" ~ positive_cases,
      type == "Fever + RDT Positive" ~ both_cases,
      TRUE ~ NA_real_
    ),
    indicator = type
  ) %>%
  # Keep only relevant rows (filter out rows where type is NA)
  filter(!is.na(indicator)) %>%
  mutate(settlement_type = case_when(
    settlement_type == "formal" ~ "Formal",
    settlement_type == "informal" ~ "Informal",
    settlement_type == "slum" ~ "Slum",
    TRUE ~ NA_character_)
  ) %>% 
  dplyr::select(settlement_type, fup_age_group, n, cases, rate, indicator)

# filter to only include prevalence and incidence
combined_summary <- combined_summary %>% 
  dplyr::filter(indicator %in% c("RDT Positivity", "Fever + RDT Positive")) %>% 
  mutate(indicator = case_when(
    indicator == "RDT Positivity" ~ "prevalence",
    indicator == "Fever + RDT Positive" ~ "incidence"
  )) %>% 
  mutate(data_type = "Longitudinal")

# filter for 0-5 and 6-10
combined_summary_0_5 <- combined_summary %>% dplyr::filter(fup_age_group == "0–5")
combined_summary_6_10 <- combined_summary %>% dplyr::filter(fup_age_group == "6–11")

# pivot to wide format
combined_summary_wide_0_5 <- combined_summary_0_5 %>%
  dplyr::select(settlement_type, n, rate, indicator, data_type) %>%
  pivot_wider(
    names_from = indicator,
    values_from = rate
  ) %>%
  dplyr::select(settlement_type, n, prevalence, incidence, data_type)

combined_summary_wide_6_10 <- combined_summary_6_10 %>%
  dplyr::select(settlement_type, n, rate, indicator, data_type) %>%
  pivot_wider(
    names_from = indicator,
    values_from = rate
  ) %>%
  dplyr::select(settlement_type, n, prevalence, incidence, data_type)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Prep Baseline Data
## -----------------------------------------------------------------------------------------------------------------------------------------

kano_dry <- read.dta13(file.path(KidsData, "KN_IP_alldata_dry.dta"))
kano_wet <- read.dta13(file.path(KidsData, "KN_IP_alldata_wet.dta"))
ibadan_dry <- read.dta13(file.path(KidsData, "IB_IP_alldata_dry.dta"))
ibadan_wet <- read.dta13(file.path(KidsData, "IB_IP_alldata_wet.dta"))

clean_kano <- function(data){
  data <- data %>% 
    rename(
      lga = bi1,
      ward = bi2,
      settlement_type = bi3,
      age = hl5,
      dob = hl6,
      two_wk_fever = q401,
      test_result = q302,
    ) %>%
    dplyr::select(lga, ward, settlement_type, age, dob, two_wk_fever, test_result) %>% 
    mutate(
      test_result = case_when(
        test_result == 1 ~ "Positive",
        test_result == 2 ~ "Negative",
        test_result == 3 ~ "Other",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      two_wk_fever = case_when(
        two_wk_fever == 1 ~ "Yes",
        two_wk_fever == 2 ~ "No",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      settlement_type = case_when(
        settlement_type == 1 ~ "Formal",
        settlement_type == 2 ~ "Informal",
        settlement_type == 3 ~ "Slum",
        TRUE ~ NA_character_)
    ) %>% 
    # clean numeric lga names
    mutate(
      lga_numeric = suppressWarnings(as.numeric(lga)),  # coerce to numeric, will become NA if not numeric
      lga = case_when(
        lga_numeric == 1 ~ "Dala",
        lga_numeric == 2 ~ "Fagge",
        lga_numeric == 3 ~ "Gwale",
        lga_numeric == 4 ~ "Kano Municipal",
        lga_numeric == 5 ~ "Nassarawa",
        TRUE ~ as.character(lga)
      )
    ) %>%
    dplyr::select(-lga_numeric) %>% 
    # clean other lga names
    mutate(
      lga = str_trim(str_to_upper(lga)),
      lga = case_when(
        lga %in% c("DALA") ~ "Dala",
        lga %in% c("FAGGE", "FAGGED2", "FAGGE D2", "FAGGR", "FAGE") ~ "Fagge",
        lga %in% c("GWALE", "GWA", "GAWLE", "Æ\u0093WALE") ~ "Gwale",
        lga %in% c("NASSARAWA", "NASARAWA", "NASSRAWA") ~ "Nassarawa",
        lga %in% c("KANO MUNICIPAL", "K/MUNICIPAL", "MUNICIPAL", "KMC", "Kmc", "KANO  MUNICIPAL") ~ "Kano Municipal",
        lga %in% c("KANO") ~ "Kano",
        lga %in% c("ZANGO", "Zango") ~ "Zango",
        lga %in% c("TARAUNI", "Tarauni") ~ "Tarauni",
        lga %in% c("GIGINYU") ~ "Ginginyu",
        TRUE ~ str_to_title(str_to_lower(lga))
      )
    ) %>% 
    mutate(state = "Kano")
}

clean_ibadan <- function(data){
  data <- data %>% 
    rename(
      lga = bi1,
      ward = bi2,
      settlement_type = settlement,
      age = hl5_x,
      dob = hl6,
      two_wk_fever = q401,
      test_result = q302,
    ) %>%
    dplyr::select(lga, ward, settlement_type, age, dob, two_wk_fever, test_result) %>% 
    mutate(
      test_result = case_when(
        test_result == 1 ~ "Positive",
        test_result == 2 ~ "Negative",
        test_result == 3 ~ "Other",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      two_wk_fever = case_when(
        two_wk_fever == 1 ~ "Yes",
        two_wk_fever == 2 ~ "No",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      settlement_type = case_when(
        settlement_type == 1 ~ "Formal",
        settlement_type == 2 ~ "Informal",
        settlement_type == 3 ~ "Slum",
        TRUE ~ NA_character_)
    ) %>% 
    # clean ward names
    mutate(
      ward = str_trim(ward),
      ward = if_else(str_detect(ward, "^[0-9]+$"), ward, str_to_upper(ward)),
      ward = case_when(
        ward %in% c("BASHORUN", "BASORUN", "BASHORUN", "Bashorun") ~ "Basorun",
        ward %in% c("CHALLENGE", "CHALLENGE WARD") ~ "Challenge",
        ward == "OLOGUNERU" ~ "Ologuneru",
        ward == "AGUGU" ~ "Agugu",
        ward == "FELELE" ~ "Felele",
        ward == "IBADAN NORTH EAST" ~ "Ibadan Northeast",
        TRUE ~ ward
      )
    ) %>% 
    # clean lga names
    mutate(
      lga = str_trim(str_to_upper(lga)),  # Remove leading/trailing space & standardize to uppercase
      lga = case_when(
        lga %in% c("IBADAN NORTH EAST", "IBADAN NORTH-EAST", "IBADAN  NORTH EAST", "NORTH EAST") ~ "Ibadan Northeast",
        lga %in% c("IBADAN SOUTH EAST", "IBADAN SOUTH-EAST", "IBADAN  SOUTH EAST") ~ "Ibadan Southeast",
        lga %in% c("IBADAN NORTH WEST", "IBADAN NORTHWEST") ~ "Ibadan Northwest",
        lga == "IBADAN NORTH" ~ "Ibadan North",
        lga == "IBADAN SOUTH WEST" ~ "Ibadan Southwest",
        lga == "ONA ARA" ~ "Ona Ara",
        lga %in% c("IDO", "IDO LG") ~ "Ido",
        lga == "LAGELU" ~ "Lagelu",
        lga == "CHALLENGE" ~ "Challenge",
        lga == "AGUGU" ~ "Agugu",
        TRUE ~ str_to_title(lga)
      )
    ) %>% 
    mutate(state = "Ibadan")
}

clean_ibadan_dry <- function(data) {
  data <- data %>% 
    rename(
      lga = bi1,
      ward = bi2,
      settlement_type = bi3,
      age = hl5,
      dob = hl6,
      two_wk_fever = q401,
      test_result = q302,
    ) %>%
    dplyr::select(lga, ward, settlement_type, age, dob, two_wk_fever, test_result) %>% 
    mutate(
      test_result = case_when(
        test_result == 1 ~ "Positive",
        test_result == 2 ~ "Negative",
        test_result == 3 ~ "Other",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      two_wk_fever = case_when(
        two_wk_fever == 1 ~ "Yes",
        two_wk_fever == 2 ~ "No",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      settlement_type = case_when(
        settlement_type == 1 ~ "Formal",
        settlement_type == 2 ~ "Informal",
        settlement_type == 3 ~ "Slum",
        TRUE ~ NA_character_)
    ) %>% 
    # clean ward names
    mutate(
      ward = case_when(
        ward == "1" ~ "Agugu",
        ward == "2" ~ "Basorun",
        ward == "3" ~ "Challenge",
        ward == "3" ~ "Olopomewa",
        TRUE ~ NA_character_)
    ) %>% 
    # clean lga names
    mutate(
      lga = case_when(
        lga == "1" ~ "Ibadan North",
        lga == "2" ~ "Ibadan Northeast",
        lga == "3" ~ "Ibadan Northwest",
        lga == "3" ~ "Ibadan Southeast",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(state = "Ibadan")
}

kano_dry <- clean_kano(kano_dry)
kano_wet <- clean_kano(kano_wet)
ibadan_dry <- clean_ibadan_dry(ibadan_dry)
ibadan_wet <- clean_ibadan(ibadan_wet)

# clean ward names in kano wet season
kano_wet <- kano_wet %>% 
  dplyr::filter(ward != "3") %>% 
  mutate(
    ward = case_when(
      ward == 1 ~ "Zango",
      ward == 2 ~ "Dorayi",
      ward == 4 ~ "Fagge D2",
      ward == 5 ~ "Gobirawa",
      ward == 6 ~ "Gingiyu",
      TRUE ~ NA_character_)
  )

# clean ward names in kano dry season
kano_dry <- kano_dry %>% 
  mutate(
    ward = case_when(
      ward == 1 ~ "Zango",
      ward == 2 ~ "Dorayi",
      ward == 3 ~ "Gingiyu",
      ward == 4 ~ "Fagge D2",
      ward == 5 ~ "Gobirawa",
      ward == "1" ~ "Zango",
      ward == "2" ~ "Dorayi",
      ward == "3" ~ "Gingiyu",
      ward == "4" ~ "Fagge D2",
      TRUE ~ NA_character_)
  )

# clean ward names in ibadan wet season
ibadan_wet <- ibadan_wet %>% 
  mutate(
    ward = case_when(
      ward == 1 ~ "Agugu",
      ward == 2 ~ "Basorun",
      ward == 3 ~ "Challenge",
      ward == 4 ~ "Olopomewa",
    )
  )

# add season variables
kano_dry <- kano_dry %>% mutate(season = "Dry")
kano_wet <- kano_wet %>% mutate(season = "Wet")
ibadan_dry <- ibadan_dry %>% mutate(season = "Dry")
ibadan_wet <- ibadan_wet %>% mutate(season = "Wet")

all_df <- rbind(kano_dry, kano_wet, ibadan_dry, ibadan_wet)

all_df$both <- ifelse(all_df$two_wk_fever == "Yes" & all_df$test_result == "Positive", "Yes", "No")

u5_cs <- all_df %>% dplyr::filter(age < 5)
six_ten_cs <- all_df %>% dplyr::filter(age >= 5 & age <= 10)
eleven_seventeen_cs <- all_df %>% dplyr::filter(age >= 11 & age <= 17)
eighteen_thirty_cs <- all_df %>% dplyr::filter(age >= 18 & age <= 30)
thirty_plus_cs <- all_df %>% dplyr::filter(age >= 30)
adults_cs <- all_df %>% dplyr::filter(age >= 18)

all_df <- all_df %>%
  mutate(age_cat = case_when(
    age >= 0 & age <= 5 ~ "0-5",
    age >= 6 & age <= 10 ~ "6-10",
    age >= 11 & age <= 17 ~ "11-17",
    age >= 18 & age <= 30 ~ "18-30",
    age > 30 ~ "30+",
    TRUE ~ NA_character_
  ))

# summarize by settlement type
all_summary <- all_df %>% 
  group_by(settlement_type, age_cat) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "Positive", na.rm = TRUE),
    incidence = mean(both == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

# summary function
summarize_by_settlement <- function(df) {
  df %>%
    group_by(settlement_type) %>%
    summarise(
      n = n(),
      prevalence = mean(test_result == "Positive", na.rm = TRUE),
      incidence = mean(both == "Yes", na.rm = TRUE),
      .groups = "drop"
    )
}

# apply the function to each age group
u5_summary <- summarize_by_settlement(u5_cs)
six_ten_summary <- summarize_by_settlement(six_ten_cs)
eleven_seventeen_summary <- summarize_by_settlement(eleven_seventeen_cs)
eighteen_thirty_summary <- summarize_by_settlement(eighteen_thirty_cs)
thirty_plus_summary <- summarize_by_settlement(thirty_plus_cs)
adults_summary <- summarize_by_settlement(adults_cs)

# add "cross-sectional" to the dfs
u5_summary <- u5_summary %>% mutate(data_type = "Cross-Sectional")
six_ten_summary <- six_ten_summary %>% mutate(data_type = "Cross-Sectional")
eleven_seventeen_summary <- eleven_seventeen_summary %>% mutate(data_type = "Cross-Sectional", age = "11-17")
eighteen_thirty_summary <- eighteen_thirty_summary %>% mutate(data_type = "Cross-Sectional", age = "18-30")
thirty_plus_summary <- thirty_plus_summary %>% mutate(data_type = "Cross-Sectional", age = "30+")
adults_summary <- adults_summary %>% mutate(data_type = "Cross-Sectional", age = "18+")


## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine and Visualize Data
## -----------------------------------------------------------------------------------------------------------------------------------------

# combine longitudinal and cross-sectional data
all_u5 <- rbind(combined_summary_wide_0_5, u5_summary)
all_six_ten <- rbind(combined_summary_wide_6_10, six_ten_summary)

u5_plot <- ggplot(all_u5, aes(x = prevalence, y = incidence, color = data_type)) +
  geom_point(aes(color = data_type), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_color_manual(
    values = c("Longitudinal" = "#63bff0", "Cross-Sectional" = "#e1a692"),
    name = "Survey Type"
  ) +
  geom_text(aes(label = settlement_type), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence in U5 Children",
    subtitle = "Longitudinal and Cross-Sectional Surveys",
    x = "Prevalence",
    y = "Incidence"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
u5_plot

ggsave(paste0(FigDir,"/", "_u5_cs_long_plot.pdf"), u5_plot, width = 8, height = 8)

six_ten_plot <- ggplot(all_six_ten, aes(x = prevalence, y = incidence, color = data_type)) +
  geom_point(aes(color = data_type), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_color_manual(
    values = c("Longitudinal" = "#63bff0", "Cross-Sectional" = "#e1a692"),
    name = "Survey Type"
  ) +
  geom_text(aes(label = settlement_type), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence in Children Aged 5-10",
    subtitle = "Longitudinal and Cross-Sectional Surveys",
    x = "Prevalence",
    y = "Incidence"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
six_ten_plot

ggsave(paste0(FigDir,"/", "_six_ten_cs_long_plot.pdf"), six_ten_plot, width = 8, height = 8)

# plot for ages 11-17, 18-30, and 30+ (combined)
eleven_plus_summary <- rbind(eleven_seventeen_summary, eighteen_thirty_summary, thirty_plus_summary)

eleven_plus_plot <- ggplot(eleven_plus_summary, aes(x = prevalence, y = incidence, color = age)) +
  geom_point(aes(color = age), size = 3) +
  #geom_smooth(se = FALSE, method = "lm") +
  scale_color_manual(
    values = c("11-17" = "#26547c", "18-30" = "#ef476f", "30+" = "#ffd166"),
    name = "Age Category"
  ) +
  geom_text(aes(label = settlement_type), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence by Age Category and Settlement Type",
    subtitle = "Cross-Sectional Survey",
    x = "Prevalence",
    y = "Incidence"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
eleven_plus_plot

ggsave(paste0(FigDir,"/", "_eleven_plus_cs_plot.pdf"), eleven_plus_plot, width = 8, height = 8)

everyone_plot <- ggplot(all_summary, aes(x = prevalence, y = incidence, color = age_cat)) +
  geom_point(aes(color = age_cat), size = 3) +
  #geom_smooth(se = FALSE, method = "lm") +
  scale_color_manual(
    values = c("0-5" = "#9381ff", "6-10" = "#7ca982","11-17" = "#26547c", "18-30" = "#ef476f", "30+" = "#ffd166"),
    name = "Age Category"
  ) +
  geom_text(aes(label = settlement_type), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence by Age Category and Settlement Type",
    subtitle = "Cross-Sectional Survey",
    x = "Prevalence",
    y = "Incidence"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
everyone_plot

ggsave(paste0(FigDir,"/", "_all_cs_plot.pdf"), everyone_plot, width = 8, height = 8)

adults_plot <- ggplot(adults_summary, aes(x = prevalence, y = incidence)) +
  geom_point(aes(size = 2)) +
  #geom_smooth(se = FALSE, method = "lm") +
  geom_text(aes(label = settlement_type), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence in Adults (18+) by Settlement Type",
    subtitle = "Cross-Sectional Survey",
    x = "Prevalence",
    y = "Incidence"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
adults_plot

ggsave(paste0(FigDir,"/", "_adults_plot.pdf"), adults_plot, width = 8, height = 8)
