# ==========================================================================================================================================
## Script Name: Prevalence vs. Incidence Analyses
## Purpose: Creates plots and models to examine the association between prevalence and incidence in the IBLS data
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")
source("data_cleaning_long.R")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Merge Baseline and Follow-Up Data
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
  group_by(month, fup_age_group) %>%
  summarise(
    n = n(),
    fever_cases = sum(fever == "yes", na.rm = TRUE),
    fever_rate = fever_cases / n
  )

# malaria positivity rate per month
rdt_summary <- rdt_long %>%
  group_by(month, fup_age_group) %>%
  summarise(
    n = n(),
    positive_cases = sum(rdt_result == "positive", na.rm = TRUE),
    positivity_rate = positive_cases / n
  )

# incident case rate per month
both_summary <- merged_long %>% 
  group_by(month, fup_age_group.x) %>% 
  summarise(
    n = n(),
    both_cases = sum(both == "yes", na.rm = TRUE),
    both_rate = both_cases / n
  )

## -----------------------------------------------------------------------------------------------------------------------------------------
### Plots
## -----------------------------------------------------------------------------------------------------------------------------------------

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
combined_summary$fup_age_group[!is.na(combined_summary$fup_age_group.x)] <- 
  combined_summary$fup_age_group.x[!is.na(combined_summary$fup_age_group.x)]

# plot for age 0–5
p_0_5 <- combined_summary %>%
  filter(fup_age_group == "0–5") %>%
  ggplot(aes(x = month, y = value, color = type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Trends in Fever, RDT Positivity, and Concurrent Cases",
    subtitle = "Children aged 0–5 years",
    x = "Month",
    y = "Proportion"
  ) +
  scale_x_continuous(breaks = 1:12) +
  ylim(0, 0.2) +
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
p_0_5

# plot for age 6–10
p_6_11 <- combined_summary %>%
  filter(fup_age_group == "6–11") %>%
  ggplot(aes(x = month, y = value, color = type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Trends in Fever, RDT Positivity, and Concurrent Cases",
    subtitle = "Children aged 6–11 years",
    x = "Month",
    y = "Proportion"
  ) +
  scale_x_continuous(breaks = 1:12) +
  ylim(0, 0.2) +
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
p_6_11

age_plots <- p_0_5 + p_6_11

ggsave(paste0(FigDir,"/", "_age_rate_line_plot.pdf"), age_plots, width = 14, height = 6)


# put prevalence (RDT positivity proportion) on the x axis and incidence (fever + RDT positive on the y-axis) and plot separately for the two groups, and also for the two areas?
# filter for only those types
combined_summary_filtered <- combined_summary %>%
  filter(type %in% c("RDT Positivity", "Fever + RDT Positive"))
# put in wide format to get one row per month + age group
combined_wide <- combined_summary_filtered %>%
  dplyr::select(month, fup_age_group, type, value) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  rename(rdt_positivity = `RDT Positivity`,
         fever_rdt_positive = `Fever + RDT Positive`)

p_compared <- ggplot(combined_wide, aes(x = rdt_positivity, y = fever_rdt_positive, color = fup_age_group)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, method = "loess") +
  scale_color_manual(values = c("0–5" = "#ffa630", 
                                "6–11" = "#4da1a9")) +
  labs(x = "Prevalence (RDT Positivity Rate)", 
       y = "Incidence (Fever + RDT Positivity Rate)",
       color = "Age at First Follow-Up",
       title = "Prevalence vs. Incidence by Age Group") +
  theme_manuscript()
p_compared

ggsave(paste0(FigDir,"/", "_prev_inc_compared.pdf"), p_compared, width = 8, height = 6)


# facet by month
p_by_month <- ggplot(combined_wide, aes(x = rdt_positivity, y = fever_rdt_positive, color = fup_age_group)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("0–5" = "#ffa630", 
                                "6–11" = "#4da1a9")) +
  facet_wrap(~ month) +
  labs(x = "Prevalence (RDT Positivity Rate)", y = "Incidence (Fever + RDT Positivity Rate)",
       color = "Age at First Follow-Up",
       title = "Prevalence vs. Incidence by Month") +
  theme_manuscript()
p_by_month

ggsave(paste0(FigDir,"/", "_prev_inc_by_month.pdf"), p_by_month, width = 10, height = 8)


## -----------------------------------------------------------------------------------------------------------------------------------------
### Statistical Tests
## -----------------------------------------------------------------------------------------------------------------------------------------

lm_model <- lm(fever_rdt_positive ~ rdt_positivity + fup_age_group, data = combined_wide)
summary(lm_model)

# try GAM

