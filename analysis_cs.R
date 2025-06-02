# ==========================================================================================================================================
## Script Name: Prevalence vs. Incidence Analyses
## Purpose: Creates plots and models to examine the association between prevalence and incidence in the cross-sectional data
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")
source("data_cleaning_cs.R")

wet$both <- ifelse(wet$two_wk_fever == "Yes" & wet$test_result == "positive", "yes", "no")
dry$both <- ifelse(dry$two_wk_fever == "Yes" & dry$test_result == "positive", "yes", "no")

wet$season <- "wet"
dry$season <- "dry"

wet$month <- sub(".*-(\\d{2})-.*", "\\1", wet$date)
dry$month <- sub("^\\d+/([0-9]+)/\\d+$", "\\1", dry$date)
dry <- dry %>%
  mutate(month = if_else(
    month %in% c(13, 14, 15, 16, 17),
    str_extract(date, "^[^/]+"),
    month
  ))
dry$month <- sprintf("%02d", as.numeric(dry$month))

# the following dates are probably in month/day/year format because dry season data shouldn't have june and august
dry <- dry %>%
  mutate(month = case_when(
    date %in% c("3/5/2024", "1/8/2024", "3/6/2024") ~ sprintf("%02d", as.integer(sub("/.*", "", date))),
    TRUE ~ month
  ))

# recode the one 01/january observation in the wet df to dry
wet <- wet %>%
  mutate(
    season = case_when(
      season == "wet" & date == as.Date("2000-01-27") ~ "dry",
      TRUE ~ season
    )
  )

# remove that row from the wet df and add it to the dry df
wet_january <- wet %>% dplyr::filter(month == "01")
dry <- dry %>% 
  rbind(wet_january)
wet <- wet %>% dplyr::filter(month != "01")

combined <- rbind(wet, dry)

wet_kano <- wet %>% dplyr::filter(state == "Kano")
wet_ibadan <- wet %>% dplyr::filter(state == "Ibadan (Oyo)")
dry_kano <- dry %>% dplyr::filter(state == "Kano")
dry_ibadan <- dry %>% dplyr::filter(state == "Ibadan (Oyo)")

# summarize by lga/ward to calculate prevalence and incidence
wet_summary_kano <- wet_kano %>%
  group_by(state, lga) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )
dry_summary_kano <- dry_kano %>%
  group_by(state, lga) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )
wet_summary_ibadan <- wet_ibadan %>%
  group_by(state, lga, ward) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )
dry_summary_ibadan <- dry_ibadan %>%
  group_by(state, lga, ward) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )

# scatter plots for Kano
wet_plot_kano <- ggplot(wet_summary_kano, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = lga), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  xlim(0, 0.3) +
  ylim(0, 0.2) +
  geom_text(aes(label = lga), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Wet Season",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript()
wet_plot_kano
dry_plot_kano <- ggplot(dry_summary_kano, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = lga), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  xlim(0, 0.3) +
  ylim(0, 0.2) +
  geom_text(aes(label = lga), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Dry Season",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript()
dry_plot_kano

kano_plots <- wet_plot_kano + dry_plot_kano
kano_plots
ggsave(paste0(FigDir,"/", "_kano_prev_inc.pdf"), kano_plots, width = 10, height = 6)


# scatter plots for Ibadan
wet_plot_ibadan <- ggplot(wet_summary_ibadan, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = lga), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  ylim(0, 1) +
  labs(
    title = "Wet Season",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript()
wet_plot_ibadan
dry_plot_ibadan <- ggplot(dry_summary_ibadan, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = lga), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  ylim(0, 1) +
  labs(
    title = "Dry Season",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript()
dry_plot_ibadan

ibadan_plots <- wet_plot_ibadan + dry_plot_ibadan
ibadan_plots
ggsave(paste0(FigDir,"/", "_ibadan_prev_inc.pdf"), ibadan_plots, width = 10, height = 6)


## -----------------------------------------------------------------------------------------------------------------------------------------
### Analysis by Month
## -----------------------------------------------------------------------------------------------------------------------------------------

combined_ibadan <- combined %>% dplyr::filter(state == "Ibadan (Oyo)")
combined_kano <- combined %>% dplyr::filter(state == "Kano")

# summarize by month to calculate prevalence and incidence
summary_ibadan <- combined_ibadan %>%
  group_by(month) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::filter(!is.na(month)) %>% 
  mutate(
    season = case_when(
      month %in% c("06", "07", "08", "09", "10", "11") ~ "Wet",
      month %in% c("01", "02", "03", "04", "05", "12") ~ "Dry",
      TRUE ~ NA_character_
    )
  )
summary_kano <- combined_kano %>%
  group_by(month) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::filter(!is.na(month)) %>% 
  mutate(
    season = case_when(
      month %in% c("06", "07", "08", "09", "10", "11") ~ "Wet",
      month %in% c("01", "02", "03", "04", "05", "12") ~ "Dry",
      TRUE ~ NA_character_
    )
  )

# same thing for each season
# summarize by month to calculate prevalence and incidence
summary_wet <- wet %>%
  group_by(month) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::filter(!is.na(month))
summary_dry <- dry %>%
  group_by(month) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::filter(!is.na(month))

# overall summary
summary_overall <- combined %>% 
  group_by(month) %>% 
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    season = case_when(
      month %in% c("06", "07", "08", "09", "10", "11") ~ "Wet",
      month %in% c("01", "02", "03", "04", "05", "12") ~ "Dry",
      TRUE ~ NA_character_
    )) %>%
  dplyr::filter(!is.na(month))


p_ibadan <- ggplot(summary_ibadan, aes(x = prevalence, y = incidence, color = season)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  geom_text_repel(aes(label = month), size = 3.5, max.overlaps = Inf) +
  xlim(0, 0.30) +
  ylim(0, 0.12) +
  scale_color_manual(
    values = c("Dry" = "#CBBD93", "Wet" = "#0E87CC"),
    name = "Season"
  ) +
  labs(
    title = "Ibadan, Nigeria",
    subtitle = "Dry and Wet Seasons",
    x = "Prevalence",
    y = "Incidence"
  ) +
  guides(color = "none") +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
p_ibadan

p_kano <- ggplot(summary_kano, aes(x = prevalence, y = incidence, color = season)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  geom_text_repel(aes(label = month), size = 3.5, max.overlaps = Inf) +
  xlim(0, 0.30) +
  ylim(0, 0.12) +
  scale_color_manual(
    values = c("Dry" = "#ECCCA2", "Wet" = "#0E87CC"),
    name = "Season"
  ) +
  labs(
    title = "Kano, Nigeria",
    subtitle = "Dry and Wet Seasons",
    x = "Prevalence",
    y = "Incidence"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
p_kano

p_both_seasons <- p_ibadan + p_kano
p_both_seasons
ggsave(paste0(FigDir,"/", "_ib_kn_plots.pdf"), p_both_seasons, width = 10, height = 6)


# plots faceted by season
p_wet <- ggplot(summary_wet, aes(x = prevalence, y = incidence)) +
  geom_point(size = 3, color = "#0E87CC") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  geom_text_repel(aes(label = month), size = 3.5, max.overlaps = Inf) +
  # xlim(0, 0.21) +
  # ylim(0, 0.16) +
  labs(
    title = "Wet Season",
    subtitle = "Ibadan and Kano, Nigeria",
    x = "Prevalence",
    y = "Incidence"
  ) +
  guides(color = "none") +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
p_wet

p_dry <- ggplot(summary_dry, aes(x = prevalence, y = incidence)) +
  geom_point(size = 3, color = "#ECCCA2") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  geom_text_repel(aes(label = month), size = 3.5, max.overlaps = Inf) +
  xlim(0, 0.21) +
  ylim(0, 0.16) +
  labs(
    title = "Dry Season",
    subtitle = "Ibadan and Kano, Nigeria",
    x = "Prevalence",
    y = "Incidence"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
p_dry

p_wet_dry <- p_wet + p_dry
p_wet_dry
ggsave(paste0(FigDir,"/", "_wet_dry_plots.pdf"), p_wet_dry, width = 10, height = 6)

# overall plot, no facets
p_overall <- ggplot(summary_overall, aes(x = prevalence, y = incidence, color = season)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  geom_text_repel(aes(label = month), size = 3.5, max.overlaps = Inf) +
  scale_color_manual(
    values = c("Dry" = "#CBBD93", "Wet" = "#0E87CC"),
    name = "Season"
  ) +
  labs(
    title = "Prevalence vs. Incidence",
    subtitle = "Ibadan and Kano, Nigeria, Dry and Wet Seasons",
    x = "Prevalence",
    y = "Incidence"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
p_overall

ggsave(paste0(FigDir,"/", "_overall.pdf"), p_overall, width = 6, height = 6)
