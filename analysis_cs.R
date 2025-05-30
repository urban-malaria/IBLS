# ==========================================================================================================================================
## Script Name: Prevalence vs. Incidence Analyses
## Purpose: Creates plots and models to examine the association between prevalence and incidence in the cross-sectional data
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")
source("data_cleaning_cs.R")

wet$both <- ifelse(wet$two_wk_fever == "Yes" & wet$test_result == "positive", "yes", "no")
dry$both <- ifelse(dry$two_wk_fever == "Yes" & dry$test_result == "positive", "yes", "no")

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
