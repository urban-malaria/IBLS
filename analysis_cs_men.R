# ==========================================================================================================================================
## Script Name: Prevalence vs. Incidence Analyses
## Purpose: Creates plots and models to examine the association between prevalence and incidence in the cross-sectional men's data
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")
source("data_cleaning_cs_men_kanoibadan.R")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Kano Data
## -----------------------------------------------------------------------------------------------------------------------------------------

kano_wet$both <- ifelse(kano_wet$two_wk_fever == "yes" & kano_wet$test_result == "positive", "yes", "no")
kano_dry$both <- ifelse(kano_dry$two_wk_fever == "yes" & kano_dry$test_result == "positive", "yes", "no")

kano_wet$season <- "Wet"
kano_dry$season <- "Dry"

mergeable_wet <- kano_wet %>% 
  dplyr::select(-dry_sn)

kano_wet_dry <- mergeable_wet %>% 
  rbind(kano_dry)

# summarize by lga/ward to calculate prevalence and incidence
wet_summary_kano <- kano_wet %>%
  group_by(ward) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )
dry_summary_kano <- kano_dry %>%
  group_by(ward) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )

# remove those under 18 yo
kano_wet_dry <- kano_wet_dry %>% 
  dplyr::filter(!age < 18)

both_summary_kano <- kano_wet_dry %>%
  group_by(ward, season) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )

# scatter plots for Kano
wet_plot_kano <- ggplot(wet_summary_kano, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = ward), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  xlim(0, 0.1) +
  ylim(0, 0.02) +
  geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Wet Season",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript()
wet_plot_kano
dry_plot_kano <- ggplot(dry_summary_kano, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = ward), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  xlim(0, 0.1) +
  ylim(0, 0.02) +
  geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Dry Season",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript()
dry_plot_kano

kano_season_plots <- wet_plot_kano + dry_plot_kano
ggsave(paste0(FigDir,"/", "_men_kano_wet_dry.pdf"), kano_season_plots, width = 10, height = 6)

overall_plot_kano <- ggplot(both_summary_kano, aes(x = prevalence, y = incidence, color = season)) +
  geom_point(aes(color = season), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  xlim(0, 0.1) +
  ylim(0, 0.02) +
  scale_color_manual(
    values = c("Dry" = "#CBBD93", "Wet" = "#0E87CC"),
    name = "Season"
  ) +
  geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence in Kano State, Nigeria",
    subtitle = "Men's Data Only",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
overall_plot_kano

ggsave(paste0(FigDir,"/", "_men_combined_kano.pdf"), overall_plot_kano, width = 6, height = 6)


both_summary_kano_extra <- kano_wet_dry %>%
  group_by(ward, settlement_type, season) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )

overall_plot_kano_extra <- ggplot(both_summary_kano_extra, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = season, shape = settlement_type), size = 3) +
  geom_smooth(se = FALSE, method = "loess", color = "black") +  # single overall trend line
  scale_color_manual(
    values = c("Dry" = "#CBBD93", "Wet" = "#0E87CC"),
    name = "Season"
  ) +
  #geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence in Kano State, Nigeria",
    subtitle = "Men's Data Only, 18+",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
overall_plot_kano_extra

ggsave(paste0(FigDir,"/", "_men_combined_kano_extra.pdf"), overall_plot_kano_extra, width = 6, height = 6)


## -----------------------------------------------------------------------------------------------------------------------------------------
### Ibadan Data
## -----------------------------------------------------------------------------------------------------------------------------------------

ibadan_wet$both <- ifelse(ibadan_wet$two_wk_fever == "yes" & ibadan_wet$test_result == "positive", "yes", "no")
ibadan_dry$both <- ifelse(ibadan_dry$two_wk_fever == "yes" & ibadan_dry$test_result == "positive", "yes", "no")

ibadan_wet$season <- "Wet"
ibadan_dry$season <- "Dry"

ibadan_wet_dry <- ibadan_wet %>% 
  rbind(ibadan_dry)

# summarize by lga/ward to calculate prevalence and incidence
wet_summary_ibadan <- ibadan_wet %>%
  group_by(ward) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )
dry_summary_ibadan <- ibadan_dry %>%
  group_by(ward) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )

both_summary_ibadan <- ibadan_wet_dry %>%
  group_by(ward, season) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )

# scatter plots for Ibadan
wet_plot_ibadan <- ggplot(wet_summary_ibadan, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = ward), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  xlim(0, 0.1) +
  ylim(0, 0.02) +
  geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence by Ward in Ibadan, Nigeria",
    subtitle = "Wet Season",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
wet_plot_ibadan
dry_plot_ibadan <- ggplot(dry_summary_ibadan, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = ward), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  xlim(0, 0.1) +
  ylim(0, 0.02) +
  geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence by Ward in Ibadan, Nigeria",
    subtitle = "Dry Season",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
dry_plot_ibadan

ibadan_season_plots <- wet_plot_ibadan + dry_plot_ibadan
ggsave(paste0(FigDir,"/", "_men_ibadan_wet_dry.pdf"), ibadan_season_plots, width = 10, height = 6)

# make overall (combined wet + dry) plot
overall_plot_ibadan <- ggplot(both_summary_ibadan, aes(x = prevalence, y = incidence, color = season)) +
  geom_point(aes(color = season), size = 3) +
  geom_smooth(se = FALSE, method = "lm") +
  xlim(0, 0.1) +
  ylim(0, 0.02) +
  scale_color_manual(
    values = c("Dry" = "#CBBD93", "Wet" = "#0E87CC"),
    name = "Season"
  ) +
  geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence in Ibadan State, Nigeria",
    subtitle = "Men's Data Only",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
overall_plot_ibadan

ggsave(paste0(FigDir,"/", "_men_combined_ibadan.pdf"), overall_plot_ibadan, width = 6, height = 6)

# add plot also grouped by settlement type
both_summary_ibadan_extra <- ibadan_wet_dry %>%
  group_by(ward, settlement_type, season) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )

overall_plot_ibadan_extra <- ggplot(both_summary_ibadan_extra, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = season, shape = settlement_type), size = 3) +
  geom_smooth(se = FALSE, method = "loess", color = "black") +  # single overall trend line
  scale_color_manual(
    values = c("Dry" = "#CBBD93", "Wet" = "#0E87CC"),
    name = "Season"
  ) +
  #geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence in Ibadan State, Nigeria",
    subtitle = "Men's Data Only, 18+",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
overall_plot_ibadan_extra

ggsave(paste0(FigDir,"/", "_men_combined_ibadan_extra.pdf"), overall_plot_ibadan_extra, width = 6, height = 6)


## -----------------------------------------------------------------------------------------------------------------------------------------
### Combined Kano and Ibadan (all adults plot)
## -----------------------------------------------------------------------------------------------------------------------------------------

mergable_kano_wet_dry <- kano_wet_dry %>% 
  dplyr::select(-community_name, -ea, -household_num, -longitude, -latitude, -date) %>% 
  mutate(state = "Kano")

ibadan_wet_dry <- ibadan_wet_dry %>% 
  mutate(state = "Ibadan")

ki_men_combined <- mergable_kano_wet_dry %>% 
  rbind(ibadan_wet_dry)

summary_overall <- ki_men_combined %>%
  group_by(ward, season, settlement_type) %>%
  summarise(
    n = n(),
    prevalence = mean(test_result == "positive", na.rm = TRUE),
    incidence = mean(both == "yes", na.rm = TRUE),
    .groups = "drop"
  )

overall_plot_ki <- ggplot(summary_overall, aes(x = prevalence, y = incidence)) +
  geom_point(aes(color = season, shape = settlement_type), size = 3) +
  geom_smooth(se = FALSE, method = "loess", color = "black") +
  geom_smooth(se = FALSE, method = "lm", color = "black") +
  scale_color_manual(
    values = c("Dry" = "#CBBD93", "Wet" = "#0E87CC"),
    name = "Season"
  ) +
  #geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
  labs(
    title = "Prevalence vs. Incidence in Kano and Ibadan States, Nigeria",
    subtitle = "Men's Data Only, 18+",
    x = "Prevalence (RDT positive rate)",
    y = "Incidence (RDT positive + fever rate)"
  ) +
  guides(color = "none") +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
overall_plot_ki

ggsave(paste0(FigDir,"/", "_men_combined_ki.pdf"), overall_plot_ki, width = 6, height = 6)
