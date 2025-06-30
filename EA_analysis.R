
source("load_path.R")
EADataDir <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Combined Working Data/Kano")
library(readstata13)

kano_dry <- read.dta13(file.path(EADataDir, "Dry Season Data/Long Data/long_dryseason_household_membersV00.dta"))
kano_wet <- read.dta13(file.path(EADataDir, "Wet Season Data/Long Data/kano_wetseason_long_data.dta"))

kano_dry <- kano_dry %>% 
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
  dplyr::select(ea_cluster, age, test_result, household_weight, latitude, longitude)

# some duplicated variable names so make names unique
names(kano_wet) <- make.names(names(kano_wet), unique = TRUE)

kano_wet <- kano_wet %>% 
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
  dplyr::select(ea_cluster, age, test_result, household_weight, latitude, longitude)

# kano_dry <- kano_dry %>%
#   mutate(
#     ea_cluster = str_to_lower(ea_cluster),                  # convert to lowercase
#     ea_cluster = str_remove(ea_cluster, "/.*$"),            # remove slash and everything after
#     ea_cluster = str_to_title(ea_cluster)                   # capitalize first letter of each word
#   )
# 
# # edit spacing and spelling of EAs to clean them up
# kano_dry <- kano_dry %>%
#   mutate(
#     ea_cluster = case_when(
#       ea_cluster == "Badawalayout" ~ "Badawa Layout",
#       ea_cluster == "Chikalaroad" ~ "Chikala Road",
#       ea_cluster == "Dorayibabba" ~ "Dorayi Babba",
#       ea_cluster == "Dorayikarama" ~ "Dorayi Karama",
#       ea_cluster == "Fagged2" ~ "Fagge D2",
#       ea_cluster == "Filindurumi" ~ "Filin Durumi",
#       ea_cluster == "Filinidi" ~ "Filin Idi",
#       ea_cluster == "Gibirawa" ~ "Gibirawa",
#       ea_cluster == "Gobirawaa" ~ "Gobirawa A",
#       ea_cluster == "Gobirawab" ~ "Gobirawa B",
#       ea_cluster %in% c("Gobirawacikingari", "G_cikingari")  ~ "Gobirawa Cikingari",
#       ea_cluster == "Hajhauwamaisakaroad" ~ "Haj Hauwa Maisaka Road",
#       ea_cluster == "Jaenjigawa" ~ "Jaen Jigawa",
#       ea_cluster == "Kasuwarmata" ~ "Kasuwar Mata",
#       ea_cluster == "Kawocikingari" ~ "Kawo Cikingari",
#       ea_cluster == "Kawomaigari" ~ "Kawo Maigari",
#       ea_cluster == "Kawokudu" ~ "Kawo Kudu",
#       ea_cluster == "Kofarmatadyepits" ~ "Kofar Mata Dye Pits",
#       ea_cluster == "Layinalhajiali" ~ "Layin Alhaji Ali",
#       ea_cluster == "Layinalhajialinocase" ~ "Layin Alhaji Ali No Case",
#       ea_cluster == "Layindabinai" ~ "Layin Dabinai",
#       ea_cluster == "Layindirebobi" ~ "Layin Dire Bobi",
#       ea_cluster == "Murtalamuhammadspecialhospital" ~ "Murtala Muhammad Special Hospital",
#       ea_cluster == "Murtalamuhammadspecialisthospital1" ~ "Murtala Muhammad Specialist Hospital 1",
#       ea_cluster == "Murtalamuhammadspecialisthospital2" ~ "Murtala Muhammad Specialist Hospital 2",
#       ea_cluster == "Nassarawagra" ~ "Nassarawa Gra",
#       ea_cluster == "Triumphpublishingcompany" ~ "Triumph Publishing Company",
#       ea_cluster == "Unguwarjakada" ~ "Unguwar Jakada",
#       ea_cluster == "Unguwarwambai" ~ "Unguwar Wambai",
#       ea_cluster == "Wapad2" ~ "Wapa D2",
#       TRUE ~ ea_cluster
#     )
#   )
# 
# kano_wet <- kano_wet %>%
#   mutate(
#     ea_cluster = str_to_lower(ea_cluster),                  # convert to lowercase
#     ea_cluster = str_remove(ea_cluster, "/.*$"),            # remove slash and everything after
#     ea_cluster = str_to_title(ea_cluster)                   # capitalize first letter of each word
#   )
# 
# # edit spacing and spelling of EAs to clean them up
# kano_wet <- kano_wet %>%
#   mutate(
#     ea_cluster = case_when(
#       ea_cluster == "Badawalayout" ~ "Badawa Layout",
#       ea_cluster == "Dorayibabba" ~ "Dorayi Babba",
#       ea_cluster == "Dorayikarama" ~ "Dorayi Karama",
#       ea_cluster == "Fagged2" ~ "Fagge D2",
#       ea_cluster == "Filindurumi" ~ "Filin Durumi",
#       ea_cluster == "Filinidi" ~ "Filin Idi",
#       ea_cluster == "Gobirawaa" ~ "Gobirawa A",
#       ea_cluster == "Gobirawab" ~ "Gobirawa B",
#       ea_cluster == "G_cikingari" ~ "Gobirawa Cikingari",
#       ea_cluster == "Hajhauwamaisakaroad" ~ "Haj Hauwa Maisaka Road",
#       ea_cluster == "Jaenjigawa" ~ "Jaen Jigawa",
#       ea_cluster == "Kasuwarmata" ~ "Kasuwar Mata",
#       ea_cluster == "Kawocikingari" ~ "Kawo Cikingari",
#       ea_cluster == "Kawomaigari" ~ "Kawo Maigari",
#       ea_cluster == "Kawokudu" ~ "Kawo Kudu",
#       ea_cluster == "Kofarmatadyepits" ~ "Kofar Mata Dye Pits",
#       ea_cluster == "Layinalhajialinocase" ~ "Layin Alhaji Ali No Case",
#       ea_cluster == "Murtalamuhammadspecialisthospital1" ~ "Murtala Muhammad Specialist Hospital 1",
#       ea_cluster == "Murtalamuhammadspecialisthospital2" ~ "Murtala Muhammad Specialist Hospital 2",
#       ea_cluster == "Nassarawagra" ~ "Nassarawa Gra",
#       ea_cluster == "Triumphpublishingcompany" ~ "Triumph Publishing Company",
#       ea_cluster == "Unguwarjakada" ~ "Unguwar Jakada",
#       ea_cluster == "Unguwarwambai" ~ "Unguwar Wambai",
#       ea_cluster == "Chikalaroad" ~ "Chikala Road",
#       TRUE ~ ea_cluster
#     )
#   )

# filter both datasets for only children under 5
kano_wet <- kano_wet %>% 
  dplyr::filter(age < 5)
kano_dry <- kano_dry %>% 
  dplyr::filter(age < 5)

# combine datasets
kano <- kano_wet %>% rbind(kano_dry)

# summarize tpr by EA
# add centroid for each EA by finding the avg latitude and longitude
kano_tpr_by_ea_dry <- kano_dry %>%
  filter(!is.na(test_result)) %>%
  group_by(ea_cluster) %>%
  summarise(
    total_weight = sum(household_weight, na.rm = TRUE),
    num_positive_tests = sum(test_result == "positive", na.rm = TRUE),
    positive_weight = sum(household_weight[test_result == "positive"], na.rm = TRUE),
    tpr = positive_weight / total_weight, # calculate tpr as the proportion of weighted positives
    longitude = mean(longitude, na.rm = TRUE),
    latitude = mean(latitude, na.rm = TRUE)
  ) %>%
  ungroup()

kano_tpr_by_ea_wet <- kano_wet %>%
  filter(!is.na(test_result)) %>%
  group_by(ea_cluster) %>%
  summarise(
    total_weight = sum(household_weight, na.rm = TRUE),
    num_positive_tests = sum(test_result == "positive", na.rm = TRUE),
    positive_weight = sum(household_weight[test_result == "positive"], na.rm = TRUE),
    tpr = positive_weight / total_weight, # calculate tpr as the proportion of weighted positives
    longitude = mean(longitude, na.rm = TRUE),
    latitude = mean(latitude, na.rm = TRUE)
  ) %>%
  ungroup()

kano_tpr_by_ea_total <- kano %>%
  filter(!is.na(test_result)) %>%
  group_by(ea_cluster) %>%
  summarise(
    total_weight = sum(household_weight, na.rm = TRUE),
    num_positive_tests = sum(test_result == "positive", na.rm = TRUE),
    positive_weight = sum(household_weight[test_result == "positive"], na.rm = TRUE),
    tpr = positive_weight / total_weight, # calculate tpr as the proportion of weighted positives
    longitude = mean(longitude, na.rm = TRUE),
    latitude = mean(latitude, na.rm = TRUE)
  ) %>%
  ungroup()

make_table <- function(data) {
  data <- data %>% 
    dplyr::select(ea_cluster, num_positive_tests, tpr, latitude, longitude) %>% 
    rename(EA = ea_cluster,
           'Positive Tests' = num_positive_tests,
           TPR = tpr,
           Latitude = latitude,
           Longitude = longitude)
}

kano_dry_table <- make_table(kano_tpr_by_ea_dry)
kano_wet_table <- make_table(kano_tpr_by_ea_wet)
kano_all_table <- make_table(kano_tpr_by_ea_total)

# export tables to a word document
library(officer)
doc <- read_docx()
doc <- doc %>%
  body_add_par("U5 TPR in Kano: Wet and Dry Seasons Combined", style = "heading 1") %>%
  body_add_table(value = kano_all_table, style = "table_template")
doc <- doc %>%
  body_add_par("U5 TPR in Kano Dry Season", style = "heading 1") %>%
  body_add_table(value = kano_dry_table, style = "table_template")
doc <- doc %>%
  body_add_par("U5 TPR in Kano Wet Season", style = "heading 1") %>%
  body_add_table(value = kano_wet_table, style = "table_template")

# save the document
file_path <- file.path(FigDir, "ea_tpr.docx")
print(doc, target = file_path)
