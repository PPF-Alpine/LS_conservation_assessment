#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(rredlist)
library(stringr)
library(scales)
library(purrr)
library(patchwork)
#install.packages("Matrix")
library(lme4)
# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#        read in the threats
#----------------------------------------------------------#

threats_stresses_species <- read.csv(paste0(data_storage_path,"Outputs/IUCN_assessment_lists/threats_stresses_species.csv"))

# the description dataframe for the classification codes by IUCN
classification_code_description <- read_delim(
  paste0(data_storage_path, "Outputs/IUCN_assessment_lists/classification_code_description.csv"),
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ","),
  show_col_types = FALSE
)

classification_code_description$code<-as.factor(classification_code_description$code)

#----------------------------------------------------------#
#        prep data
#----------------------------------------------------------#
# Mountain selection
mountain_selection <- c(
  "Himalaya", "Northern Andes", "Central Andes", "Central European Highlands", 
  "Intermountain West", "Hindu Kush", "Ethiopian Highlands", "Albertine Rift Mountain",
  "South Island", "North European Highlands", "Tibetan Plateau", "Great Escarpment",
  "Malay Archipelago", "Caucasus Mountains", "East European Highlands",
  "Rocky Mountains", "Pacific Coast Ranges", "Eastern Rift mountains", "Mexican Highlands"
)


threats_stresses_species_filter <- threats_stresses_species|>
  filter(Mountain_range%in%mountain_selection)


# Split threat codes into rows
threats_long <- threats_stresses_species_filter |>
  separate_rows(threats_broad, sep = ";\\s*")|>
  filter(!is.na(threats_code) & threats_code != "")

#----------------------------------------------------------#
# calculate proportion and count of threats per elev band
#----------------------------------------------------------#
threat_counts <- threats_long |>
  group_by(Mountain_range, elevation_band, threats_broad) |>
  count(name = "n")

threat_props <- threat_counts |>
  group_by(Mountain_range, elevation_band) |>
  mutate(prop = n / sum(n)) |>
  ungroup()|>
  left_join(classification_code_description, by = c("threats_broad" = "code"))



ggplot(threat_props, aes(x = elevation_band, y = prop, fill = description)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ Mountain_range, scales = "free_x", ncol = 4) +  # ✅ different x-axis per mountain
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Proportion of IUCN Threats Across Elevation Bands (per Mountain Range)",
    x = "Elevation Band",
    y = "Proportion of Threats",
    fill = "Threat"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


