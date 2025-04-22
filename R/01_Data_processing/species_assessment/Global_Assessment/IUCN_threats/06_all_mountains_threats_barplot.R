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

# Split threat codes into rows
threats_long <- threats_stresses_species|>
  separate_rows(threats_broad, sep = ";\\s*")|>
  filter(!is.na(threats_code) & threats_code != "")


threat_counts <- threats_long |>
  group_by(tax_group, threats_broad) |>
  summarise(n = n(), .groups = "drop")


threat_props <- threat_counts |>
  group_by(tax_group) |>
  mutate(prop = n / sum(n)) |>
  ungroup()


threat_props <- threat_props |>
  left_join(classification_code_description, by = c("threats_broad" = "code"))

#----------------------------------------------------------#
# plot overall most common threats
#----------------------------------------------------------#
# proportions
ggplot(threat_props, aes(x = tax_group, y = prop, fill = description)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Proportion of IUCN Threats by Taxonomic Group",
    x = "Taxonomic Group",
    y = "Proportion of Threats",
    fill = "Threat Type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# actual number for each tax group
plot_count<-ggplot(threat_props, aes(x = description, y = n, fill = description)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ tax_group) +
  scale_fill_viridis_d(option = "C", guide = "none") +  # Hide legend if redundant
  labs(
    x = NULL,
    y = "threat count"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

#----------------------------------------------------------#
#        prep data for count alpine categories
#----------------------------------------------------------#
threat_counts_elev <- threats_long |>
  group_by(alpine_category, threats_broad) |>
  summarise(n = n(), .groups = "drop")


threat_props_elev <- threat_counts_elev |>
  group_by(alpine_category) |>
  mutate(prop = n / sum(n)) |>
  ungroup()


threat_props_elev <- threat_props_elev |>
  left_join(classification_code_description, by = c("threats_broad" = "code"))

# actual number for each elevation belt
plot_elev_prop<-ggplot(threat_props_elev, aes(x = description, y = prop, fill = description)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ alpine_category) +
  scale_fill_viridis_d(option = "C", guide = "none") +  # Hide legend if redundant
  labs(
    x = NULL,
    y = "threat proportion %"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

x11()
plot(plot_elev)

# actual number for each elevation belt
plot_elev_count<-ggplot(threat_props_elev, aes(x = description, y = n, fill = description)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ alpine_category) +
  scale_fill_viridis_d(option = "C", guide = "none") +  # Hide legend if redundant
  labs(
    x = NULL,
    y = "threat count"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

x11()
plot(plot_elev_count)
#----------------------------------------------------------#
#   save the plots
#----------------------------------------------------------#
today <- format(Sys.Date(), "%Y%m%d")
# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/species_assessment/all_mountains_threats", today, ".jpeg")

# Save last plot as PNG
ggsave(filename = plot_path, plot=plot_count,width = 12, height = 6, dpi = 300)

# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/species_assessment/all_mountains_threats_alp_category_prop", today, ".jpeg")

# Save last plot as PNG
ggsave(filename = plot_path, plot=plot_elev_prop,width = 10, height = 6, dpi = 300)

# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/species_assessment/all_mountains_threats_alp_category_count", today, ".jpeg")

# Save last plot as PNG
ggsave(filename = plot_path, plot=plot_elev_count,width = 10, height = 6, dpi = 300)
