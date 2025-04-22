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
stress_long <- threats_stresses_species|>
  separate_rows(stresses_broad, sep = ";\\s*")|>
  filter(!is.na(stresses_code) & stresses_code != "")


stress_counts <- stress_long |>
  group_by(tax_group, stresses_broad) |>
  summarise(n = n(), .groups = "drop")


stress_props <- stress_counts |>
  group_by(tax_group) |>
  mutate(prop = n / sum(n)) |>
  ungroup()


stress_props <- stress_props |>
  left_join(classification_code_description, by = c("stresses_broad" = "code"))

#----------------------------------------------------------#
# plot overall most common threats
#----------------------------------------------------------#
# proportions
ggplot(stress_props, aes(x = tax_group, y = prop, fill = description)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Proportion of IUCN stresses by Taxonomic Group",
    x = "Taxonomic Group",
    y = "Proportion of stresses",
    fill = "Threat Type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# actual number for each tax group
plot_count<-ggplot(stress_props, aes(x = description, y = n, fill = description)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ tax_group) +
  scale_fill_viridis_d(option = "C", guide = "none") +  # Hide legend if redundant
  labs(
    x = NULL,
    y = "stress count"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

x11()
plot(plot_count)

#----------------------------------------------------------#
#   save the plots
#----------------------------------------------------------#
today <- format(Sys.Date(), "%Y%m%d")
# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/species_assessment/all_mountains_stresses", today, ".jpeg")

# Save last plot as PNG
ggsave(filename = plot_path, plot=plot_count,width = 9, height = 6, dpi = 300)

