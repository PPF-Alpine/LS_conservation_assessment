#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(rredlist)
library(stringr)
library(scales)
library(purrr)
library(patchwork)

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
#   plot a heatmap to visualize threats for one mountain
#----------------------------------------------------------#

mountain <- threats_stresses_species|>
  filter(Mountain_range=="South Island")

# Bin mid elevations into ranges (e.g., every 500 m)
breaks <- seq(0, max(mountain$mid_elevation, na.rm = TRUE) + 500, by = 500)
labels <- paste0(head(breaks, -1), "–", tail(breaks, -1))

threats_binned <- mountain |>
  mutate(elevation_band = cut(mid_elevation, breaks = breaks, labels = labels, include.lowest = TRUE))

# Split threat codes into rows
threats_long <- threats_binned |>
  separate_rows(threats_broad, sep = ";\\s*")|>
  filter(!is.na(threats_code) & threats_code != "")

# frequency of each threat code per elevation band
threat_counts <- threats_long |>
  count(elevation_band, threats_broad)

# which are the greatest threats at each elevation belt? 
threat_props <- threats_long |>
  count(elevation_band, threats_broad) |>
  group_by(elevation_band) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  mutate(
    threats_broad = factor(threats_broad, levels = sort(as.numeric(unique(threats_broad))))
  )|>
  left_join(classification_code_description, by = c("threats_broad" = "code"))

#----------------------------------------------------------#
#        plot heatmap 
#----------------------------------------------------------#
ggplot(threat_props, aes(x = elevation_band, y = description, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Proportion", labels = percent) +
  labs(
    x = "Mid Elevation Band (m)",
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------------------------#
#      Plot heatmap for all mountains 
#----------------------------------------------------------#

# Mountain selection
mountain_selection <- c(
  "Himalaya", "Northern Andes", "Central Andes", "Central European Highlands", 
  "Intermountain West", "Hindu Kush", "Ethiopian Highlands", "Albertine Rift Mountain",
  "South Island", "North European Highlands", "Tibetan Plateau", "Great Escarpment",
  "Malay Archipelago", "Caucasus Mountains", "East European Highlands",
  "Rocky Mountains", "Pacific Coast Ranges", "Eastern Rift mountains", "Mexican Highlands"
)

# function inn R/Functions

# Generate all plots
threat_plots <- map(mountain_selection, plot_threat_heatmap)

# Remove NULLs for mountains with no data
threat_plots <- compact(threat_plots)

#----------------------------------------------------------#
#     safe plot 
#----------------------------------------------------------#

wrap_plots(threat_plots,ncol=3) 

today <- format(Sys.Date(), "%Y%m%d")

# Define file path
output_path <- paste0(data_storage_path, "Outputs/Figures/species_assessment/heatmap_iucn_threats", today, ".jpg")

# Save the plot
ggsave(output_path,
       plot = last_plot(),      # or assign your full plot to a variable and use it here
       width = 18, height = 18, # adjust size as needed
       dpi = 300,               # high-quality output
       device = "jpeg")


