library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

#----------------------------------------------------------#
#        read in the threats
#----------------------------------------------------------#

assessment_threats_complete <- read.csv(paste0(data_storage_path,"Outputs/IUCN_assessment_lists/assessment_threats_complete.csv"))|>
  distinct(sciname, .keep_all = TRUE)


# count if there are NA for threats for species that are listed as threathened 
num_na <- assessment_threats_complete |>
  filter(red_list_code != "LC", is.na(threats_code)) |>
  nrow()
  
count_na <- assessment_threats_complete |>
  filter(red_list_code != "LC", is.na(threats_code)) 

# all species that are not Least Concern, Near Threathened, or Data Deficient have a threat assigned 

# Sum up all duplicates (even if one row is repeated multiple times)
total_rows <- assessment_threats_complete |>
  filter(red_list_code != "LC", is.na(threats_code)) |>
  count(across(everything())) |>
  summarise(total = sum(n)) 

dupes <- duplicated(assessment_threats_complete)

# get a threats description df
# get the IUCN threat categories
iucn_key <-"nG711EY6sxQrJKeV18Epiv5ngaeeMMTDFMNm"
threat_classification <- rl_threats(key=iucn_key)

threat_classification_codes <- as.data.frame(threat_classification$threats)

threat_classification_codes <- threat_classification$threats|>
  filter(code %in% 1:12)

str(threat_classification$threats, max.level = 2)


#----------------------------------------------------------#
#         Load and prepare species data 
#----------------------------------------------------------#
# mammal checklist 
checklist_mammals <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_mammal_database.xlsx"))

# birds checklist 
checklist_birds <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_bird_database.xlsx"))

# reptiles checklist 
checklist_reptiles <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_reptile_database.xlsx"))

checklist <- bind_rows(checklist_mammals,checklist_reptiles,checklist_birds)

checklist <-checklist|>
  mutate(max_elevation = if_else(
    Mountain_range == "North European Highlands" & max_elevation > 2000,
    2000,
    max_elevation
  ))

# Join with treeline elevations
Treeline_Elevations <- readxl::read_excel(file.path(data_storage_path, "Datasets/Mountains/Treeline_Lapse_Rate_04_05.xlsx"))|>
  select(Mountain_range,
         Mean_elevation_2_degree,
         Mean_elevation_4_degree,
         Mean_elevation_6_degree)

# filter alpine species
checklist_reduced <-checklist|>
  select(source,sciname,Mountain_range,min_elevation,max_elevation,mean_treeline)|>
  left_join(Treeline_Elevations,by="Mountain_range")|>
  filter(max_elevation >= mean_treeline)|>
  mutate(range_size = max_elevation - min_elevation)|>
  mutate(mid_elevation = (max_elevation + min_elevation)/2)

#----------------------------------------------------------#
#         left join assessments with species
#----------------------------------------------------------#

threats_species <- checklist_reduced|>
  select(source,sciname,Mountain_range,min_elevation,max_elevation,range_size,mid_elevation,mean_treeline)|>
  left_join(assessment_threats_complete,by="sciname")


threats_species <- threats_species|>
  mutate(threats_broad = threats_code |>
           str_replace_all("_", ".") |>                   # Replace underscores with dots
           str_split(";\\s*") |>                          # Split into list by ";"
           lapply(function(x) str_extract(x, "^\\d+")) |> # Extract first number from each part
           lapply(unique) |>                              # Keep unique top-level threats
           sapply(paste, collapse = "; ")                 # Collapse back into a single string
  )


#----------------------------------------------------------#
#        prep data for visualizations  
#----------------------------------------------------------#

mountain <- threats_species|>
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
  left_join(threat_classification_codes, by = c("threats_broad" = "code"))

#----------------------------------------------------------#
#        plot heatmap 
#----------------------------------------------------------#
ggplot(threat_props, aes(x = elevation_band, y = description$en, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Proportion", labels = percent) +
  labs(
    x = "Mid Elevation Band (m)",
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------------------------#
#       Do the same but for all mountains 
#----------------------------------------------------------#
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(purrr)
library(patchwork)

# Mountain selection
mountain_selection <- c(
  "Himalaya", "Northern Andes", "Central Andes", "Central European Highlands", 
  "Intermountain West", "Hindu Kush", "Ethiopian Highlands", "Albertine Rift Mountain",
  "South Island", "North European Highlands", "Tibetan Plateau", "Great Escarpment",
  "Malay Archipelago", "Caucasus Mountains", "East European Highlands",
  "Rocky Mountains", "Pacific Coast Ranges", "Eastern Rift mountains", "Mexican Highlands"
)

# Function to create one heatmap per mountain for threats
plot_threat_heatmap <- function(mountain_name) {
  mountain <- threats_species |>
    filter(Mountain_range == mountain_name)
  
  # Skip if no data
  if (nrow(mountain) == 0) return(NULL)
  
  # Define elevation bands based on max elevation for this mountain
  breaks <- seq(0, max(mountain$mid_elevation, na.rm = TRUE) + 500, by = 500)
  labels <- paste0(head(breaks, -1), "–", tail(breaks, -1))
  
  threats_binned <- mountain |>
    mutate(elevation_band = cut(mid_elevation, breaks = breaks, labels = labels, include.lowest = TRUE))
  
  threats_long <- threats_binned |>
    separate_rows(threats_broad, sep = ";\\s*") |>
    filter(!is.na(threats_code) & threats_code != "")
  
  threat_props <- threats_long |>
    count(elevation_band, threats_broad) |>
    group_by(elevation_band) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    mutate(threats_broad = factor(threats_broad, levels = sort(as.numeric(unique(threats_broad))))) |>
    left_join(threat_classification_codes, by = c("threats_broad" = "code"))
  
  # Plot
  ggplot(threat_props, aes(x = elevation_band, y = description$en, fill = prop)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "magma", name = "Proportion", labels = percent) +
    labs(
      title = mountain_name,
      x = "Mid Elevation Band (m)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate all plots
threat_plots <- map(mountain_selection, plot_threat_heatmap)

# Remove NULLs for mountains with no data
threat_plots <- compact(threat_plots)

# Combine and display
wrap_plots(threat_plots, ncol = 3)


