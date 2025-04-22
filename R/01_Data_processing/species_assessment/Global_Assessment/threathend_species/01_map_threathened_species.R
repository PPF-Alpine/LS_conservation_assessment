#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(rredlist)
library(stringr)
library(scales)
library(purrr)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)


# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#        read in the threats
#----------------------------------------------------------#

threats_stresses_species <- read.csv(paste0(data_storage_path,"Outputs/IUCN_assessment_lists/threats_stresses_species.csv"))

#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", 
                                     sep = "/"))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
#----------------------------------------------------------#
#     prep the data
#----------------------------------------------------------#
# Define categories
threatened_codes <- c("CR", "EN", "NT", "VU")
not_threatened_codes <- c("LC")
unclassified_codes <- c("DD", "not assessed")

# Add threat status
map_data <- threats_stresses_species |>
  mutate(
    threat_status = case_when(
      red_list_code %in% threatened_codes ~ "Threatened",
      red_list_code %in% not_threatened_codes ~ "Not Threatened",
      red_list_code %in% unclassified_codes ~ "Unclassified",
      TRUE ~ "Unknown"
    )
  ) |>
  group_by(Mountain_range, threat_status) |>
  summarise(n = n(), .groups = "drop")

# Pivot wider to get counts in columns
map_data_wide <- map_data |>
  pivot_wider(names_from = threat_status, values_from = n, values_fill = 0)

# Calculate proportion of threatened species
map_data_wide <- map_data_wide |>
  mutate(
    total_classified = `Threatened` + `Not Threatened`,
    proportion = if_else(total_classified > 0, Threatened / total_classified, NA_real_),
    prop_unclassified = Unclassified / (Unclassified + total_classified))


mountain_data <- mountain_shapes |>
  left_join(map_data_wide, by = c("MapName" = "Mountain_range"))

#----------------------------------------------------------#
#   create the map
#----------------------------------------------------------#

thr_map <-ggplot(mountain_data) +
  geom_sf(data = world, fill = "grey95", color = "grey90") +
  geom_sf(aes(fill = proportion, color = proportion), size = 0.1) +
  scale_fill_viridis_c(option = "inferno", direction = -1, limits = c(0, 0.6), na.value = "lightgrey") +
  scale_color_viridis_c(option = "inferno", direction = -1, limits = c(0, 0.6), na.value = "lightgrey", guide = "none") +
  theme_void() +
  labs(fill = "% threatened")



uncl_map<-ggplot(mountain_data) +
  geom_sf(data = world, fill = "grey95", color = "grey90") +
  geom_sf(aes(fill = prop_unclassified, color = prop_unclassified), size = 0.1) +
  scale_fill_viridis_c(option = "inferno", direction = -1, limits = c(0, 0.6), na.value = "lightgrey") +
  scale_color_viridis_c(option = "inferno", direction = -1, limits = c(0, 0.6), na.value = "lightgrey", guide = "none") +
  theme_void() +
  labs(fill = "% unclassified")

#----------------------------------------------------------#
#   save the maps
#----------------------------------------------------------#
today <- format(Sys.Date(), "%Y%m%d")

# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/species_assessment/threathened_species_", today, ".jpeg")

# Save last plot as PNG
ggsave(filename = plot_path, plot=thr_map,width = 10, height = 6, dpi = 300)


# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/species_assessment/unclassified_species_", today, ".jpeg")

# Save last plot as PNG
ggsave(filename = plot_path, plot=uncl_map,width = 10, height = 6, dpi = 300)
