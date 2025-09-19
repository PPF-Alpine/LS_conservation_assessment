library(terra)
library(stringr)
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(tools)
library(stringr)

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

total_endemism_join <- read.csv(paste0(data_storage_path,"Datasets/species_list/species_endemism/total_species_endemism.csv"))|>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
template<-dem_crop

#---------------------------------------------#
# select species with small area and elev range
#---------------------------------------------#
smallest_range <- total_endemism_join %>%
  slice_min(total_area_km2, prop = 0.40)

# 20% highest % HKH
most_hkh <- total_endemism_join %>%
  slice_max(pct_in_HKH_area, prop = 0.40)

smallest_elev <- total_endemism_join %>%
  slice_min(elev_range, prop = 0.40)


x11()
ggplot(total_endemism_join, 
       aes(x = log10(total_area_km2), 
           y = elev_range, 
           color = pct_in_HKH_area)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_viridis_c() 

overlap_species <- intersect(smallest_elev$species, smallest_range$species)

overlap_species <- list(
  smallest_elev$species,
  smallest_range$species,
  most_hkh$species
) %>% 
  reduce(intersect)

# How many?
length(overlap_species)
length(smallest_10pct_elev$species)

length(overlap_species) / nrow(smallest_elev) * 100

# Which ones?
overlap_species
