#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# Load configuration file
source(here::here("R/00_Config_file.R"))


# Load required libraries
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(elevatr)

#----------------------------------------------------------#
# Load selected LUC and alpine shapes --
#----------------------------------------------------------#

# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                   sep = "/"))


luc_current_selected <- rast(paste0(data_storage_path, "Datasets/human_footprint/LUH_2/luc_current_selected.tif"))


# Define mountain selection
mountain_selection <- c("Alaska-Yukon Ranges", 
                        "Greenland Kalaallit Nunaat", 
                        "Spitsbergen")


alpine_shapes_selected <- alpine_shapes|>
  filter(!Mntn_rn%in%mountain_selection)

#---------------------------------------------------------------#
# get mean, median, max and min luc values in alpine polygons--
#----------------------------------------------------------------#
# convert polygons to terra vector 
alpine_shapes_vect <- vect(alpine_shapes_selected)

# Extract raster values for all layers per polygon
extracted_values <- terra::extract(luc_current_selected, alpine_shapes_vect, ID = TRUE)

# Join polygon attributes (e.g., Mntn_rn)
polygon_info <- alpine_shapes_selected |>
  st_drop_geometry() |>
  mutate(ID = row_number())  # Match extract's ID

# Combine extracted values with polygon info
extracted_df <- extracted_values |>
  left_join(polygon_info, by = "ID") |>
  pivot_longer(
    cols = -c(ID, Mntn_rn),   # All raster layers become 'name' + 'value'
    names_to = "layer",
    values_to = "luc_value"
  ) |>
  filter(!is.na(luc_value))

luc_summary <- extracted_df |>
  group_by(Mntn_rn, layer) |>
  summarise(
    mean_luc   = round(mean(luc_value, na.rm = TRUE), 4),
    median_luc = round(median(luc_value, na.rm = TRUE), 4),
    max_luc    = round(max(luc_value, na.rm = TRUE), 4),
    min_luc    = round(min(luc_value, na.rm = TRUE), 4),
    .groups = "drop"
  )

# filter the areas into a seperate df
luc_metadata <- luc_summary |>
  filter(layer %in% c("area_sz", "log_are"))|>
  select(Mntn_rn, layer, mean_luc) |>
  pivot_wider(names_from = layer, values_from = mean_luc)


luc_summary_current <- luc_summary |>
  filter(!layer %in% c("area_sz", "log_are"))|>
  group_by(Mntn_rn)|>
  mutate(mean_sum_luc =sum(mean_luc))|>
  left_join(luc_metadata,by="Mntn_rn")

#---------------------------------------------------------------#
# write to csv --
#----------------------------------------------------------------#

today <- format(Sys.Date(), "%Y%m%d")

# Define file path
output_path <- paste0(data_storage_path, "Outputs/land_use_change/all_mountains_luc_summary_current_", today, ".csv")

write.csv(luc_summary_current,output_path)
