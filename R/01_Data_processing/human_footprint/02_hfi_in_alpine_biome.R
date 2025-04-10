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
# Load human footprint index (HFI) and alpine shapes --
#----------------------------------------------------------#

# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                   sep = "/"))


hfi_alpine <- rast(paste0(data_storage_path, "Datasets/human_footprint/hfi_alpine.tif"))


# Define mountain selection
mountain_selection <- c("Himalaya", 
                        "Northern Andes", 
                        "Central Andes", 
                        "Central European Highlands", 
                        "Intermountain West",
                        "Hindu Kush", 
                        "Ethiopian Highlands", 
                        "Albertine Rift Mountain",
                        "South Island",
                        "North European Highlands",
                        "Tibetan Plateau",
                        "Great Escarpment",
                        "Malay Archipelago",
                        "Caucasus Mountains",
                        "East European Highlands",
                        "Rocky Mountains",
                        "Pacific Coast Ranges",
                        "Eastern Rift mountains",
                        "Mexican Highlands")


alpine_shapes_selected <- alpine_shapes|>
  filter(Mntn_rn%in%mountain_selection)

#---------------------------------------------------------------#
# get mean, median, max and min hfi values in alpine polygons--
#----------------------------------------------------------------#
# convert polygons to terra vector 
alpine_shapes_vect <- vect(alpine_shapes_selected)

#extract raster values for each polygon
extracted_values <- terra::extract(hfi_alpine, alpine_shapes_vect, fun=NULL, na.rm=TRUE, ID=TRUE)

# combine values with polygon names
colnames(extracted_values)[2] <- "hfi_value"

# Join with polygon attributes based on IDs
alpine_hfi_df <- extracted_values |>
  left_join(
    alpine_shapes_selected |> 
      st_drop_geometry() |>
      mutate(ID = row_number()), 
    by = "ID"
  ) |>
  filter(!is.na(hfi_value))

# statistics per polygon
alpine_hfi_summary <- alpine_hfi_df |>
  group_by(Mntn_rn) |>
  summarise(
    mean_hfi   = mean(hfi_value, na.rm = TRUE),
    median_hfi = median(hfi_value, na.rm = TRUE),
    max_hfi    = max(hfi_value, na.rm = TRUE),
    min_hfi    = min(hfi_value, na.rm = TRUE),
    .groups = "drop"
  )

#---------------------------------------------------------------#
# write to csv --
#----------------------------------------------------------------#

today <- format(Sys.Date(), "%Y%m%d")

# Define file path
output_path <- paste0(data_storage_path, "Outputs/IUCN_assessment_lists/alpine_hfi_summary", today, ".csv")

write.csv(alpine_hfi_summary,output_path)
