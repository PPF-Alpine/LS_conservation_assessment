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
hfi <- rast(paste0(data_storage_path, "Datasets/human_footprint/hii_2020-01-01.tif"))

#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", 
                                     sep = "/"))

#----------------------------------------------------------#
# crop and mask HFI to alpine biome -
#----------------------------------------------------------#

hfi_cropped_mountains <- crop(hfi, mountain_shapes)

hfi_mountains <- mask(hfi_cropped_mountains, mountain_shapes, touches=TRUE)

#----------------------------------------------------------#
# crop and mask HFI to alpine biome -
#----------------------------------------------------------#
writeRaster(hfi_mountains, paste0(data_storage_path, "Datasets/human_footprint/hfi_mountains.tif"), overwrite=TRUE)

