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

# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                   sep = "/"))

#----------------------------------------------------------#
# crop and mask HFI to alpine biome -
#----------------------------------------------------------#

hfi_cropped <- crop(hfi, alpine_shapes)

hfi_alpine <- mask(hfi_cropped, alpine_shapes, touches=TRUE)

#----------------------------------------------------------#
# crop and mask HFI to alpine biome -
#----------------------------------------------------------#

writeRaster(hfi_alpine, paste0(data_storage_path, "Datasets/human_footprint/hfi_alpine.tif"), overwrite=TRUE)
