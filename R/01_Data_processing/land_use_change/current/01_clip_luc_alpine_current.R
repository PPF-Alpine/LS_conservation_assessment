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
# Load LUC for RCP2.6 SSP 1 (from IMAGE model) -
#----------------------------------------------------------#

# https://luh.umd.edu/data.shtml 

# load as raster stack
luc <- rast(paste0(data_storage_path, "Datasets/human_footprint/LUH_2/states.nc"))

# extract only luc for 2015
years <- time(luc)
luc_2015 <- luc[[which(years == 2015)]]


# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                   sep = "/"))

#----------------------------------------------------------#
# crop and mask HFI to alpine biome -
#----------------------------------------------------------#

luc_cropped <- crop(luc_2015, alpine_shapes)

luc_alpine <- mask(luc_cropped, alpine_shapes, touches=TRUE)


x11()
plot(luc_alpine)


#----------------------------------------------------------#
# write rasters 
#----------------------------------------------------------#

writeRaster(luc_alpine, paste0(data_storage_path, "Datasets/human_footprint/LUH_2/luc_alpine_current.tif"), overwrite=TRUE)

