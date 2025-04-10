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
luc <- rast(paste0(data_storage_path, "Datasets/human_footprint/LUH_2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc"))

# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                   sep = "/"))

#----------------------------------------------------------#
# crop and mask HFI to alpine biome -
#----------------------------------------------------------#

luc_cropped <- crop(luc, alpine_shapes)

luc_alpine <- mask(luc_cropped, alpine_shapes, touches=TRUE)

head(luc_alpine)
x11()
plot(range_1)

#investigate layers
range_1 <- luc_alpine$range_1 #year 2015
range_86 <- luc_alpine$range_86 #year 2100
x11()
plot(range_85)


#----------------------------------------------------------#
# write rasters 
#----------------------------------------------------------#

writeRaster(luc_alpine, paste0(data_storage_path, "Datasets/human_footprint/LUH_2/luc_alpine.tif"), overwrite=TRUE)

writeRaster(range_85, paste0(data_storage_path, "Datasets/human_footprint/LUH_2/luc_alpine_range_85.tif"), overwrite=TRUE)
