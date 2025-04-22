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
# Load LUC for current state -
#----------------------------------------------------------#

# https://luh.umd.edu/data.shtml 

# load as raster stack
luc_alpine <- rast(paste0(data_storage_path, "Datasets/human_footprint/LUH_2/luc_alpine_current.tif"))


#----------------------------------------------------------------------------#
# ❓ exclude natural vegetation ? primary or secondary forest or non forest ? 
#----------------------------------------------------------------------------#
layer_names <- names(luc_alpine)

selected_names <- grep("^(pastr|range|urban|c3ann|c3per|c4ann|c4per|c3nfx)", layer_names, value = TRUE)

# Subset the raster stack
luc_current_selected <- luc_alpine[[selected_names]]
plot(luc_current_selected)

#-------------------------#
# save as tif
#------------------------#
writeRaster(luc_current_selected, paste0(data_storage_path, "Datasets/human_footprint/LUH_2/luc_current_selected.tif"), overwrite=TRUE)
