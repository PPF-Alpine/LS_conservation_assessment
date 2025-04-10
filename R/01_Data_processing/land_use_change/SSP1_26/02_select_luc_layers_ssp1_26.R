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
luc_alpine <- rast(paste0(data_storage_path, "Datasets/human_footprint/luc_alpine.tif"))

#-------------------#
# select for 2100
#------------------#
total_layers <- nlyr(luc_alpine)

# Its always 86 layers (86 years) for each variable
group_size <- 86

# select layer 86 which is year = 2100
selected_indices <- seq(86, total_layers, by = group_size)

# Subset the stack
luc_2100_ssp1_26 <- luc_alpine[[selected_indices]]
plot(luc_2100_ssp1_26)

#----------------------------------------------------------------------------#
# ❓ exclude natural vegetation ? primary or secondary forest or non forest ? 
#----------------------------------------------------------------------------#
layer_names <- names(luc_2100_ssp1_26)

selected_names <- grep("^(pastr|range|urban|c3ann|c3per|c4ann|c4per|c3nfx)", layer_names, value = TRUE)

# Subset the raster stack
luc_2100_ssp1_26_selected <- luc_2100_ssp1_26[[selected_names]]
plot(luc_2100_ssp1_26_selected)

#-------------------------#
# save as tif
#------------------------#
writeRaster(luc_2100_ssp1_26_selected, paste0(data_storage_path, "Datasets/human_footprint/LUH_2/luc_2100_ssp1_26_selected.tif"), overwrite=TRUE)
