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
# Load human footprint index (HFI) and land use change info --
#----------------------------------------------------------#
hfi <- rast(paste0(data_storage_path, "Datasets/human_footprint/HFI/hii_2020-01-01.tif"))
plot(hfi)
## https://wcshumanfootprint.org/data-access 

luc <- rast(paste0(data_storage_path, "Datasets/human_footprint/LUH_2/states.nc"))

# source HKH boundary
hkh_boundary <- sf::st_read("~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/HKH_boundary/HKH_boundary.shp")



# extract only luc for 2015
years <- time(luc)
luc_2015 <- luc[[which(years == 2015)]]
plot(luc_2015)


#----------------------------------------------------------#
# crop and mask HFI and LUC to hkh  -
#----------------------------------------------------------#

luc_cropped <- crop(luc_2015, hkh_boundary)

luc_hkh <- mask(luc_cropped, hkh_boundary, touches=TRUE)
plot(luc_hkh)


hfi_cropped <- crop(hfi, hkh_boundary)

hfi_hkh <- mask(hfi_cropped, hkh_boundary, touches=TRUE)
plot(hfi_hkh)


#----------------------------------------------------------#
# save results as tif 
#----------------------------------------------------------#

writeRaster(
  hfi_hkh,
  "~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/hfi/hfi_hkh.tif",
  overwrite = TRUE
)
