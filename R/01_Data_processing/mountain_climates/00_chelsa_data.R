#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(terra)
library(pastclim)

# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#         load chelsa dataand mountain range
#----------------------------------------------------------#

list_available_datasets()[grepl("CHELSA_2.1", list_available_datasets())]

set_data_path(paste0(data_storage_path,"Datasets/Mountains/Chelsa/raw_download/"))

download_dataset(
 dataset = "CHELSA_2.1_MPI-ESM1-2-HR_ssp585_0.5m_vsi", #♻️ CHANGE DATASET HERE 
  bio_variables = c("bio01", "bio12")
)

#----------------------------------------------------------#
#         fetch time slice 2055 
#----------------------------------------------------------#

# there is no time slice available for CHELSA for 2030
future_slice_2055 <- region_slice(
  time_ce = 2055,
  dataset = "CHELSA_2.1_MPI-ESM1-2-HR_ssp585_0.5m_vsi", #♻️ CHANGE DATASET HERE 
  bio_variables = c("bio01", "bio12")
)

#----------------------------------------------------------#
#      get temp and prec variables and save as rasters
#----------------------------------------------------------#
temp_2055 <- future_slice_2055$bio01
prec_2055 <- future_slice_2055$bio12
x11()
plot(prec_2055)
# Save the rasters
output_dir <- paste0(data_storage_path, "Datasets/Mountains/Chelsa/time_slices_2055/")
writeRaster(temp_2055, filename = paste0(output_dir, "temp_2055_126.tif"), overwrite = TRUE)
writeRaster(prec_2055, filename = paste0(output_dir, "prec_2055_585.tif"), overwrite = TRUE)

