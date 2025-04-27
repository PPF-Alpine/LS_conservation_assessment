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
  dataset = "CHELSA_2.1_MPI-ESM1-2-HR_ssp370_0.5m_vsi",
  bio_variables = c("bio01", "bio12")
)


# there is no time slice available for CHELS
future_slice_2055 <- region_slice(
  time_ce = 2055,
  dataset = "CHELSA_2.1_MPI-ESM1-2-HR_ssp585_0.5m_vsi",
  bio_variables = c("bio01", "bio12")
)

#
variable <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/raw_download/CHELSA_2.1_0.5m_bio01_v1.0.0_vsi.vrt"))

