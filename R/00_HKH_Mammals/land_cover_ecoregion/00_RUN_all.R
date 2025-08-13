

#----------------------------------------------------------#
# 0. Set up  -----
#----------------------------------------------------------#
library(here)
#devtools::install_github("alrobles/mdd")
library(mdd)
library(terra) # for plot

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# 0. load files  -----
#----------------------------------------------------------#

# load files 
# dem 
# lc 
# species list 
# ecoregions

lc <- rast(paste0(data_storage_path, "Datasets/land_cover/rlcms_2021/data/hkh_lc-2021.tif")) 

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

# the cropped dem
dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
plot(dem_crop)

eco <- rast(paste0(data_storage_path, "Datasets/ecoregions/ecoreg_HKH.tif")) 

lc_data_descr <- readxl::read_excel(paste0(data_storage_path,"Datasets/land_cover/lc_data_description.xlsx"))

#----------------------------------------------------------#
# 1. get species range  -----
#----------------------------------------------------------#

# specify sciname 
# ❗ loop

target_sciname <- "Marmota himalayana"

# run file 

#----------------------------------------------------------#
# 2. rasterize species range  -----
#----------------------------------------------------------#

# run file 

#----------------------------------------------------------#
# 3. elevational mask per species  -----
#----------------------------------------------------------#

# run file 

#----------------------------------------------------------#
# 4. get the ecoregions within species ranges  -----
#----------------------------------------------------------#

# run file 

#----------------------------------------------------------#
# 5. combine results to a dataframe  -----
#----------------------------------------------------------#

# run file 

#----------------------------------------------------------#
# 6. save everything  -----
#----------------------------------------------------------#

# run file 
