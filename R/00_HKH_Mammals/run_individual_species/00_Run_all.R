
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

#---------------------------------------------#
# Step 1: read in raster one by one
#---------------------------------------------#
in_dir <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
files  <- list.files(in_dir, pattern = "_elev_masked\\.tif$", full.names = TRUE)
message("Found ", length(files), " rasters.")


sci_name <- str_remove(basename(f), "_elev_masked\\.tif$")

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

