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


species_richness_total<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
#----------------------------------------------------------#
# 2. ICIMOD land cover  -----
#----------------------------------------------------------#
#  ID raster
coarse_id <- rast(species_richness_total)
values(coarse_id) <- 1:ncell(coarse_id)

# Resample IDs to LC 
coarse_id_fine <- resample(coarse_id, lc, method = "near")

lc_counts <- zonal(lc, coarse_id_fine, fun = "count", freq = TRUE)

fact <- round(res(species_richness_total)[1] / res(lc)[1])

# get proportion of each LC class per SR cell
lc_frac <- classify(lc, cbind(1:9, 1:9))  

out <- list()

for (cl in 1:9) {
  bin <- lc == cl
  out[[cl]] <- aggregate(bin, fact = fact, fun = mean, na.rm = TRUE)
}

lc_stack <- rast(out)
names(lc_stack) <- paste0("lc_prop_", 1:9)
plot(lc_stack)

out_file <- file.path(data_storage_path, "Datasets", "land_cover","fraction_land_cover_new.tif")
writeRaster(lc_stack, out_file, overwrite = TRUE, datatype = "FLT4S",
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9"))