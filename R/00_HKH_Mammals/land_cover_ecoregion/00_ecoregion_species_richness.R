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
# 1. ecoregions where mammal richness is high-----
#----------------------------------------------------------#
# resample to the eco grid
eco_res <- resample(eco,species_richness_total,method = "near")
plot(eco_res)

# average richness per ecoregion
eco_summary <- zonal(species_richness_total, eco_res, fun = "mean", na.rm = TRUE)

# area 
cell_area <- cellSize(eco_res, unit = "km")

# sum of areas per ecoregion 
eco_area <- zonal(cell_area, eco_res, fun = "sum", na.rm = TRUE)

# combine
eco_summary <- eco_summary |>
  rename(mean_richness = mean) |>
  left_join(eco_area |>
              rename(area_km2 = sum),   
            by = "ECO_NAME")

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

# extract proportions with richness

# 1) Resample LC proportions to match richness grid exactly
# (proportions are continuous ⇒ bilinear OK; use "near" if you prefer)
lc_stack_r <- resample(lc_stack, species_richness_total, method = "bilinear")

# keep within [0,1] after interpolation
lc_stack_r <- clamp(lc_stack_r, 0, 1)

# 2) Crop both to the common intersection (optional, avoids edge NA mismatches)
common_ext <- intersect(ext(species_richness_total), ext(lc_stack_r))
rich_i <- crop(species_richness_total, common_ext)
lc_i   <- crop(lc_stack_r,           common_ext)

# 3) Sanity check: should pass without error
compareGeom(rich_i, lc_i, stopOnError = TRUE)

# 4) Combine and make a dataframe
all_rasters <- c(rich_i, lc_i)
df <- as.data.frame(all_rasters, na.rm = TRUE)
