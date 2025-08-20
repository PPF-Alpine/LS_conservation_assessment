library(terra)
library(stringr)
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(tools)

source(here::here("R/00_Config_file_HKH.R"))

sr_1 <- rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_1_100.tif"))
sr_2 <- rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_101_200.tif"))
sr_3 <- rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_201_300.tif"))
sr_4 <- rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_301_400.tif"))
sr_5 <- rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_401_497.tif"))


total_richness <- sum(sr_1,sr_2,sr_3,sr_4,sr_5, na.rm = TRUE)
plot(total_richness)


# save 
out_file <- file.path(data_storage_path, "Datasets", "species_list","species_richness","HKH_species_richness_TOTAL.tif")
writeRaster(total_richness, out_file, overwrite = TRUE, datatype = "INT2U",
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9"))

plot(richness, main = "Species richness (count)")