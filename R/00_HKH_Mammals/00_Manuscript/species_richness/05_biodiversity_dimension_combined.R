# pkgs
library(terra)        # you already use this
library(tidyterra)    # ggplot geoms for terra rasters
library(ggplot2)
library(patchwork)    # to arrange plots

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

total_endemism_join <- read.csv(paste0(data_storage_path,"Datasets/species_list/species_endemism/total_species_endemism.csv"))|>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))


species_richness_total<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
smallest_range<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/smallest_range_median.tif"))
elev_range <-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/smallest_elev_median.tif"))
HKH_only<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/mosthkh_median.tif"))
threatened<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/globally_threathened.tif"))
threatened_nat<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/nationally_threathened.tif"))
data_deficient<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/dd_and_NA.tif"))

#---------------------------------------------#
# Select biodiversity importance layers
#---------------------------------------------#

# small geogr range 
# small elev range
# hkh endemics
# threathened

# scale to 0-1 
endemics_n   <- scale_linear(HKH_only, min = 0, max = 1)
plot(endemics_n)
small_geo_n  <- scale_linear(smallest_range, min = 0, max = 1)
plot(small_geo_n)
small_elev_n <- scale_linear(elev_range, min = 0, max = 1)
threatened_n <- scale_linear(threatened, min = 0, max = 1)

biodiv_imp <- (0.25 * endemics_n) +
  (0.5 * small_geo_n) +
  #(0.175 * small_elev_n) +
  (0.25 * threatened_n)


biodiv_imp <- (0.25 * endemics_n) +
  (0.25 * small_geo_n) +
  (0.25 * small_elev_n) +
  (0.25 * threatened_n)

plot(biodiv_imp)

thresh_biod <- quantile(values(biodiv_imp), probs = 0.75, na.rm = TRUE)
biod_high <- ifel(biodiv_imp >= thresh_biod, 1, 0)
plot(biod_high, main = "High biodiv")

writeRaster(
  biodiv_imp,
  "~/Desktop/Manuscripts/Ch_2_HKH_mammals/Output/biodiv_dimensions/biodiv_imp.tif",
  overwrite = TRUE
)
