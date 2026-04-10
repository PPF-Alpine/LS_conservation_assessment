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


# relevant raster files for biodiversity importance
species_richness_total<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
smallest_range<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/smallest_range_median.tif"))
elev_range <-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/smallest_elev_median.tif"))
HKH_only<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/mosthkh_median.tif"))
threatened<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/globally_threathened.tif"))
threatened_nat<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/nationally_threathened.tif"))
data_deficient<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/dd_and_NA.tif"))

uniqueness <- rast(paste0(data_storage_path, "Output/uniqueness/r_lcbd.tif"))
plot(uniqueness)
phylo_div <- rast(paste0(data_storage_path, "Output/phylogenetic_diversity/PD_raster.tif"))
phylo_div <- resample(phylo_div, threatened)

#---------------------------------------------#
# Select biodiversity importance layers
#---------------------------------------------#

# small geogr range 
# small elev range
# hkh endemics
# threathened

# scale to 0-1 
uniqueness_n <- scale_linear(uniqueness, min = 0, max = 1)
plot(uniqueness_n)
small_geo_n  <- scale_linear(smallest_range, min = 0, max = 1)
plot(small_geo_n)
#small_elev_n <- scale_linear(elev_range, min = 0, max = 1)
threatened_n <- scale_linear(threatened, min = 0, max = 1)
phylo_n <- scale_linear(phylo_div, min = 0, max = 1)


biodiv_imp <- (0.25 * uniqueness_n) +
  (0.25 * small_geo_n) +
  (0.25 * phylo_n) +
  (0.25 * threatened_n)

biodiv_imp <- (0.4 * uniqueness_n) +
  (0.2 * small_geo_n) +
  (0.2 * phylo_n) +
  (0.2 * threatened_n)

plot(biodiv_imp)
biodiv_imp[is.na(biodiv_imp)] <- 0
biodiv_imp <- mask(biodiv_imp, threatened)
plot(biodiv_imp)

thresh_biod <- quantile(values(biodiv_imp), probs = 0.75, na.rm = TRUE)
biod_high <- ifel(biodiv_imp >= thresh_biod, 1, 0)
plot(biod_high, main = "High biodiv")

writeRaster(
  biodiv_imp,
  "~/Desktop/Manuscripts/Ch_2_HKH_mammals/Output/biodiv_dimensions/biodiv_imp.tif",
  overwrite = TRUE
)
