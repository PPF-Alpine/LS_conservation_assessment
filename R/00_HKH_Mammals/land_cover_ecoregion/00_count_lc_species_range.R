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

# the cropped dem
dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
plot(dem_crop)

eco <- rast(paste0(data_storage_path, "Datasets/ecoregions/ecoreg_HKH.tif")) 


lc_data_descr <- readxl::read_excel(paste0(data_storage_path,"Datasets/land_cover/lc_data_description.xlsx"))

hkh_boundary <- sf::st_read(paste0(data_storage_path,"Datasets/HKH_boundary/HKH_boundary.shp"))

species_rast<-rast(paste0(data_storage_path, "Datasets/species_list/rasterfiles/Hyaena_hyaena_elev_masked.tif"))
species_name <- file_path_sans_ext(basename(sources(species_rast)[1]))
species_name <- sub("_elev_masked$", "", species_name)

#----------------------------------------------------------#
# mask with lc layer (sp outside hkh)  -----
#----------------------------------------------------------#
lc_mask <- !is.na(lc)
sp_clipped <- crop(species_rast, hkh_boundary)
sp_clipped <- crop(species_rast,hkh_boundary)
plot(species_rast)
#----------------------------------------------------------#
# polygonize species_range  -----
#----------------------------------------------------------#
species_poly <- as.polygons(sp_clipped, dissolve = TRUE)
plot(species_poly)


#----------------------------------------------------------#
# mask with lc layer (sp outside hkh)  -----
#----------------------------------------------------------#
vals_within_range <- terra::extract(lc, species_poly)

counts <- vals_within_range |>
  rename(poly_id = ID) |>
  rename(ras_id = "hkh_lc-2021")|>
  count(ras_id, name = "n_cells") |>
  mutate(poly_id = species_name) |>
  mutate(prop = n_cells / sum(n_cells))

counts
