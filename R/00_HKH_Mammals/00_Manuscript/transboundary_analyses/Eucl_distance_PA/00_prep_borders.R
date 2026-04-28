library(sf)
library(dplyr)
library(purrr)
library(rnaturalearth)
library(lwgeom)
library(terra)

source(here::here("R/00_Config_file_HKH.R"))

#---------------------------------------------#
# get country borders world bank 2025
#---------------------------------------------#

borders_WB <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/World Bank Official Boundaries - Admin 0_all_layers/WB_GAD_ADM0_complete.shp"))

hkh_boundary <- sf::st_read("~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/HKH_boundary/HKH_Boundary.shp")

# high resolution but doesnt work with PA which is lower resolution 
# get the hkh countries and also areas with non determined legal status 
hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh")

ndls <- borders_WB |>
  filter(
    NAM_0 %in% hkh_countries |
      WB_STATUS == "Non-determined legal status area"
  )

ndls_crop <- sf::st_intersection(ndls, hkh_boundary)|>
  filter( WB_STATUS == "Non-determined legal status area")|>
  dplyr::select(NAM_0,geometry)

#---------------------------------------------#
# get country borders GADM for each country individually
#---------------------------------------------#
AFG <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_AFG_shp/gadm41_AFG_0.shp"))
IND <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_IND_shp/gadm41_IND_0.shp"))
PAK <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/gadm41_PAK_0.shp"))

#---------------------------------------------#
# where applicable get ndlsa and join the names from WB
#---------------------------------------------#

# split
IND_NDLSA <- IND |> filter(GID_0 != "IND")
IND_country <- IND |> filter(GID_0 == "IND")

# overlapping geometries 
intersections <- st_intersection(IND_NDLSA, ndls_crop) 
# calculate overlap area 
intersections$overlap_area <- st_area(intersections)

IND_NDLSA_clean <- intersections |>
  group_by(GID_0) |>
  slice_max(overlap_area, n = 1, with_ties = FALSE) |>
  ungroup()

# combine back
IND_final <- bind_rows(IND_joined_clean, IND_GID_0)

#---------------------------------------------#
# PAKISTAN
#---------------------------------------------#

# split
NDLSA <- PAK |> filter(GID_0 != "PAK")
country <- PAK |> filter(GID_0 == "PAK")

# overlapping geometries 
intersections <- st_intersection(NDLSA, ndls_crop) 
# calculate overlap area 
intersections$overlap_area <- st_area(intersections)

NDLSA_clean <- intersections |>
  group_by(GID_0) |>
  slice_max(overlap_area, n = 1, with_ties = FALSE) |>
  ungroup()


#---------------------------------------------#
# save 
#---------------------------------------------#
sf::st_write(NDLSA_clean, paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/PAK_NDLSA.shp"))
sf::st_write(country, paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/PAK_country.shp"))

