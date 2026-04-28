
library(sf)
library(dplyr)
library(purrr)
library(rnaturalearth)
library(lwgeom)
library(terra)

source(here::here("R/00_Config_file_HKH.R"))
# check out if that works with pairwise border length multipolygons: pairwiseborders_GADM.shp. .
pairwiseborders <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/GADM_pairwiseborders_final.gpkg"))

# for individual countries 
#---------------------------------------------#
# get country borders GADM for each country individually
#---------------------------------------------#

AFG <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_AFG_shp/gadm41_AFG_0.shp"))
# 1 obs 

IND_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_IND_shp/gadm41_IND_0_clean.shp"))|>
  dplyr::filter(GID_0 == "IND")
# 1 obs

IND_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_IND_shp/gadm41_IND_0_clean.shp"))|>
  dplyr::filter(GID_0 != "IND")
# 5 obs

NPL <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_NPL_shp/gadm41_NPL_0.shp"))
# 1 obs 

PAK_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/gadm41_PAK_0_clean.shp"))|>
  dplyr::filter(GID_0 == "PAK")
# 1 obs 

PAK_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/gadm41_PAK_0_clean.shp"))|>
  dplyr::filter(GID_0 != "PAK")
# 1 obs

BTN <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_BTN_shp/gadm41_BTN_0.shp"))

CHN_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/cleaned_CHN.shp"))|>
  filter(GID_0 == "CHN") 

CHN_country <- st_transform(CHN_country, 4326)

st_is_valid(CHN_country, reason = TRUE)

CHN_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/cleaned_CHN.shp"))|>
  dplyr::filter(GID_0 != "CHN")

MMR <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_MMR_shp/gadm41_MMR_0.shp"))

BGD <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_BGD_shp/gadm41_BGD_0.shp"))

# sf::st_write(CHN_country, paste0(data_storage_path, "Datasets/transboundary/GADM/CHN_country.shp") )

plot(CHN_country$geometry)

#---------------------------------------------#
# 1. combine and clean names
#---------------------------------------------#

countries_all <- bind_rows(
  AFG, 
  NPL, 
  PAK_country, 
  PAK_ndlsa,
  IND_country, 
  IND_ndlsa,
  CHN_country, 
  CHN_ndlsa,
  MMR, 
  BGD, BTN
)|>
  st_make_valid()

#---------------------------------------------#
# create buffer but only land inwards
#---------------------------------------------#

# projected CRS in meters
IND_country_proj <- st_transform(IND_country, 8857)

# get country boundary line
IND_boundary <- st_boundary(IND_country_proj)

# 100 km buffer around boundary
boundary_buffer_100km <- st_buffer(IND_boundary, dist = 100000)

# keep only the part inside India
buffer_inward <- st_intersection(boundary_buffer_100km, IND_country_proj)

plot(st_geometry(IND_country_proj), col = "grey90", border = "grey40")
plot(st_geometry(buffer_inward), add = TRUE, col = "red", border = NA)

#---------------------------------------------#
# crop to hkh boundary
#---------------------------------------------#
hkh_boundary <- sf::st_read("~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/HKH_boundary/HKH_Boundary.shp")

# create buffer for hkh boundary 
hkh_boundary<- st_transform(hkh_boundary, 8857)
hkh_buffer_100km <- st_buffer(hkh_boundary, dist = 100000)

# crop india buffer to buffered hkh boundary
IND_HKH <- st_intersection(IND_country_proj, hkh_buffer_100km)
plot(IND_HKH_buffer$geometry)
IND_HKH_buffer <- st_intersection(buffer_inward,hkh_buffer_100km)

#sf::st_write(IND_HKH_buffer, paste0(data_storage_path, "Datasets/transboundary/GADM/2IND_buffer_100km.shp") )


#---------------------------------------------#
# create buffer pairwise borders
#---------------------------------------------#
# projected CRS in meters
pairwiseborders_proj <- st_transform(pairwiseborders, 8857)


# set default + custom buffer distances in meters
pairwiseborders_proj <- pairwiseborders_proj |>
  mutate(
    buffer_m = case_when(
      country_1 %in% c("Z01", "Z02", "Z03", "Z04", "Z05", "Z06", "Z07") ~ 1000,
      country_2 %in% c("Z01", "Z02", "Z03", "Z04", "Z05", "Z06", "Z07") ~ 1000,
      TRUE ~ 5000
    )
  )|>
  filter(pair_id %in% c("BTN_IND", "AFG_PAK"))

# 100 km buffer around boundary
border_buffers <- pairwiseborders_proj |>
  mutate(
    geom = st_buffer(geom, dist = buffer_m)
  )

plot(pairwiseborders_proj$geom)
plot(border_buffers$geom)

countries_proj <- st_transform(countries_all, st_crs(border_buffers))

border_buffer_sides <- border_buffers |>
  st_intersection(countries_proj) |>
  filter(GID_0 == country_1 | GID_0 == country_2) |>
  mutate(
    border_side = GID_0,
    pair_id = paste(pmin(country_1, country_2),
                    pmax(country_1, country_2),
                    sep = "_")
  )

test<-border_buffer_sides|>filter(pair_id=="AFG_PAK")
plot(test$geom)

sf::st_write(border_buffer_sides, paste0(data_storage_path, "Datasets/transboundary/GADM/border_buffer_sides.gpkg") )

#---------------------------------------------#
# overlap with protected areas 
#---------------------------------------------#