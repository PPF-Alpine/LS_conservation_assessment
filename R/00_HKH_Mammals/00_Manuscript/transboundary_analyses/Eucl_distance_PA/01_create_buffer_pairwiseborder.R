
library(sf)
library(dplyr)
library(purrr)
library(rnaturalearth)
library(lwgeom)
library(terra)

source(here::here("R/00_Config_file_HKH.R"))


# check out if that works with pairwise border length multipolygons: pairwiseborders_GADM.shp. .
pairwiseborders <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/GADM_pairwiseborders_new.gpkg"))

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

# GADM
CHN_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/CHN_snapped.shp"))

CHN_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/cleaned_CHN.shp"))|>
  dplyr::filter(GID_0 != "CHN")

MMR <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_MMR_shp/gadm41_MMR_0.shp"))

BGD <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_BGD_shp/gadm41_BGD_0.shp"))


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
# create buffer pairwise borders
#---------------------------------------------#

pairwiseborders$pair_id


# projected CRS in meters
pairwiseborders_8857 <- st_transform(pairwiseborders, 8857)

z_ids <- c("Z01", "Z02", "Z03", "Z04", "Z05", "Z06", "Z07", "Z08", "Z09")

pairwiseborders_proj <- pairwiseborders_8857 |>
  mutate(
    buffer_m = case_when(
      country_1 %in% c("Z04", "Z05","Z09") | country_2 %in% c("Z04", "Z05","Z09") ~ 5000,
      country_1 == "Z07" | country_2 == "Z07" ~ 10000,
      country_1 %in% z_ids | country_2 %in% z_ids ~ 10000,  # fallback for other Zs
      
      TRUE ~ 100000
    )
  ) |> 
  filter(pair_id %in% c("BGD_IND"))


# done 
# "AFG_PAK" "AFG_Z06" "BTN_IND" "BGD_MMR" "IND_MMR" "IND_NPL" "BTN_Z07" "IND_PAK" "IND_Z01" "IND_Z04" IND_Z05 "IND_Z07" "IND_Z09" "MMR_Z07" "PAK_Z01" "PAK_Z06"
# "Z01_Z03" "Z01_Z06" "Z01_Z08" "Z02_Z06" "CHN_NPL" "BTN_CHN"  "CHN_IND" "CHN_MMR"
#  "CHN_Z01" "CHN_Z02" "CHN_Z03" "CHN_Z04" "CHN_Z05" "CHN_Z06" "CHN_Z07" "CHN_Z08" "CHN_Z09" "AFG_CHN"


# to do
#    

# bad alloc:
# "BGD_IND" 

# Z07 =~150km 
# Z09 = 30 km 
# z05 = tiny
# Z04 = tiny
# z01 and Z 02 ~ 300km 

# 100 km buffer around boundary
border_buffers <- pairwiseborders_proj |>
  mutate(
    geom = st_buffer(geom, dist = buffer_m)
  )


# project borders to countries 
countries_proj <- st_transform(countries_all, st_crs(border_buffers))

border_buffer_sides <- border_buffers |>
  st_intersection(countries_proj) |>
  filter(GID_0 == country_1 | GID_0 == country_2) |>
  mutate(
    border_side = GID_0,
    pair_id = paste(pmin(country_1, country_2),
                    pmax(country_1, country_2),
                    sep = "_")
  )|>
  select(country_1,country_2,pair_id,border_length_m,border_length_km,buffer_m,GID_0,COUNTRY,geom,border_side)

#test<-border_buffer_sides|>filter(pair_id=="AFG_PAK")
plot(border_buffer_sides$geom)

st_write(
  border_buffer_sides,
  paste0(data_storage_path, "Datasets/transboundary/GADM/pairwise_border_buffers/pbb_AFG_CHN_final.gpkg")
)


