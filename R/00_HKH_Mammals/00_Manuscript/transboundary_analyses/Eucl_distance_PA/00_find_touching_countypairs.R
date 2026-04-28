
library(sf)
library(dplyr)
library(purrr)
library(rnaturalearth)
library(lwgeom)
library(terra)

# FIX CHINA # 

source(here::here("R/00_Config_file_HKH.R"))
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

# World bank
CHN_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/CHN_WB.shp"))|>
  select(SOVEREIGN, NAM_0, geometry) |>
  rename(
    GID_0 = SOVEREIGN,
    COUNTRY = NAM_0
  )

# GADM
CHN_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/CHN_snapped.shp"))

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


plot(countries_all$geometry)
unique(countries_all$GID_0)

countries_all <- countries_all |>
  st_transform(3857) |>
  st_make_valid()

#---------------------------------------------#
#  find touching country pairs
#---------------------------------------------#

touch_list <- st_touches(countries_all)


rel_list <- st_relate(
  countries_all,
  countries_all,
  pattern = "****1****"
)

pairs <- tibble(
  i = rep(seq_along(rel_list), lengths(rel_list)),
  j = unlist(rel_list)
) |>
  filter(i < j)

pairs_named <- pairs |>
  mutate(
    country_i = countries_all$GID_0[i],
    country_j = countries_all$GID_0[j]
  ) |>
  arrange(country_i, country_j)

pairs_named



#---------------------------------------------#
# extract shared border for each country pair
#---------------------------------------------#

border_list <- map(seq_len(nrow(pairs)), function(k) {
  
  i <- pairs$i[k]
  j <- pairs$j[k]
  
  # geometry only
  geom_i <- st_boundary(st_geometry(countries_all[i, ]))
  geom_j <- st_boundary(st_geometry(countries_all[j, ]))
  
  g <- suppressWarnings(st_intersection(geom_i, geom_j))
  
  if (length(g) == 0) return(NULL)
  if (all(st_is_empty(g))) return(NULL)
  
  # keep only line parts
  g <- tryCatch(
    st_collection_extract(g, "LINESTRING"),
    error = function(e) g
  )
  
  g <- tryCatch(
    st_cast(g, "LINESTRING"),
    error = function(e) NULL
  )
  
  if (is.null(g) || length(g) == 0) return(NULL)
  if (all(st_is_empty(g))) return(NULL)
  
  st_sf(
    country_a = rep(countries_all$GID_0[i], length(g)),
    country_b = rep(countries_all$GID_0[j], length(g)),
    geometry = g,
    crs = st_crs(countries_all)
  )
})

# remove NULLs
border_list <- border_list[!sapply(border_list, is.null)]

# combine only after everything is clean
pairwise_borders <- do.call(rbind, border_list)

#---------------------------------------------#
# full border length for each touching country pair
#---------------------------------------------#

pairwise_border_lengths <- pairwise_borders |>
  mutate(
    country_1 = pmin(country_a, country_b),
    country_2 = pmax(country_a, country_b),
    pair_id = paste(country_1, country_2, sep = "_")
  ) |>
  group_by(country_1, country_2, pair_id) |>
  summarise(
    geometry = st_combine(geometry),
    .groups = "drop"
  ) |>
  st_cast("MULTILINESTRING") |>
  mutate(
    border_length_m = as.numeric(st_length(geometry)),
    border_length_km = border_length_m / 1000
  )

x11()
plot(pairwise_border_lengths$geometry)

# pairwise borders dataframe 
pairwise_borders_summary <- pairwise_border_lengths|>
  sf::st_drop_geometry()

#---------------------------------------------#
# save pairwise borders 
#---------------------------------------------#

sf::st_write(pairwise_border_lengths, paste0(data_storage_path, "Datasets/transboundary/GADM/GADM_pairwiseborders_new.gpkg"),append=FALSE )

#---------------------------------------------#
# FIX CHINA  
#---------------------------------------------#
countries_all |>
  st_drop_geometry() |>
  mutate(row_id = row_number()) |>
  select(row_id, GID_0)

chn <- countries_all |> filter(GID_0 == "CHN")
ind <- countries_all |> filter(GID_0 == "IND")
npl <- countries_all |> filter(GID_0 == "NPL")
btn <- countries_all |> filter(GID_0 == "BTN")

st_distance(chn, ind)
st_distance(chn, npl)
st_distance(chn, btn)

st_intersects(chn, ind, sparse = FALSE)
st_overlaps(chn, ind, sparse = FALSE)
st_relate(chn, ind)

