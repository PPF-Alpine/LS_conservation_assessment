library(sf)
library(dplyr)
library(purrr)
library(rnaturalearth)
library(lwgeom)
library(terra)

#---------------------------------------------#
# 1. get HKH country polygons
#---------------------------------------------#

hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh")

countries_hkh <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name %in% hkh_countries) |>
  st_make_valid()

# use same projected CRS for lengths in metres
countries_hkh <- st_transform(countries_hkh, 8857)


#---------------------------------------------#
# 2. find touching country pairs
#---------------------------------------------#

touch_list <- st_touches(countries_hkh)

country_pairs <- tibble(
  i = rep(seq_along(touch_list), lengths(touch_list)),
  j = unlist(touch_list)
) |>
  filter(i < j)

#---------------------------------------------#
# 3. extract shared border for each country pair
#---------------------------------------------#

pairwise_borders <- map_dfr(seq_len(nrow(country_pairs)), function(k) {
  
  i <- country_pairs$i[k]
  j <- country_pairs$j[k]
  
  # shared boundary between exactly these two countries
  g <- st_intersection(
    st_boundary(countries_hkh[i, ]),
    st_boundary(countries_hkh[j, ])
  )
  
  if (nrow(g) == 0) return(NULL)
  
  # keep only line geometries
  g <- st_collection_extract(g, "LINESTRING")
  if (nrow(g) == 0) return(NULL)
  
  g$country_a <- countries_hkh$name[i]
  g$country_b <- countries_hkh$name[j]
  g
}) |>
  st_as_sf()


#---------------------------------------------#
# full border length for each touching country pair
#---------------------------------------------#

pairwise_border_lengths <- pairwise_borders |>
  st_transform(8857) |>
  group_by(country_a, country_b) |>
  summarise(
    geometry = st_line_merge(st_union(geometry)),
    .groups = "drop"
  ) |>
  mutate(
    pair_id = paste(country_a, country_b, sep = "_"),
    border_length_m = as.numeric(st_length(geometry)),
    border_length_km = border_length_m / 1000
  )|>
  mutate(seg_id = row_number())

#---------------------------------------------#
# 6. inspect
#---------------------------------------------#
pairwise_border_lengths |>
  st_drop_geometry()


#---------------------------------------------#
# safe
#---------------------------------------------#

sf::st_write(
  pairwise_border_lengths,
  paste0(data_storage_path, "Output/transboundary/full_borderlenght_countrypairs.gpkg"),
  delete_layer = TRUE
)

