library(sf)
library(dplyr)
library(purrr)
library(rnaturalearth)
library(lwgeom)
library(terra)

source(here::here("R/00_Config_file_HKH.R"))

#---------------------------------------------#
# 1. get country borders world bank 2025
#---------------------------------------------#

borders_WB <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/World Bank Official Boundaries - Admin 0_all_layers/WB_GAD_ADM0_complete.shp"))


# get the hkh countries and also areas with non determined legal status 
hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh")


ndls <- borders_WB |>
  filter(
    NAM_0 %in% hkh_countries |
      WB_STATUS == "Non-determined legal status area"
  )


#---------------------------------------------#
# get non determined legal status areas 
#---------------------------------------------#
ndls_crop <- st_intersection(ndls, hkh_boundary)

ndls_hkh_only <- ndls_crop|>
  filter( WB_STATUS == "Non-determined legal status area")
  
plot(ndls_hkh_only$geometry)



ndls <- st_intersection(ndls,hkh_boundary)|>
  filter(WB_STATUS== "Non-determined legal status area")

ndls_hkh_only$NAM_0



sf::st_write(
  ndls_hkh_only,
  paste0(data_storage_path, "Output/transboundary/ndls.shp"),
  delete_layer = TRUE
)


#---------------------------------------------#
# all countries 
#---------------------------------------------#
countries_full <- borders_WB |>
  filter(
    NAM_0 %in% hkh_countries |
      WB_STATUS == "Non-determined legal status area"
  ) |>
  mutate(
    NAM_0 = if_else(
      WB_STATUS == "Non-determined legal status area",
      "NDLS",
      NAM_0
    )
  ) |>
  group_by(NAM_0) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid() |>
  st_transform(8857) |>
  rename(country = NAM_0)

countries_full <- st_make_valid(countries_full) |>
  st_transform(8857)

hkh_boundary <- sf::st_read("~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/HKH_boundary/HKH_Boundary.shp") 
hkh_boundary <- st_transform(hkh_boundary, st_crs(countries_full))

#devtools::install_github("ropensci/rnaturalearthhires")


#---------------------------------------------#
# find out which country borders touch each other
#---------------------------------------------#

touch_list <- st_touches(countries_full)

country_pairs <- tibble(
  i = rep(seq_along(touch_list), lengths(touch_list)),
  j = unlist(touch_list)
) |>
  filter(i < j) |>
  mutate(
    country_a = countries_full$country[i],
    country_b = countries_full$country[j],
    pair_id = paste(pmin(country_a, country_b), pmax(country_a, country_b), sep = "_")
  )


# get the border pairs
pairwise_borders_full <- map_dfr(seq_len(nrow(country_pairs)), function(k) {
  i <- country_pairs$i[k]
  j <- country_pairs$j[k]
  
  g <- st_intersection(
    st_boundary(countries_full[i, ]),
    st_boundary(countries_full[j, ])
  )
  
  if (nrow(g) == 0) return(NULL)
  
  g <- st_collection_extract(g, "LINESTRING")
  if (nrow(g) == 0) return(NULL)
  
  g$country_a <- country_pairs$country_a[k]
  g$country_b <- country_pairs$country_b[k]
  g$pair_id   <- country_pairs$pair_id[k]
  g
}) |>
  st_as_sf()


pairwise_borders_full <- pairwise_borders_full |>
  group_by(country_a, country_b, pair_id) |>
  summarise(
    geometry = st_line_merge(st_union(geometry)),
    .groups = "drop"
  )

x11()
plot(pairwise_borders_full$geometry)



# create a buffer around the borders
# maybe kick this part out
hkh_buf <- st_buffer(hkh_boundary, dist = 40000)
pairwise_borders_buf <- st_intersection(pairwise_borders_full, hkh_buf) 

#---------------------------------------------#
# get the protected areas 
#---------------------------------------------#
pa_path <- paste0(data_storage_path, "Datasets/protected_areas/PA_HKH/pa_in_hkh.gpkg")
pa <- st_read(pa_path)

pa <- st_make_valid(pa) |>
  st_transform(8857)
plot(pa$geom)


# intersect pa with country polygons
pa_by_country <- st_intersection(pa,countries_full)

#---------------------------------------------#
# create a buffer around border lines
#---------------------------------------------#
# define what "adjacent to the border" means
adj_dist_m <- 5000      # PA within 5 km of border

border_buf <- st_buffer(pairwise_borders_full, dist = adj_dist_m)


#---------------------------------------------#
# identify PAs within/ touching buffer zones
#---------------------------------------------#

# calculate % overall border protection 
# border protection one side 
# border protection both sides


line_length_m <- function(x) {
  if (is.null(x)) return(0)
  if (length(x) == 0) return(0)
  if (all(st_is_empty(x))) return(0)
  
  x_lines <- st_collection_extract(x, "LINESTRING")
  if (length(x_lines) == 0 || all(st_is_empty(x_lines))) return(0)
  
  as.numeric(sum(st_length(x_lines)))
}


calc_border_pa_direct <- function(border_row, countries_full, pa_by_country, adj_dist_m = 10000) {
  
  border <- st_make_valid(border_row)
  a <- border$country_a
  b <- border$country_b
  pair <- border$pair_id
  
  side_a <- countries_full |> filter(country == a)
  side_b <- countries_full |> filter(country == b)
  
  pa_a <- pa_by_country |> filter(country == a)
  pa_b <- pa_by_country |> filter(country == b)
  
  # 10 km buffer around border
  border_buf <- st_buffer(border, dist = adj_dist_m)
  
  # split that buffer into each country's side
  zone_a <- st_intersection(border_buf, side_a) |> st_make_valid()
  zone_b <- st_intersection(border_buf, side_b) |> st_make_valid()
  
  # PA that occurs within 10 km of border on each side
  pa_near_a <- if (nrow(pa_a) > 0) {
    st_intersection(pa_a, zone_a) |> st_make_valid()
  } else {
    NULL
  }
  
  pa_near_b <- if (nrow(pa_b) > 0) {
    st_intersection(pa_b, zone_b) |> st_make_valid()
  } else {
    NULL
  }
  
  # Convert "PA within 10 km" back into border-line portions
  # by buffering the near-border PA by 10 km and intersecting with the border line
  line_a <- if (!is.null(pa_near_a) && nrow(pa_near_a) > 0) {
    pa_near_a |>
      st_union() |>
      st_buffer(adj_dist_m) |>
      st_intersection(st_geometry(side_a)) |>
      st_intersection(st_geometry(border))
  } else {
    st_sfc(st_geometrycollection(), crs = st_crs(border))
  }
  
  line_b <- if (!is.null(pa_near_b) && nrow(pa_near_b) > 0) {
    pa_near_b |>
      st_union() |>
      st_buffer(adj_dist_m) |>
      st_intersection(st_geometry(side_b)) |>
      st_intersection(st_geometry(border))
  } else {
    st_sfc(st_geometrycollection(), crs = st_crs(border))
  }
  
  border_line <- st_union(st_geometry(border))
  
  any_line <- st_union(line_a, line_b)
  both_line <- st_intersection(line_a, line_b)
  one_side_line <- st_difference(any_line, both_line)
  neither_line <- st_difference(border_line, any_line)
  
  total_m <- line_length_m(border_line)
  a_m <- line_length_m(line_a)
  b_m <- line_length_m(line_b)
  any_m <- line_length_m(any_line)
  both_m <- line_length_m(both_line)
  one_side_m <- line_length_m(one_side_line)
  neither_m <- line_length_m(neither_line)
  
  tibble(
    pair_id = pair,
    country_a = a,
    country_b = b,
    total_m = total_m,
    protected_any_m = any_m,
    protected_both_m = both_m,
    protected_one_side_m = one_side_m,
    protected_a_side_m = a_m,
    protected_b_side_m = b_m,
    unprotected_m = neither_m,
    pct_protected_any = 100 * any_m / total_m,
    pct_both = 100 * both_m / total_m,
    pct_one_side = 100 * one_side_m / total_m,
    pct_unprotected = 100 * neither_m / total_m
  )
}



# check results in arcgis !!
pair_summary <- map_dfr(seq_len(nrow(pairwise_borders_full)), function(i) {
  calc_border_pa_direct(
    border_row = pairwise_borders_full[i, ],
    countries_full = countries_full,
    pa_by_country = pa_by_country,
    adj_dist_m = 10000
  )
})




overall_summary <- pair_summary |>
  summarise(
    total_m = sum(total_m),
    protected_any_m = sum(protected_any_m),
    protected_both_m = sum(protected_both_m),
    protected_one_side_m = sum(protected_one_side_m),
    unprotected_m = sum(unprotected_m)
  ) |>
  mutate(
    pct_protected_any = 100 * protected_any_m / total_m,
    pct_both = 100 * protected_both_m / total_m,
    pct_one_side = 100 * protected_one_side_m / total_m,
    pct_unprotected = 100 * unprotected_m / total_m
  )


nrow(pa_near_a)
sum(st_area(pa_near_a))
plot(st_geometry(zone_a))
plot(st_geometry(pa_a), add = TRUE, border = "grey")
plot(st_geometry(pa_near_a), add = TRUE, col = "red")

plot(pairwise_borders_full$geometry)
plot(countries_full$geometry)
