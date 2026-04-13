library(sf)
library(dplyr)
library(purrr)
library(rnaturalearth)
library(lwgeom)
library(terra)

#---------------------------------------------#
# 1. get country borders world bank 2025
#---------------------------------------------#


# use same projected CRS for lengths in metres
sf::st_write(countries_hkh,paste0(data_storage_path, "Datasets/transboundary/borders_naturalearth_new.shp"))

borders_WB <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/World Bank Official Boundaries - Admin 0_all_layers/WB_GAD_ADM0_complete.shp"))

hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh")

borders_hkh <- borders_WB |>
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
  )|>
  group_by(NAM_0) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

plot(borders_hkh$geometry)


# crop borders to raster extent
hkh_boundary<- sf::st_read("~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/HKH_boundary/HKH_Boundary.shp")

#devtools::install_github("ropensci/rnaturalearthhires")
#---------------------------------------------#
# crop to hkh extent 
#---------------------------------------------#

border_hkh <- st_intersection(borders_hkh,hkh_boundary)
plot(border_hkh$geometry)



#---------------------------------------------#
# I need to join the countries of each side of border 
#---------------------------------------------#

touch_list <- st_touches(border_hkh)

country_pairs <- tibble(
  i = rep(seq_along(touch_list), lengths(touch_list)),
  j = unlist(touch_list)
) |>
  filter(i < j) |>
  mutate(
    country_a = border_hkh$NAM_0[i],
    country_b = border_hkh$NAM_0[j],
    pair_id = paste(pmin(country_a, country_b), pmax(country_a, country_b), sep = "_")
  )

country_pairs


# this part produces holes. 
pairwise_borders <- map_dfr(seq_len(nrow(country_pairs)), function(k) {
  
  i <- country_pairs$i[k]
  j <- country_pairs$j[k]
  
  g <- st_intersection(
    st_boundary(border_hkh[i, ]),
    st_boundary(border_hkh[j, ])
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

plot(pairwise_borders$geometry)

pairwise_borders_merged <- pairwise_borders |>
  group_by(country_a, country_b, pair_id) |>
  summarise(
    geometry = st_line_merge(st_union(geometry)),
    .groups = "drop"
  )|>
  st_transform(8857) |>
  mutate(
    border_length_m = as.numeric(st_length(geometry)),
    border_length_km = border_length_m / 1000,
    seg_id = row_number()
  )

plot(st_geometry(border_hkh), col = NA, border = "grey50")
plot(st_geometry(pairwise_borders_merged), add = TRUE, col = "red", lwd = 2)

#---------------------------------------------#
# get the protected areas 
#---------------------------------------------#
pa_path <- paste0(data_storage_path, "Datasets/protected_areas/PA_HKH/pa_in_hkh.gpkg")
pa <- st_read(pa_path)

#---------------------------------------------#
# 4. prepare PA layer
#---------------------------------------------#

pa <- st_make_valid(pa) |>
  st_transform(8857)
plot(pa$geom)


#---------------------------------------------#
# 5. border settings
#---------------------------------------------#
pairwise_borders_lines <- pairwise_borders_merged |>
  st_cast("LINESTRING") |>
  mutate(
    line_id = row_number(),
    line_length_m = as.numeric(st_length(geometry)),
    line_length_km = line_length_m / 1000
  )

x11()
plot(pairwise_borders_merged)

# define what "adjacent to the border" means
adj_dist_m <- 5000      # PA within 5 km of border
sample_dist_m <- 1000   # sample every 1 km along border

#---------------------------------------------#
# create a buffer around border lines
#---------------------------------------------#

border_buf <- st_buffer(pairwise_borders_lines, dist = adj_dist_m)

border_pa <- st_intersection(
  border_buf |> select(pair_id, country_a, country_b, seg_id),
  pa_country
) |>
  st_drop_geometry() |>
  mutate(
    side = case_when(
      country == country_a ~ "A",
      country == country_b ~ "B",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(side)) |>
  distinct(pair_id, country_a, country_b, seg_id, side)




library(tidyr)

border_status <- border_pa |>
  mutate(has_pa = TRUE) |>
  pivot_wider(
    names_from = side,
    values_from = has_pa,
    values_fill = FALSE,
    names_prefix = "pa_"
  ) |>
  mutate(
    border_protection = case_when(
      pa_A & pa_B ~ "both sides protected",
      pa_A & !pa_B ~ "only side A protected",
      !pa_A & pa_B ~ "only side B protected",
      TRUE ~ "neither side protected"
    )
  )


pairwise_borders_lines <- pairwise_borders_lines |>
  mutate(
    line_length_m = as.numeric(st_length(geometry)),
    line_length_km = line_length_m / 1000
  )


borders_status <- pairwise_borders_lines |>
  left_join(
    border_status,
    by = c("pair_id", "country_a", "country_b", "seg_id")
  ) |>
  mutate(
    pa_A = if_else(is.na(pa_A), FALSE, pa_A),
    pa_B = if_else(is.na(pa_B), FALSE, pa_B),
    border_protection = if_else(
      is.na(border_protection),
      "neither side protected",
      border_protection
    )
  )



overall_border_protection <- borders_status |>
  summarise(
    total_length_km = sum(line_length_km, na.rm = TRUE),
    protected_length_km = sum(line_length_km[pa_A | pa_B], na.rm = TRUE),
    unprotected_length_km = sum(line_length_km[!(pa_A | pa_B)], na.rm = TRUE),
    prop_protected = protected_length_km / total_length_km * 100,
    prop_unprotected = unprotected_length_km / total_length_km * 100
  )

border_side_summary <- borders_status |>
  group_by(border_protection) |>
  summarise(
    total_length_km = sum(line_length_km, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    prop = total_length_km / sum(total_length_km) * 100
  )

sample_border_points <- function(line_sf, dist_m = 1000) {
  
  line_len <- as.numeric(st_length(line_sf))
  n_pts <- max(1, floor(line_len / dist_m))
  
  pts <- st_line_sample(line_sf, n = n_pts, type = "regular")
  pts <- st_cast(pts, "POINT")
  
  st_sf(
    pair_id = line_sf$pair_id,
    country_a = line_sf$country_a,
    country_b = line_sf$country_b,
    seg_id = line_sf$seg_id,
    line_id = line_sf$line_id,
    geometry = pts
  )
}

border_pts <- map_dfr(seq_len(nrow(pairwise_borders_lines)), function(i) {
  sample_border_points(pairwise_borders_lines[i, ], dist_m = sample_dist_m)
}) |>
  mutate(pt_id = row_number())



border_pts_buf <- st_buffer(border_pts, dist = adj_dist_m)

x11()
plot(border_pts_buf$geometry)

#---------------------------------------------#
# intersect borders and PAs 
#---------------------------------------------#
pt_pa <- st_intersection(
  border_pts_buf |> 
    select(pt_id, pair_id, country_a, country_b, seg_id, line_id),
  pa_country
) |>
  st_drop_geometry() |>
  mutate(
    side = case_when(
      country == country_a ~ "A",
      country == country_b ~ "B",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(side)) |>
  distinct(pt_id, pair_id, country_a, country_b, seg_id, line_id, side)

plot(pt_pa)
#---------------------------------------------#
# find overall % of border protection 
#---------------------------------------------#
pt_status <- pt_pa |>
  mutate(has_pa = TRUE) |>
  pivot_wider(
    names_from = side,
    values_from = has_pa,
    values_fill = FALSE,
    names_prefix = "pa_"
  )

border_pts_status <- border_pts |>
  st_drop_geometry() |>
  left_join(
    pt_status,
    by = c("pt_id", "pair_id", "country_a", "country_b", "seg_id", "line_id")
  ) |>
  mutate(
    pa_A = if_else(is.na(pa_A), FALSE, pa_A),
    pa_B = if_else(is.na(pa_B), FALSE, pa_B),
    border_protection = case_when(
      pa_A & pa_B ~ "both sides protected",
      pa_A & !pa_B ~ "only side A protected",
      !pa_A & pa_B ~ "only side B protected",
      TRUE ~ "neither side protected"
    )
  )

overall_border_protection <- border_pts_status |>
  summarise(
    total_points = n(),
    protected_points = sum(pa_A | pa_B),
    unprotected_points = sum(!(pa_A | pa_B)),
    prop_protected = protected_points / total_points * 100,
    prop_unprotected = unprotected_points / total_points * 100
  )

overall_border_protection
#---------------------------------------------#
# find protection on side a/ b only vs both sides protected
#---------------------------------------------#

border_side_summary <- border_pts_status |>
  count(border_protection) |>
  mutate(
    prop = n / sum(n) * 100
  )

border_side_summary
