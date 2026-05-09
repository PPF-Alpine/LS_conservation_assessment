library(sf)
library(terra)
library(dplyr)
library(purrr)
library(tidyr)
library(rnaturalearth)


#---------------------------------------------#
# get border segments
#---------------------------------------------#

border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))
plot(border_segments$geom)

# read HKH countries again
countries <- ne_countries(scale = "medium", returnclass = "sf")

hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh","Kashmir")

countries_hkh <- countries |>
  filter(name %in% hkh_countries) |>
  st_make_valid() |>
  st_transform(st_crs(border_segments))

plot(countries_hkh$geometry)

#---------------------------------------------#
# create buffer around segments
#---------------------------------------------#
buffer_dist <- 10000  # meters

segment_buffers <- st_buffer(border_segments, dist = buffer_dist)


#---------------------------------------------#
# sides of segments --> countries
#---------------------------------------------#

make_segment_sides_pair <- function(seg_row, countries_hkh, segment_buffers) {
  
  seg_id <- seg_row$seg_id
  ctry_a <- seg_row$country_a
  ctry_b <- seg_row$country_b
  
  buf <- segment_buffers |> filter(seg_id == !!seg_id)
  
  poly_a <- countries_hkh |> filter(name == ctry_a)
  poly_b <- countries_hkh |> filter(name == ctry_b)
  
  side_a <- st_intersection(buf, poly_a) |>
    mutate(seg_id = seg_id, country = ctry_a, side = "a")
  
  side_b <- st_intersection(buf, poly_b) |>
    mutate(seg_id = seg_id, country = ctry_b, side = "b")
  
  bind_rows(side_a, side_b)
}

segment_sides <- purrr::map_dfr(
  seq_len(nrow(border_segments)),
  function(i) make_segment_sides_pair(
    seg_row = border_segments[i, ],
    countries_hkh = countries_hkh,
    segment_buffers = segment_buffers
  )
)

segment_sides_vect <- terra::vect(segment_sides)
#---------------------------------------------#
# species present
#---------------------------------------------#

extract_species_presence <- function(rfile, segment_sides, segment_sides_vect) {
  
  r <- rast(rfile)
  sp_name <- gsub("_al$", "", tools::file_path_sans_ext(basename(rfile)))
  
  ex <- terra::extract(r, segment_sides_vect, fun = max, na.rm = TRUE)
  
  tibble(
    seg_id   = segment_sides$seg_id,
    country  = segment_sides$country,
    side     = segment_sides$side,
    species  = sp_name,
    presence = ex[[2]]
  ) |>
    mutate(
      presence = ifelse(is.na(presence), 0, presence),
      presence = ifelse(presence > 0, 1, 0)
    )
}

presence_long <- purrr::map_dfr(
  aligned_files,
  extract_species_presence,
  segment_sides = segment_sides,
  segment_sides_vect = segment_sides_vect
)

