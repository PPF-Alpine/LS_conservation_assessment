library(sf)
library(terra)
library(rnaturalearth)
# pkgs
library(terra)        # you already use this
library(tidyterra)    # ggplot geoms for terra rasters
library(ggplot2)
library(patchwork)    # to arrange plots

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))
#---------------------------------------------#
# get border segments, countries and cons priority
#---------------------------------------------#

# these are the border segments geometries with line id and a/b country information 
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))

# numerical conservatin priority raster 
cons_prio <- terra::rast(paste0(data_storage_path, "Output/priority_indices/priority_mapp_all_combo.tif"))
plot(cons_prio)
cons_prio_eq <- terra::project(cons_prio, crs(border_segments), method = "near")


# dataframe that corresponds to the different classes 
levels_df <- data.frame(
  value = c(111,110,121,120,131,130,
            211,210,221,220,231,230,
            311,310,321,320,331,330),
  biodiv = rep(c("low","medium","high"), each = 6),
  climate = rep(c("low","low","medium","medium","high","high"), times = 3),
  protection = rep(c("protected","unprotected"), 9)
)

levels_df$class_name <- paste(
  "Biodiv:", levels_df$biodiv,
  "| Climate:", levels_df$climate,
  "|", levels_df$protection
)

levels_df


#  HKH countries 
countries <- ne_countries(scale = "medium", returnclass = "sf")

hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh")

countries_hkh <- countries |>
  filter(name %in% hkh_countries) |>
  st_make_valid() |>
  st_transform(st_crs(border_segments))


countries_hkh_eq <- countries_hkh |>
  st_transform(st_crs(border_segments))


#---------------------------------------------#
# create buffer around segments
#---------------------------------------------#

# create buffer around each segment

buffer_dist <- 20000

segment_buffers <- st_buffer(border_segments, dist = buffer_dist)

#---------------------------------------------#
# get sides for each segment/ country
#---------------------------------------------#

# get the area of each countries that falls within buffer of border segments 
make_segment_sides <- function(seg_row, segment_buffers, countries_hkh_eq) {
  
  seg_id <- seg_row$seg_id
  ctry_a <- seg_row$country_a
  ctry_b <- seg_row$country_b
  
  buf <- segment_buffers |> filter(seg_id == !!seg_id)
  
  poly_a <- countries_hkh_eq |> filter(name == ctry_a)
  poly_b <- countries_hkh_eq |> filter(name == ctry_b)
  
  side_a <- st_intersection(buf, poly_a) |>
    mutate(
      seg_id = seg_id,
      country = ctry_a,
      side = "A"
    )
  
  side_b <- st_intersection(buf, poly_b) |>
    mutate(
      seg_id = seg_id,
      country = ctry_b,
      side = "B"
    )
  
  bind_rows(side_a, side_b)
}


# apply
segment_sides <- map_dfr(
  seq_len(nrow(border_segments)),
  function(i) {
    make_segment_sides(
      seg_row = border_segments[i, ],
      segment_buffers = segment_buffers,
      countries_hkh_eq = countries_hkh_eq
    )
  }
)


segment_sides_vect <- terra::vect(segment_sides)

plot(segment_sides_vect)

#writeVector(segment_sides_vect,paste0(data_storage_path, "Output/transboundary/segment_sides_vect.shp"))

#---------------------------------------------#
# extract cons prio for each segment side
#---------------------------------------------#

prio_extract <- terra::extract(cons_prio_eq, segment_sides_vect)

#terra::crs(cons_prio)
#terra::crs(cons_prio_eq)
#terra::crs(segment_sides_vect)


plot(cons_prio_eq)
plot(segment_sides_vect, add = TRUE, border = "red")


# add segment info
prio_extract$seg_id   <- segment_sides$seg_id[prio_extract$ID]
prio_extract$country  <- segment_sides$country[prio_extract$ID]
prio_extract$side     <- segment_sides$side[prio_extract$ID]

names(prio_extract)[2] <- "value"


#---------------------------------------------#
# save 
#---------------------------------------------#

sf::st_write(
  prio_extract,
  paste0(data_storage_path, "Output/transboundary/border_segments_consprio_biodivimp.gpkg"),
  delete_layer = TRUE
)
