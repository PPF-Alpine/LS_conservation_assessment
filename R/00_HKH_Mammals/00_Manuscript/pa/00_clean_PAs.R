library(terra)
library(sf)
library(sf)
library(dplyr)
library(units)

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# 0. load files  -----
#----------------------------------------------------------#

pa <- sf::st_read(
  "C:/Users/losch5089/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/protected_areas/PA_HKH/PA_HKH_complete_clip.shp"
)

#----------------------------------------------------------#
# CLEANING PAs---
#----------------------------------------------------------#

# TO DO CLEANING PAs

# buffer zones need to be dissolved 
# India and China PAs have to be added✅ (arcgis)
# transboundary PAs have to be marked
# ICIMOD transboundary landscapes need to be added 


# prep one clean PA + transb lanscape shp 

#----------------------------------------------------------#
# select relevant columns --
#----------------------------------------------------------#

pa_select <- pa |>
  dplyr::select(
    WDPAID,
    name,
    DESIG_E,
    type,
    geometry
  )

# Nanda Devi
# Khaptad
# Suklaphanta
# Bardia
# Banke
# Shey-Phoksundo
# Rara
# Langtang
# Chitwan
# Parsa
# Sagarmatha
# Makalu-Barun
# Khangchendzonga National Park
# Govind Pashu
# Great Himalayan National Park


library(sf)
library(dplyr)
library(stringr)

merge_names <- c(
  "Nanda Devi",
  "Khaptad",
  "Suklaphanta",
  "Bardia",
  "Banke",
  "Shey-Phoksundo",
  "Rara",
  "Langtang",
  "Chitwan",
  "Parsa",
  "Sagarmatha",
  "Makalu-Barun",
  "Khangchendzonga National Park",
  "Govind Pashu",
  "Great Himalayan National Park"
)

pa_merge <- pa_select |>
  mutate(
    merge_group = case_when(
      str_detect(name, regex(str_c(merge_names, collapse = "|"), ignore_case = TRUE)) ~
        str_extract(name, regex(str_c(merge_names, collapse = "|"), ignore_case = TRUE)),
      TRUE ~ name
    )
  )

check <- pa_merge |>
  st_drop_geometry() |>
  group_by(merge_group) |>
  summarise(
    n = n(),
    original_names = paste(unique(name), collapse = " | ")
  ) |>
  filter(n > 1) |>
  arrange(desc(n))

check


sf_use_s2(FALSE)

pa_merge_clean <- pa_merge |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_make_valid() |>
  st_collection_extract("POLYGON")

pa_dissolved <- pa_merge_clean |>
  group_by(merge_group) |>
  summarise(
    WDPAID = paste(unique(WDPAID), collapse = "; "),
    name = first(merge_group),
    DESIG_E = paste(unique(DESIG_E), collapse = "; "),
    type = paste(unique(type), collapse = "; "),
    .groups = "drop",
    do_union = TRUE
  ) |>
  select(WDPAID, name, DESIG_E, type, geometry)



#----------------------------------------------------------#
# add columns for transboundary PA --
#----------------------------------------------------------#

border_segments <- sf::st_read(
  file.path(data_storage_path, "Datasets/protected_areas/borders_segments_100.shp")
)

# Make sure CRS matches PA layer
pa_dissolved <- st_transform(pa_dissolved, st_crs(border_segments))

buffer_dist <- 10000  # meters

segment_buffers <- border_segments |>
  st_make_valid() |>
  st_buffer(dist = buffer_dist)

# Add yes/no column
pa_dissolved <- pa_dissolved |>
  st_make_valid() |>
  mutate(
    transboundary = if_else(
      lengths(st_intersects(geometry, segment_buffers)) > 0,
      "yes",
      "no"
    )
  )

pa_diss_check <- pa_dissolved|>
  filter(transboundary == "yes")

sf::st_write(pa_dissolved,
             "C:/Users/losch5089/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/protected_areas/PA_HKH/PA_HKH_complete_clean.shp",append=FALSE)
