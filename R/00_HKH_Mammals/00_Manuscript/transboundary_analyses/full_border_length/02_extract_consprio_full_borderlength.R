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
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/full_borderlenght_countrypairs.gpkg"))
plot(border_segments$geom)
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
prio_extract$pair_id     <- segment_sides$pair_id[prio_extract$ID]
prio_extract$border_length_km <-segment_sides$border_length_km[prio_extract$ID]

names(prio_extract)[2] <- "value"

prio_extract_filtered <- prio_extract|>
  filter(!is.na(value)) |>
  group_by(seg_id) |>
  mutate(total_cells_n = n()) |>
  ungroup()
#---------------------------------------------#
# summarise and visualize 
#---------------------------------------------#

value_summary <- prio_extract_filtered |>
  filter(!is.na(value)) |>
  group_by(seg_id, side, value,country,pair_id,border_length_km) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(seg_id, side) |>
  mutate(
    total_cells_n = sum(n),
    prop = round(n / total_cells_n * 100, 4)
  ) |>
  ungroup()|>
  mutate(
    protection = if_else(
      value %in% c(111,121,131,211,221,231,311,321,331),
      "protected",
      "unprotected"
    )
  )|>
  group_by(seg_id, side, country) |>
  mutate(
    prop_protected = sum(prop[protection == "protected"]),
    prop_unprotected = sum(prop[protection == "unprotected"])
  ) |>
  ungroup()

#---------------------------------------------#
# plot  
#---------------------------------------------#

plot_data <- value_summary |>
  distinct(seg_id, side, country, pair_id, prop_protected, prop_unprotected) |>
  pivot_longer(
    cols = c(prop_protected, prop_unprotected),
    names_to = "protection",
    values_to = "prop"
  ) |>
  mutate(
    protection = recode(
      protection,
      prop_protected = "protected",
      prop_unprotected = "unprotected"
    )
  )


ggplot(plot_data, aes(x = interaction(pair_id, country, lex.order = TRUE),
                      y = prop, fill = protection)) +
  geom_col(width = 0.7) +
  labs(
    x = "Country within pair",
    y = "Percent of extracted cells",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggplot(plot_data, aes(x = country, y = prop, fill = protection)) +
  geom_col(width = 0.7) +
  facet_wrap(~ pair_id, scales = "free_x") +
  labs(
    x = NULL,
    y = "Percent of extracted cells",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#---------------------------------------------#
# save 
#---------------------------------------------#

sf::st_write(
  prio_extract,
  paste0(data_storage_path, "Output/transboundary/fullborderlength_consprio_biodivimp.gpkg"),
  delete_layer = TRUE
)
