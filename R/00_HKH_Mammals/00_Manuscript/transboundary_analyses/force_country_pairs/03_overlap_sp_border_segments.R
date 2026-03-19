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

# read HKH countries again
countries <- ne_countries(scale = "medium", returnclass = "sf")

hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh")

countries_hkh <- countries |>
  filter(name %in% hkh_countries) |>
  st_make_valid() |>
  st_transform(st_crs(border_segments))


#---------------------------------------------#
# create buffer around segments
#---------------------------------------------#

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

#---------------------------------------------#
# get overview dataframes 
#---------------------------------------------#


presence_summary <- presence_long |>
  group_by(seg_id, species) |>
  summarise(
    n_countries_present = sum(presence, na.rm = TRUE),
    countries_present = paste(sort(unique(country[presence == 1])), collapse = ", "),
    .groups = "drop"
  ) |>
  mutate(
    shared = ifelse(n_countries_present >= 2, 1, 0),
    one_country_only = ifelse(n_countries_present == 1, 1, 0),
    near_border = ifelse(n_countries_present >= 1, 1, 0)
  )

segment_summary <- presence_summary |>
  group_by(seg_id) |>
  summarise(
    n_shared = sum(shared, na.rm = TRUE),
    n_one_country_only = sum(one_country_only, na.rm = TRUE),
    n_near_border = sum(near_border, na.rm = TRUE),
    prop_shared = ifelse(n_near_border > 0, n_shared / n_near_border, 0),
    .groups = "drop"
  )

segment_species_lists <- presence_summary |>
  filter(shared == 1) |>
  group_by(seg_id) |>
  summarise(
    species_list = paste(species, collapse = ", "),
    .groups = "drop"
  )

border_segments_sum <- border_segments |>
  left_join(segment_summary, by = "seg_id") |>
  left_join(segment_species_lists, by = "seg_id") |>
  mutate(
    country_str = paste(country_a, country_b, sep = " - "),
    species_list = species_list |>
      stringr::str_remove_all("_elev_masked") |>
      stringr::str_replace_all("_", " ")
  )
#---------------------------------------------#
# plot 
#---------------------------------------------#

plot<-ggplot() +
  geom_sf(data = countries_hkh, fill = "grey86", color = "grey5", linewidth = 0.2) +
  geom_sf(
    data = border_segments_sum,
    aes(color = n_shared, linewidth = n_shared),
    show.legend = "line"
  ) +
  scale_color_viridis_c(name = "Shared threatened\nspecies") +
  scale_linewidth(range = c(0.5, 2.5), guide = "none") +
  theme_void() +
  labs(title = "Shared threatened mammals along HKH border segments")

plot(plot)

#---------------------------------------------#
# safe 
#---------------------------------------------#
sapply(border_segments_sum, class)

sf::st_write(
  border_segments_sum,
  paste0(data_storage_path, "Output/transboundary/transb_glob_threat_new.shp"),
  delete_layer = TRUE
)



# 
ggsave(
  filename = paste0(data_storage_path, "Output/transboundary/transb_glob_threat.png"),
  plot = plot,
  width = 14,
  height = 9,
  dpi = 300
)
