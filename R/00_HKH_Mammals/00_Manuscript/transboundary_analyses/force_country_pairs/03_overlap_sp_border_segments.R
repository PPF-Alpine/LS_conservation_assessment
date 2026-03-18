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
buffer_dist <- 10000  # meters

segment_buffers <- st_buffer(border_segments, dist = buffer_dist)


segment_country_hits <- st_intersects(segment_buffers, countries_hkh)

segment_countries <- tibble(
  seg_id = border_segments$seg_id,
  hit_ids = segment_country_hits
) |>
  mutate(
    n_countries = lengths(hit_ids),
    country_names = map(hit_ids, ~ countries_hkh$name[.x])
  )

table(segment_countries$n_countries)

valid_segments <- segment_countries |>
  filter(n_countries == 2) |>
  mutate(
    country_a = map_chr(country_names, 1),
    country_b = map_chr(country_names, 2)
  ) |>
  select(seg_id, country_a, country_b)


#---------------------------------------------#
# sides of segments --> countries
#---------------------------------------------#
make_segment_sides <- function(seg_id, country_a, country_b, segment_buffers, countries_hkh) {
  
  buf <- segment_buffers |> filter(seg_id == !!seg_id)
  
  poly_a <- countries_hkh |> filter(name == country_a)
  poly_b <- countries_hkh |> filter(name == country_b)
  
  side_a <- st_intersection(buf, poly_a) |>
    mutate(seg_id = seg_id, side = "A", country = country_a)
  
  side_b <- st_intersection(buf, poly_b) |>
    mutate(seg_id = seg_id, side = "B", country = country_b)
  
  bind_rows(side_a, side_b)
}


segment_sides <- pmap_dfr(
  valid_segments,
  make_segment_sides,
  segment_buffers = segment_buffers,
  countries_hkh = countries_hkh
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
    side     = segment_sides$side,
    country  = segment_sides$country,
    species  = sp_name,
    presence = ex[[2]]
  ) |>
    mutate(
      presence = ifelse(is.na(presence), 0, presence),
      presence = ifelse(presence > 0, 1, 0)
    )
}


presence_long <- map_dfr(
  aligned_files,
  extract_species_presence,
  segment_sides = segment_sides,
  segment_sides_vect = segment_sides_vect
)


#---------------------------------------------#
# get overview dataframes 
#---------------------------------------------#
presence_wide <- presence_long |>
  select(seg_id, species, side, presence) |>
  pivot_wider(
    names_from = side,
    values_from = presence,
    values_fill = 0
  ) |>
  mutate(
    shared = ifelse(A == 1 & B == 1, 1, 0),
    one_sided = ifelse((A + B) == 1, 1, 0),
    near_border = ifelse((A + B) >= 1, 1, 0)
  )


segment_summary <- presence_wide |>
  group_by(seg_id) |>
  summarise(
    n_shared = sum(shared, na.rm = TRUE),
    n_one_sided = sum(one_sided, na.rm = TRUE),
    n_near_border = sum(near_border, na.rm = TRUE),
    prop_shared = ifelse(n_near_border > 0, n_shared / n_near_border, 0),
    .groups = "drop"
  )


border_segments_sum <- border_segments |>
  left_join(valid_segments, by = "seg_id") |>
  left_join(segment_summary, by = "seg_id")

segment_species_lists <- presence_wide |>
  filter(shared == 1) |>
  group_by(seg_id) |>
  summarise(
    species_list = paste(species, collapse = ", "),
    .groups = "drop"
  )

border_segments_sum <- border_segments_sum |>
  left_join(segment_species_lists, by = "seg_id")

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
  labs(title = "Shared (nationally) threatened mammals along HKH border segments")

plot(plot)

#---------------------------------------------#
# safe 
#---------------------------------------------#

sf::st_write(
  border_segments_sum,
  paste0(data_storage_path, "Output/transboundary/transb_TEST_threat.shp"),
  delete_layer = TRUE
)



# 
ggsave(
  filename = paste0(data_storage_path, "Output/transboundary/transb_nat_threat.png"),
  plot = plot,
  width = 14,
  height = 9,
  dpi = 300
)
