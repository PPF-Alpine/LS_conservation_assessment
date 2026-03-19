#---------------------------------------------#
# get border segments, countries and cons priority
#---------------------------------------------#

border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))

cons_prio <- rast(paste0(data_storage_path, "Output/priority_indices/priority_summary_threathend.tif"))

cons_prio_eq <- terra::project(cons_prio, crs(border_segments_tr_sp), method = "near")

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

# create 10km buffer around each segment

buffer_dist <- 10000

segment_buffers <- st_buffer(border_segments, dist = buffer_dist)


countries_hkh_eq <- countries_hkh |>
  st_transform(st_crs(border_segments))

#---------------------------------------------#
# get sides for each segment/ country
#---------------------------------------------#

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


plot(st_geometry(countries_hkh_eq), col = "grey90", border = "grey50")
plot(st_geometry(segment_sides[segment_sides$side == "A", ]), add = TRUE, col = scales::alpha("red", 0.4), border = NA)
plot(st_geometry(segment_sides[segment_sides$side == "B", ]), add = TRUE, col = scales::alpha("blue", 0.4), border = NA)
plot(st_geometry(border_segments), add = TRUE, col = NA, border = "black", lwd = 1)



segment_sides_vect <- terra::vect(segment_sides)


#---------------------------------------------#
# extract cons prio for each segment side
#---------------------------------------------#

cons_prio_eq <- terra::project(
  cons_prio,
  st_crs(border_segments)$wkt,
  method = "near"
)


prio_extract <- terra::extract(cons_prio_eq, segment_sides_vect)

prio_extract$seg_id <- segment_sides$seg_id[prio_extract$ID]
prio_extract$country <- segment_sides$country[prio_extract$ID]
prio_extract$side <- segment_sides$side[prio_extract$ID]

#---------------------------------------------#
# get summaries
#---------------------------------------------#

prio_side_summary <- prio_extract |>
  filter(!is.na(sum)) |>
  group_by(seg_id, country, side) |>
  summarise(
    mean_class   = mean(sum, na.rm = TRUE),
    median_class = median(sum, na.rm = TRUE),
    class_0 = sum(sum == 0, na.rm = TRUE),
    class_1 = sum(sum == 1, na.rm = TRUE),
    class_2 = sum(sum == 2, na.rm = TRUE),
    class_3 = sum(sum == 3, na.rm = TRUE),
    class_4 = sum(sum == 4, na.rm = TRUE),
    class_5 = sum(sum == 5, na.rm = TRUE),
    class_6 = sum(sum == 6, na.rm = TRUE),
    .groups = "drop"
  )

prio_segment_wide <- prio_side_summary |>
  pivot_wider(
    id_cols = seg_id,
    names_from = side,
    values_from = c(country, mean_class, median_class, class_0, class_1, class_2, class_3, class_4, class_5, class_6),
    names_sep = "_"
  ) |>
  dplyr::rename(
    ctryA = country_A,
    ctryB = country_B,
    meanA = mean_class_A,
    meanB = mean_class_B,
    medA  = median_class_A,
    medB  = median_class_B,
    c0A = class_0_A,
    c0B = class_0_B,
    c1A = class_1_A,
    c1B = class_1_B,
    c2A = class_2_A,
    c2B = class_2_B,
    c3A = class_3_A,
    c3B = class_3_B,
    c4A = class_4_A,
    c4B = class_4_B,
    c5A = class_5_A,
    c5B = class_5_B,
    c6A = class_6_A,
    c6B = class_6_B
  )


#---------------------------------------------#
# join and save
#---------------------------------------------#

border_segments_prio <- border_segments |>
  left_join(prio_segment_wide, by = "seg_id")


tolower(names(border_segments_prio))

sf::st_write(
  border_segments_prio,
  paste0(data_storage_path, "Output/transboundary/border_segments_consprio.gpkg"),
  delete_layer = TRUE
)
