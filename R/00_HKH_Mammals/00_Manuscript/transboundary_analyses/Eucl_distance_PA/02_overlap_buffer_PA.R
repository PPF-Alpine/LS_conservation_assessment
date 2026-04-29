library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(dplyr)
library(purrr)


source(here::here("R/00_Config_file_HKH.R"))
#---------------------------------------------#
# get country borders GADM for each country individually
#---------------------------------------------#
AFG <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_AFG_shp/gadm41_AFG_0.shp"))
# 1 obs 

IND_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_IND_shp/gadm41_IND_0_clean.shp"))|>
  dplyr::filter(GID_0 == "IND")
# 1 obs

IND_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_IND_shp/gadm41_IND_0_clean.shp"))|>
  dplyr::filter(GID_0 != "IND")
# 5 obs

NPL <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_NPL_shp/gadm41_NPL_0.shp"))
# 1 obs 

PAK_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/gadm41_PAK_0_clean.shp"))|>
  dplyr::filter(GID_0 == "PAK")
# 1 obs 

PAK_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/gadm41_PAK_0_clean.shp"))|>
  dplyr::filter(GID_0 != "PAK")
# 1 obs

BTN <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_BTN_shp/gadm41_BTN_0.shp"))

# GADM
CHN_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/cleaned_CHN.shp"))|> # use this one here bc of PA 
dplyr::filter(GID_0 == "CHN")

CHN_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/cleaned_CHN.shp"))|>
  dplyr::filter(GID_0 != "CHN")

MMR <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_MMR_shp/gadm41_MMR_0.shp"))

BGD <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_BGD_shp/gadm41_BGD_0.shp"))


plot(CHN_country$geometry)
#---------------------------------------------#
# 1. combine and clean names
#---------------------------------------------#

countries_all <- bind_rows(
  AFG, 
  NPL, 
  PAK_country, 
  PAK_ndlsa,
  IND_country, 
  IND_ndlsa,
  CHN_country, 
  CHN_ndlsa,
  MMR, 
  BGD, BTN
)|>
  st_make_valid()

#---------------------------------------------#
# 1. Read and prepare data
#---------------------------------------------#

pa_path <- paste0(data_storage_path, "Datasets/protected_areas/PA_HKH/pa_in_hkh.gpkg")

pa <- st_read(pa_path) |>
  st_make_valid() |>
  st_transform(8857) |>
  mutate(pa_id = WDPAID)


# get the ndlsa names 
ndlsa_lookup <- readxl::read_xlsx(paste0(data_storage_path, "Datasets/transboundary/GADM/ndlsa_clean.xlsx"))

#---------------------------------------------#
#  load buffered borders
#---------------------------------------------#


buffer_folder <- paste0(
  data_storage_path,
  "Datasets/transboundary/GADM/pairwise_border_buffers/"
)

buffer_files <- list.files(
  buffer_folder,
  pattern = "\\.gpkg$",
  full.names = TRUE
)

border_buffer_sides <- buffer_files |>
  map(st_read, quiet = TRUE) |>
  bind_rows() |>
  st_make_valid() |>
  st_transform(st_crs(pa)) |>
  mutate(
    buffer_side_id = row_number(),
    buffer_area_m2 = as.numeric(st_area(geom)),
    buffer_area_km2 = buffer_area_m2 / 1e6
  )


length(buffer_files)
nrow(border_buffer_sides)
st_crs(border_buffer_sides)
plot(border_buffer_sides$geom)

#---------------------------------------------#
# 2. Intersect protected areas with buffers
#---------------------------------------------#
# overlap first with countries because of edges 
# assign each PA to a country

## IMPORTANT ! HERE I NEED To use the different CHINA shapefile (otherwise PAs are filtered out)
pa_country <- st_intersection(
  pa,
  countries_all |> 
    st_transform(st_crs(pa)) |> 
    select(GID_0)
) |>
  mutate(country_overlap_m2 = as.numeric(st_area(geom))) |>
  st_drop_geometry() |>
  group_by(WDPAID) |>
  # there are some overlaps because of PA inaccuracies, wanna keep only the country where PA has largest overlap
  slice_max(country_overlap_m2, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(WDPAID, pa_country = GID_0)


pa2 <- pa |>
  left_join(pa_country, by = "WDPAID")


# clip PA to buffer area
pa_border <- st_intersection(pa2, border_buffer_sides) |>
  filter(pa_country == border_side) |>
  mutate(
    pa_overlap_m2 = as.numeric(st_area(geom)),
    pa_overlap_km2 = pa_overlap_m2 / 1e6,
    pa_buffer_cover_pct = 100 * pa_overlap_m2 / buffer_area_m2
  )
#---------------------------------------------#
# 3a. Overall PA coverage per border-buffer side
#---------------------------------------------#

# summarise total PA coverage per buffer side
overall_pa_coverage <- pa_border |>
  st_drop_geometry() |>
  group_by(
    buffer_side_id,
    pair_id,
    country_1,
    country_2,
    border_side,
    buffer_m,
    buffer_area_m2,
    buffer_area_km2
  ) |>
  summarise(
    pa_area_m2 = sum(pa_overlap_m2, na.rm = TRUE),
    pa_area_km2 = sum(pa_overlap_km2, na.rm = TRUE),
    n_pa = n_distinct(pa_id),
    .groups = "drop"
  ) |>
  mutate(
    pa_cover_pct = 100 * pa_area_m2 / buffer_area_m2
  )

#---------------------------------------------#
# 3b. Individual PA coverage per buffer
#---------------------------------------------#

individual_pa_coverage <- pa_border |>
  st_drop_geometry() |>
  select(
    buffer_side_id,
    pair_id,
    country_1,
    country_2,
    border_side,
    buffer_m,
    pa_id,
    WDPAID,
    buffer_area_m2,
    buffer_area_km2,
    pa_overlap_m2,
    pa_overlap_km2,
    pa_buffer_cover_pct
  )

#---------------------------------------------#
# 4. Euclidean distance: internal
#---------------------------------------------#

# calculate distance among PAs that are insdide same border pair
# on same side of border
# inside buffer 

pa_candidates_internal <- pa2 |>
  st_transform(st_crs(pa_border)) |>
  filter(!is.na(pa_country))

nearest_internal <- pa_border |>
  group_by(pair_id, border_side) |>
  group_modify(\(x, y) {
    
    x <- st_as_sf(x)
    
    focal_country <- y$border_side[[1]]
    
    candidates <- pa_candidates_internal |>
      filter(pa_country == focal_country)
    
    if (nrow(candidates) < 2) {
      x$nearest_internal_m <- NA_real_
      x$nearest_internal_WDPAID <- NA_real_
      return(x)
    }
    
    dmat <- st_distance(x, candidates)
    
    # Prevent each PA from selecting itself
    for (i in seq_len(nrow(x))) {
      self_idx <- which(candidates$WDPAID == x$WDPAID[i])
      if (length(self_idx) > 0) {
        dmat[i, self_idx] <- NA
      }
    }
    
    nearest_idx <- apply(dmat, 1, \(z) {
      if (all(is.na(z))) NA_integer_ else which.min(z)
    })
    
    x$nearest_internal_m <- sapply(seq_along(nearest_idx), \(i) {
      if (is.na(nearest_idx[i])) NA_real_ else as.numeric(dmat[i, nearest_idx[i]])
    })
    
    x$nearest_internal_WDPAID <- sapply(nearest_idx, \(i) {
      if (is.na(i)) NA_real_ else candidates$WDPAID[i]
    })
    
    x
  }) |>
  ungroup()

#---------------------------------------------#
# 5. Euclidean distance: across border
#---------------------------------------------#
# nearest PA 
nearest_across <- nearest_internal |>
  group_by(pair_id) |>
  group_modify(\(x, ...) {
    
    x <- st_as_sf(x)
    
    sides <- unique(x$border_side)
    
    x$nearest_across_m <- NA_real_
    x$nearest_across_WDPAID <- NA_real_
    x$nearest_across_side <- NA_character_
    
    if (length(sides) < 2) return(x)
    
    for (s in sides) {
      
      this_side  <- x$border_side == s
      other_side <- x$border_side != s
      
      nearest_idx <- st_nearest_feature(
        x[this_side, ],
        x[other_side, ]
      )
      
      x$nearest_across_m[this_side] <- as.numeric(
        st_distance(
          x[this_side, ],
          x[other_side, ][nearest_idx, ],
          by_element = TRUE
        )
      )
      
      x$nearest_across_WDPAID[this_side] <- x$WDPAID[other_side][nearest_idx]
      x$nearest_across_side[this_side] <- x$border_side[other_side][nearest_idx]
    }
    
    x
  }) |>
  ungroup() |>
  mutate(
    nearest_internal_km = nearest_internal_m / 1000,
    nearest_across_km = nearest_across_m / 1000
  )


#---------------------------------------------#
# 6. Final PA-level summary
#---------------------------------------------#
pa_distance_coverage_summary <- nearest_across |>
  st_drop_geometry() |>
  select(
    pair_id, country_1, country_2, border_side,
    buffer_side_id, buffer_m, pa_id, WDPAID,
    buffer_area_km2, pa_overlap_km2, pa_buffer_cover_pct,
    nearest_internal_WDPAID,
    nearest_internal_km,
    nearest_across_side,
    nearest_across_WDPAID,
    nearest_across_km
  )

pa_distance_coverage_geom <- nearest_across |>
  select(
    pair_id, country_1, country_2, border_side,
    buffer_side_id, buffer_m, pa_id, WDPAID,
    buffer_area_km2, pa_overlap_km2, pa_buffer_cover_pct,
    nearest_internal_WDPAID,
    nearest_internal_km,
    nearest_across_side,
    nearest_across_WDPAID,
    nearest_across_km,geom
  )



# join the missing country pairs (those with no PAs to df)

pa_distance_coverage_geomtest <- border_buffer_sides |>
  select(
    pair_id, country_1, country_2,
    border_length_m, border_length_km,
    buffer_m, GID_0, COUNTRY,
    border_side, buffer_side_id,
    buffer_area_m2, buffer_area_km2,
    geom
  ) |>
  left_join(
    pa_distance_coverage_geom |>
      st_drop_geometry() |>
      select(
        pair_id, country_1, country_2, border_side,
        buffer_side_id, buffer_m,
        pa_id, WDPAID,
        pa_overlap_km2, pa_buffer_cover_pct,
        nearest_internal_WDPAID,
        nearest_internal_km,
        nearest_across_side,
        nearest_across_WDPAID,
        nearest_across_km
      ),
    by = c(
      "pair_id",
      "country_1",
      "country_2",
      "border_side",
      "buffer_side_id",
      "buffer_m"
    )
  ) |>
  mutate(
    has_pa = !is.na(WDPAID),
    n_pa = if_else(has_pa, 1L, 0L)
  )



sf::st_write(pa_distance_coverage_geom, paste0(data_storage_path, "Datasets/transboundary/GADM/pairwise_border_buffers/distances/pa_distance_coverage.gpkg"),append=FALSE)

#---------------------------------------------#
# 7. Plot nearest distances by country side
#---------------------------------------------#

# to only run the plots : 
pa_distance_coverage_summary <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/pairwise_border_buffers/distances/pa_distance_coverage.gpkg"))

# internal: one point per PA on its own side
internal_points <- pa_distance_coverage_summary |>
  distinct(pair_id, border_side, WDPAID, .keep_all = TRUE) |>
  transmute(
    pair_id,
    border_side,
    comparison = "Internal",
    point_id = WDPAID,
    distance_km = nearest_internal_km
  )

# across: one point per unique nearest across-border PA
across_points <- pa_distance_coverage_summary |>
  filter(!is.na(nearest_across_WDPAID)) |>
  group_by(pair_id, border_side, nearest_across_WDPAID) |>
  summarise(
    distance_km = min(nearest_across_km, na.rm = TRUE),
    .groups = "drop"
  ) |>
  transmute(
    pair_id,
    border_side,
    comparison = "Across border",
    point_id = nearest_across_WDPAID,
    distance_km = distance_km
  )

#---------------------------------------------#
# clean points df
#---------------------------------------------#

points_df <- bind_rows(internal_points, across_points) |>
  filter(!is.na(distance_km))


# named lookup vector: Z01 = "Jammu and Kashmir", etc.
ndlsa_lookup <- setNames(ndlsa_lookup$ndlsa_name, ndlsa_lookup$GID_0)

points_df <- points_df |>
  mutate(
    border_side = recode(border_side, !!!ndlsa_lookup)
  )


summary_df <- points_df |>
  group_by(border_side, comparison) |>
  summarise(
    mean_km = mean(distance_km),
    se_km = sd(distance_km) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

#---------------------------------------------#
# plot nearest distance
#---------------------------------------------#
plot <- ggplot() +
  geom_jitter(
    data = points_df,
    aes(x = border_side, y = distance_km, color = comparison),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.5),
    size = 1.6,
    alpha = 0.5
  ) +
  geom_point(
    data = summary_df,
    aes(x = border_side, y = mean_km, color = comparison),
    position = position_dodge(width = 0.5),
    size = 3
  ) +
  geom_errorbar(
    data = summary_df,
    aes(
      x = border_side,
      ymin = mean_km - se_km,
      ymax = mean_km + se_km,
      color = comparison
    ),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  labs(
    x = NULL,
    y = "Nearest neighbor distance (km)",
    color = "Comparison"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

plot(plot)

ggsave(
  filename = paste0(data_storage_path, "Output/transboundary/euclidean_dist_plot.png"),
  plot = plot,
  width = 14,
  height = 9,
  dpi = 300
)

# ration of distance internal and across 
ratio_df <- summary_df |>
  select(border_side, comparison, mean_km) |>
  pivot_wider(
    names_from = comparison,
    values_from = mean_km
  )

ratio_df <- ratio_df |>
  mutate(
    ratio = `Across border` / Internal,
    barrier_index = (`Across border` - Internal) / Internal
  )

ggplot(ratio_df, aes(x = border_side, y = ratio)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = "Border side",
    y = "Distance ratio (Across / Internal)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
