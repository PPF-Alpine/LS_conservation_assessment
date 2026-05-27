biodiv_imp <- rast(file.path(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))
clim_velocity_temp <- terra::rast(file.path(data_storage_path, "Datasets/climate_shift/clim_velocity/clim_velocity_temp_capped.tif"))
hfi <- rast(file.path(data_storage_path, "Datasets/hfi/hfi_hkh.tif"))

#---------------------------------------------#
# bring to ssame extent and resolution 
#---------------------------------------------#
template <- clim_velocity_temp

# crop biodiversity to climate extent/grid
biodiv_rs <- crop(biodiv_imp, template)
biodiv_rs <- resample(biodiv_rs, template, method = "bilinear")

# resample HFI down to climate grid
hfi_rs <- crop(hfi, template)
hfi_rs <- resample(hfi_rs, template, method = "bilinear")


template[!is.na(template)] <- 1

# fill HFI NA with 0
hfi_rs[is.na(hfi_rs)] <- 0

# remove values outside the template/HKH area
hfi_rs <- mask(hfi_rs, template)

#============================================================#
# Load border segments and create buffers
#============================================================#

# border segments and create buffer around each segment
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))
# 1. choose projected CRS in metres
border_segments_m <- st_transform(border_segments, 8857)

# 2. buffer in metres
segment_buffers <- border_segments_m |>
  st_buffer(dist = 100000) |>
  st_make_valid()

# 3. transform finished buffers to raster CRS
segment_buffers <- st_transform(segment_buffers, crs(layers)) |>
  mutate(ID = row_number())

segment_buffers_v <- terra::vect(segment_buffers)

#============================================================#
# Extract biodiversity values within buffer areas
#============================================================#

layers <- c(
  biodiv_rs,
  hfi_rs,
  clim_velocity_temp
)

names(layers) <- c(
  "biodiversity",
  "hfi",
  "clim_velocity"
)

# make sure buffers have an ID
segment_buffers <- segment_buffers |>
  mutate(ID = row_number())


# extract mean values per buffer
vals <- terra::extract(
  layers,
  segment_buffers_v
)


# calculations 
biodiv_buffer_median <- vals |>
  group_by(ID) |>
  summarise(
    biodiversity = quantile(
      biodiversity,
      probs = 0.9,
      na.rm = TRUE
    ),
    
    hfi = quantile(
      hfi,
      probs = 0.9,
      na.rm = TRUE
    ),
    
    clim_velocity = quantile(
      clim_velocity,
      probs = 0.9,
      na.rm = TRUE
    ),
    
    n = sum(!is.na(biodiversity))
  )


plot(layers[[1]])
plot(st_geometry(segment_buffers), add = TRUE, border = "red")
#============================================================#
# Extract biodiversity values within buffer areas
#============================================================#
# join extracted values back to buffer polygons
segment_buffers_median <- segment_buffers |>
  left_join(biodiv_buffer_median, by = "ID")

border_segments_median <- border_segments |>
  mutate(ID = row_number()) |>
  left_join(
    st_drop_geometry(segment_buffers_median),
    by = "ID"
  ) |>
  filter(!is.na(biodiversity))

#----------------------------------------------------------#
# classify extracted medians
#----------------------------------------------------------#

classify_vector_3 <- function(x) {
  
  qs <- quantile(
    x,
    probs = c(1/3, 2/3),
    na.rm = TRUE
  )
  
  case_when(
    x <= qs[1] ~ 1,
    x <= qs[2] ~ 2,
    x > qs[2]  ~ 3
  )
}

border_segments_median <- border_segments_median |>
  mutate(
    
    biodiv_class = classify_vector_3(biodiversity),
    climate_class = classify_vector_3(clim_velocity),
    hfi_class = classify_vector_3(hfi),
    
    # create bivariate combinations
    bio_clim = paste0(biodiv_class, climate_class),
    bio_hfi  = paste0(biodiv_class, hfi_class),
    clim_hfi = paste0(climate_class, hfi_class)
  )


bivar_cols <- c(
  "11" = "#e8e8e8",
  "12" = "#b8d6be",
  "13" = "#73ae80",
  "21" = "#dfb0d6",
  "22" = "#a5add3",
  "23" = "#5698b9",
  "31" = "#be64ac",
  "32" = "#8c62aa",
  "33" = "#3b4994"
)


p_bio_clim_segments <- ggplot() +
  
  geom_sf(
    data = border_segments_median,
    aes(color = bio_clim),
    linewidth = 1.5
  ) +
  
  scale_color_manual(
    values = bivar_cols,
    guide = "none"
  ) +
  
  theme_void()

plot(p_bio_clim_segments)

p_clim_hfi_segments <- ggplot() +
  
  geom_sf(
    data = border_segments_median,
    aes(color = clim_hfi),
    linewidth = 1.5
  ) +
  
  scale_color_manual(
    values = bivar_cols,
    guide = "none"
  ) +
  
  theme_void()

plot(p_clim_hfi_segments)

p_bio_hfi_segments <- ggplot() +
  
  geom_sf(
    data = border_segments_median,
    aes(color = bio_hfi),
    linewidth = 1.5
  ) +
  
  scale_color_manual(
    values = bivar_cols,
    guide = "none"
  ) +
  
  theme_void()

plot(p_bio_hfi_segments)



combined <- p_clim_hfi_segments /
  (p_bio_clim_segments | p_bio_hfi_segments) +
  plot_layout(heights = c(1.3, 1))

#----------------------------------------------------------#
# save plots 
#----------------------------------------------------------#
ggsave(
  filename = file.path(data_storage_path, "Output/pa/bivariat_segments.png"),
  plot = combined,
  width = 14,
  height = 9,
  dpi = 300
)
