

source(here::here("R/00_Config_file_HKH.R"))
library(terra)
library(sf)
#---------------------------------------------#
# Input data
#---------------------------------------------#

##============================================================#
# Load input rasters
#============================================================#

# load biodiv imp, climate velocity and human footprint 
biodiv_imp <- rast(file.path(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))
clim_velocity_temp <- terra::rast(file.path(data_storage_path, "Datasets/climate_shift/clim_velocity/clim_velocity_temp_capped.tif"))
hfi <- rast(file.path(data_storage_path, "Datasets/hfi/hfi_hkh.tif"))


#============================================================#
# Load border segments and create buffers
#============================================================#

# border segments and create buffer around each segment
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))
plot(border_segments$geom)

# create 10km or 100 km ? buffer around each segment
buffer_dist <- 100000 

segment_buffers <- st_buffer(border_segments, dist = buffer_dist)

plot(segment_buffers)


#============================================================#
# Bring rasters to same extent and resolution
#============================================================#

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

plot(hfi_rs)

#============================================================#
# Scale rasters to 0–1
#============================================================#

scale01 <- function(x) {
  (x - global(x, "min", na.rm = TRUE)[1,1]) /
    (global(x, "max", na.rm = TRUE)[1,1] -
       global(x, "min", na.rm = TRUE)[1,1])
}

biodiv_01 <- scale01(biodiv_rs)
clim_01   <- scale01(clim_velocity_temp)
hfi_01    <- scale01(hfi_rs)


#============================================================#
# Summarise border segments
#============================================================#

# Convert border segments to terra vector
border_segments_v <- terra::vect(segment_buffers)

# Make sure CRS matches raster CRS
border_segments_v <- terra::project(border_segments_v, terra::crs(biodiv_imp))


#------------------------------------------------------------#
# Extract mean and 90th percentile values per segment
#------------------------------------------------------------#

# extract biodiversity values in each border segment
biodiv_mean <- terra::extract(
  biodiv_01,
  border_segments_v,
  fun = mean,
  na.rm = TRUE
) |>
  dplyr::rename(biodiv_mean = GMTED2010_30)

biodiv_q90 <- terra::extract(
  biodiv_01,
  border_segments_v,
  fun = function(x, na.rm=TRUE)
    quantile(x, 0.9, na.rm = na.rm),
  na.rm = TRUE
)|>
  dplyr::rename(biodiv_q90 = GMTED2010_30)

clim_mean <- terra::extract(
  clim_01,
  border_segments_v,
  fun = mean,
  na.rm = TRUE
)|>
  dplyr::rename(clim_mean = bio01)

clim_q90 <- terra::extract(
  clim_01,
  border_segments_v,
  fun = function(x, na.rm=TRUE)
    quantile(x, 0.9, na.rm = na.rm),
  na.rm = TRUE
)|>
  dplyr::rename(clim_q90 = bio01)

hfi_mean <- terra::extract(
  hfi_01,
  border_segments_v,
  fun = mean,
  na.rm = TRUE
)|>
  dplyr::rename(hfi_mean = "hii_2020-01-01")

hfi_q90 <- terra::extract(
  hfi_01,
  border_segments_v,
  fun = function(x, na.rm=TRUE)
    quantile(x, 0.9, na.rm = na.rm),
  na.rm = TRUE
)|>
  dplyr::rename(hfi_q90 = "hii_2020-01-01")


#------------------------------------------------------------#
# Join extracted statistics back to border segments
#------------------------------------------------------------#

border_segment_stats <- border_segments |>
  mutate(ID = row_number()) |>
  left_join(biodiv_mean, by = "ID") |>
  left_join(biodiv_q90, by = "ID") |>
  left_join(clim_mean, by = "ID")|>
  left_join(clim_q90, by = "ID")|>
  left_join(hfi_mean, by = "ID")|>
  left_join(hfi_q90, by = "ID")


#============================================================#
# Plot segment-level 90th percentile values
#============================================================#

plot_df <- border_segment_stats %>%
  st_drop_geometry() %>%
  select(
    seg_id,
    pair_id,
    biodiv_mean,
    clim_mean,
    hfi_mean,
    biodiv_q90,clim_q90,hfi_q90
  ) %>%
  pivot_longer(
    cols = c(biodiv_q90, clim_q90, hfi_q90),
    names_to = "variable",
    values_to = "value"
  )

ggplot(plot_df,
       aes(x = reorder(seg_id, value),
           y = value,
           color = variable,
           group = variable)) +
  geom_point(size = 1.8) +
  facet_wrap(~pair_id, scales = "free_x") +
  labs(
    x = "Border segment",
    y = "Scaled value",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank()
  )


#============================================================#
# Extract all pixels per border segment
#============================================================#

biodiv_pix <- terra::extract(biodiv_01, border_segments_v) |>
  rename(value = GMTED2010_30) |>
  mutate(variable = "Biodiversity importance")

clim_pix <- terra::extract(clim_01, border_segments_v) |>
  rename(value = bio01) |>
  mutate(variable = "Climate velocity")

hfi_pix <- terra::extract(hfi_01, border_segments_v) |>
  rename(value = `hii_2020-01-01`) |>
  mutate(variable = "Human footprint")

seg_meta <- border_segments |>
  st_drop_geometry() |>
  mutate(ID = row_number()) |>
  select(ID, pair_id, seg_id, len_km)

pixel_df <- bind_rows(biodiv_pix, clim_pix, hfi_pix) |>
  left_join(seg_meta, by = "ID") |>
  filter(!is.na(value))


#============================================================#
# Plot all segments for country pairs
#============================================================#

#------------------------------------------------------------#
# Settings
#------------------------------------------------------------#

out_dir <- paste0(data_storage_path, "Output/transboundary/")

var_cols <- c(
  "Biodiversity importance" = "#FF0000",  # bright red
  "Climate velocity"        = "#00CC00",  # bright green
  "Human footprint"         = "#0000FF"   # bright blue
)

base_violin_theme <- theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.2, "lines"),
    strip.text = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 14)
  )


#------------------------------------------------------------#
# Plot function: country-pair violin plots
#------------------------------------------------------------#

make_pair_violin_plot <- function(df) {
  
  ggplot(df,
         aes(y = variable,
             x = value,
             fill = variable)) +
    
    geom_violin(
      scale = "width",
      trim = TRUE,
      alpha = 0.8,
      color = NA
    ) +
    
    geom_boxplot(
      width = 0.12,
      outlier.shape = NA,
      alpha = 0.7
    ) +
    
    facet_wrap(~pair_id, ncol = 1) +
    
    scale_fill_manual(values = var_cols) +
    
    labs(
      x = "Scaled pixel value",
      y = NULL
    ) +
    
    base_violin_theme
}


#------------------------------------------------------------#
# Split country pairs into two plots
#------------------------------------------------------------#

pair_levels <- unique(pixel_df$pair_id)

pair_set1 <- pair_levels[1:7]
pair_set2 <- pair_levels[8:13]

pixel_df_1 <- pixel_df |>
  filter(pair_id %in% pair_set1)

pixel_df_2 <- pixel_df |>
  filter(pair_id %in% pair_set2)


#------------------------------------------------------------#
# Plot split figures
#------------------------------------------------------------#

prio10_1 <- make_pair_violin_plot(pixel_df_1)
prio10_2 <- make_pair_violin_plot(pixel_df_2)

ggsave(
  filename = paste0(out_dir, "prio10_1.png"),
  plot = prio10_1,
  width = 9,
  height = 18,
  dpi = 300
)

ggsave(
  filename = paste0(out_dir, "prio10_2.png"),
  plot = prio10_2,
  width = 9,
  height = 18,
  dpi = 300
)

plot(prio10_2)
plot(prio10_1)


#============================================================#
# Plot individual segments for one country pair
#============================================================#

pixel_segid <- pixel_df |>
  mutate(segment_id = paste0(pair_id, "_seg", seg_id))

pair_name <- "Nepal_India"

pixel_df_one_pair <- pixel_segid |>
  filter(pair_id == pair_name) |>
  mutate(segment_id = paste0("seg ", seg_id))

seg_plot <- ggplot(pixel_df_one_pair,
                   aes(y = variable,
                       x = value,
                       fill = variable)) +
  
  geom_violin(
    scale = "width",
    trim = TRUE,
    alpha = 0.8,
    color = NA
  ) +
  
  stat_summary(
    fun = median,
    geom = "point",
    size = 1.5,
    color = "black"
  ) +
  
  facet_wrap(~segment_id, ncol = 3) +
  
  scale_fill_manual(values = var_cols) +
  
  labs(
    x = "Scaled pixel value",
    y = NULL
  ) +
  
  base_violin_theme +
  theme(
    strip.text = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 16)
  )

seg_plot

ggsave(
  filename = paste0(out_dir, pair_name, ".png"),
  plot = seg_plot,
  width = 14,
  height = 20,
  dpi = 300
)

###

global(clim_velocity_temp, c("min", "max", "mean", "sd"), na.rm=TRUE)
global(biodiv_imp, c("min", "max", "mean", "sd"), na.rm=TRUE)
global(hfi, c("min", "max", "mean", "sd"), na.rm=TRUE)
global(clim_velocity_temp,
       quantile,
       probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
       na.rm = TRUE)
