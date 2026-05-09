library(dplyr)
library(ggplot2)
library(stringr)
#---------------------------------------------#
# read raster 
#---------------------------------------------#

aligned_files_transboundary <- rast(
  file.path(
    data_storage_path,
    "Datasets/transboundary/transb_species/GLOBAL/aligned_files_transboundary.tif"
  )
)

# plot species layers
plot(aligned_files_transboundary)

# Load DEM for HKH; used as both grid template and mask
dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))


template <- terra::project(dem_crop, "EPSG:4326")

# plot the richness of global transboundary species 
richness <- sum(aligned_files_transboundary, na.rm = TRUE)
richness <- terra::project(richness, "EPSG:4326")


# align richness to template 
richness_template <- terra::crop(richness, template)

richness_template <- terra::resample(
  richness_template,
  template,
  method = "near"
)


# Turn NA -> 0 only where the template has data, keep outside as NA
richness0 <- cover(richness_template, 0 * template)

# enforce the outside mask explicitly
richness_mask <- mask(richness0, template)

plot(richness_mask, colNA = "grey90")

names(richness_mask) <- "richness"

# convert raster to dataframe for ggplot
richness_df <- as.data.frame(richness_mask, xy = TRUE, na.rm = FALSE)

#---------------------------------------------#
# border segments 
#---------------------------------------------#
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))

shared_by_segment <- readxl::read_xlsx(
  file.path(
    data_storage_path,
    "Datasets/transboundary/transb_species/GLOBAL/shared_by_segment.xlsx"
  )
)
# 
border_segments <- st_transform(border_segments, crs(richness_mask))

# join number of shared species to segments
border_segments_plot <- border_segments |>
  left_join(shared_by_segment, by = "seg_id") |>
  mutate(
    n_shared_species = ifelse(is.na(n_shared_species), 0, n_shared_species)
  )

#---------------------------------------------#
# plot transboundary map 
#---------------------------------------------#
transbmap <- ggplot() +
  geom_raster(
    data = richness_df,
    aes(x = x, y = y, fill = richness),
    alpha = 0.8
  ) +
  geom_sf(
    data = border_segments_plot,
    aes(linewidth = n_shared_species),
    color = "black"
  ) +
  scale_linewidth(
    range = c(0.2, 3)
  ) +
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    begin = 0.15,
    end = 0.95,
    na.value = "white",
    name = NULL
  ) +
  guides(linewidth = "none") +
  coord_sf(crs = st_crs(border_segments_plot)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

plot(transbmap)

#---------------------------------------------#
# shared country 
#---------------------------------------------#
shared_by_country_pair_plot <- shared_by_country_pair |>
  mutate(
    n_seg_ids = str_count(seg_ids_text, ";") + 1,
    label = paste0("n seg. = ", n_seg_ids)
  )

countrypair <- ggplot(
  shared_by_country_pair_plot,
  aes(
    x = reorder(country_pair, n_shared_species),
    y = n_shared_species
  )
) +
  geom_col(fill = "#3B528B") +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 5
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of transboundary threatened species",
    title = NULL
  ) +
  expand_limits(
    y = max(shared_by_country_pair_plot$n_shared_species) * 1.15
  ) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 17),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 17),
    axis.text = element_text(size = 17),
    axis.title = element_text(size = 17)
  )

plot(countrypair)


#---------------------------------------------#
# save outputs 
#---------------------------------------------#
ggsave(
  filename = paste0(
    data_storage_path,
    "Output/transboundary/transbmap.jpeg"
  ),
  plot = transbmap,
  width = 14,
  height = 9,
  dpi = 300
)

ggsave(
  filename = paste0(
    data_storage_path,
    "Output/transboundary/countrypair.png"
  ),
  plot = countrypair,
  width = 10,
  height = 9,
  dpi = 300
)
