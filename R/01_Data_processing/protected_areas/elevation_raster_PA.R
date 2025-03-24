# Load required libraries
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(elevatr)
library(raster)

# 1. Get the elevational raster for the Himalaya
elev_data <- elevatr::get_elev_raster(mountain_range, z = 9, clip = "locations")
DEM <- terra::rast(elev_data)

#  visualization
x11()
plot(DEM)
lines(clean_PAs_himalaya, lwd = 2, alpha = .6)

# 2. Transform protected areas to match DEM CRS
wdpa_himalaya <- sf::st_transform(clean_PAs_himalaya, crs(DEM))

wdpa_himalaya_geom <-wdpa_himalaya$geometry

# 3. Rasterize protected areas
# Use touches=TRUE or cover=TRUE to capture small overlaps better
protected_raster <- terra::rasterize(
  vect(wdpa_himalaya_geom),
  DEM,
  field = 1,
  background = 0
)


plot(wdpa_himalaya$geometry)

# Crop and mask both rasters to the same extent
protected_raster_crop <- crop(protected_raster, mountain_range, snap = "in", mask = TRUE)
DEM_crop <- crop(DEM, mountain_range, snap = "in", mask = TRUE)

stacked_rasters <- c(protected_raster_crop, DEM_crop)
names(stacked_rasters) <- c("protected", "elevation")

# Convert to dataframe
df <- as.data.frame(stacked_rasters, xy = TRUE, na.rm = TRUE)


# 6. Filter out NA values
df <- df %>% filter(!is.na(elevation), !is.na(protected))

# 7. Create elevation bins (e.g., every 50 meters)
df <- df %>%
  mutate(elev_bin = cut(elevation, breaks = seq(0, 9000, by = 50), include.lowest = TRUE))

# 8. Calculate proportion of protected cells per elevation bin
prop_bins <- df %>%
  group_by(elev_bin) %>%
  summarise(
    total_cells = n(),
    protected_cells = sum(protected == 1),
    protected_area_prop = mean(protected == 1)
  )

# 9. Plot proportion of protected area across elevation bins
ggplot(prop_bins, aes(x = elev_bin, y = protected_area_prop)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(
    title = "Proportion of Protected Area across Elevational Gradient",
    x = "Elevation (m)",
    y = "Proportion Protected"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
