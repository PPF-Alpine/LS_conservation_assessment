#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# Load configuration file
source(here::here("R/00_Config_file.R"))


# Load required libraries
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(elevatr)
library(raster)

format(99999999,scientific = FALSE)

#----------------------------------------------------------#
#     Load data 
#----------------------------------------------------------#
# PA for Himalaya
gpkg_file <- paste0(data_storage_path, "Datasets/protected_areas/Himalaya/clean_PAs_himalaya.gpkg")

# List available layers
st_layers(gpkg_file)

# Read a specific layer
clean_PAs_himalaya <- st_read(gpkg_file, layer = "clean_PAs_himalaya")


#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", 
                                     sep = "/"))
mountain_range <- mountain_shapes|>
  filter(MapName=="Himalaya")


# get the elevational raster for the Himalaya

#elev_data <- elevatr::get_elev_raster(mountain_range, z = 9, clip = "locations")
#DEM <- terra::rast(elev_data)

#writeRaster(DEM,filename = paste0(data_storage_path, "Datasets/protected_areas/Himalaya/DEM_raster.tif"),
            #overwrite = TRUE)

DEM <- rast(paste0(data_storage_path, "Datasets/protected_areas/Himalaya/DEM_raster.tif"))

#  visualization
x11()
plot(DEM)
lines(clean_PAs_himalaya, lwd = 2, alpha = .6)

#----------------------------------------------------------#
#     Rasterize shps
#----------------------------------------------------------#

# transform protected areas to match DEM CRS
wdpa_himalaya <- sf::st_transform(clean_PAs_himalaya, crs(DEM))

wdpa_himalaya_geom <-wdpa_himalaya$geometry

# rasterize protected areas
protected_raster <- rasterize(
  vect(wdpa_himalaya),
  DEM,
  field = 1,
  background = 0
)


plot(wdpa_himalaya$geometry)

# Crop and mask both rasters to the same extent
protected_raster_crop <- crop(protected_raster, mountain_range, snap = "in", mask = TRUE)
DEM_crop <- crop(DEM, mountain_range, snap = "in", mask = TRUE)

#----------------------------------------------------------#
#     Stack rasters together
#----------------------------------------------------------#

# Stack rasters together
r_stack <- c(DEM_crop, protected_raster_crop)
plot(r_stack)

# Convert to dataframe
df <- as.data.frame(r_stack, xy = TRUE, na.rm = FALSE)
names(df) <- c("x", "y", "elevation", "protected")

df_drop <- df|>
  drop_na(elevation, protected)|>
  filter(elevation>0)

#----------------------------------------------------------#
#     Calculate proportion of protection per elev bin
#----------------------------------------------------------#

# Define bin width
bin_width <- 200

# Get min/max elevation rounded
min_elev <- floor(min(df_drop$elevation, na.rm = TRUE))
max_elev <- ceiling(max(df_drop$elevation, na.rm = TRUE))

# Generate breaks
breaks <- seq(min_elev, max_elev, by = bin_width)
breaks <- c(breaks, Inf)  # catch any elevation above last bin

# Custom labels
labels <- paste0(
  format(breaks[-length(breaks)], scientific = FALSE, big.mark = ""),
  "–",
  format(breaks[-1], scientific = FALSE, big.mark = "")
)

# Manually fix the last label to be open-ended (e.g., ">8520")
labels[length(labels)] <- paste0(">", format(breaks[length(breaks) - 1], scientific = FALSE))


# Bin the elevation
df_drop <- df_drop %>%
  mutate(elev_bin = cut(elevation,
                        breaks = breaks,
                        labels = labels,
                        include.lowest = TRUE,
                        right = FALSE))

# Summarise by bin
bin_summary <- df_drop %>%
  group_by(elev_bin) %>%
  summarise(
    n_total = n(),
    n_1 = sum(protected == 1, na.rm = TRUE),
    proportion_1 = n_1 / n_total,
    .groups = "drop"
  )

# View result
print(bin_summary)


#----------------------------------------------------------#
#     Plot histogram of elev bins
#----------------------------------------------------------#
# 
library(ggplot2)

ggplot(bin_summary, aes(x = elev_bin, y = proportion_1 * 100)) +  # convert to %
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.6) +  # light, transparent green
  labs(
    x = "Elevation (m)",
    y = "protected area or OECM (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),  # remove all grid lines
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))




