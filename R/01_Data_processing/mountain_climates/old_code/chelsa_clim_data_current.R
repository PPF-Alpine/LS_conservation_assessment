#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(terra)

# Load configuration file
source(here::here("R/00_Config_file.R"))

install.packages("chelsa")

#----------------------------------------------------------#
#         load chelsa dataand mountain range
#----------------------------------------------------------#

annual_temp <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio1_1981-2010_V.2.1.tif"))

annual_prec <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio12_1981-2010_V.2.1.tif"))

DEM <- rast(paste0(data_storage_path,"Datasets/Mountains/DEM/GMTED2010_30.tiff"))


#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", 
                                     sep = "/"))

mountain_range <- mountain_shapes|>
  filter(MapName=="Himalaya")

#----------------------------------------------------------#
#         crop rasters by mountain range
#----------------------------------------------------------#

# temp
temp_mountain_crop <- crop(annual_temp, mountain_range)

temp_mountain <- mask(temp_mountain_crop, mountain_range)
plot(temp_mountain)

#prec
prec_mountain_crop <- crop(annual_prec, mountain_range)

prec_mountain <- mask(prec_mountain_crop, mountain_range)
plot(prec_mountain)

#DEM
dem_mountain_crop <- crop(DEM, mountain_range)

dem_mountain <- mask(dem_mountain_crop, mountain_range)
plot(dem_mountain)

#----------------------------------------------------------#
#         2 dimensional space 
#----------------------------------------------------------#

# Stack rasters and convert to data.frame
climate_stack <- c(temp_mountain, prec_mountain)
names(climate_stack) <- c("temperature", "precipitation")

climate_df <- as.data.frame(climate_stack, na.rm = TRUE)

# Optional: thin the data if too large
climate_df <- climate_df[sample(nrow(climate_df), min(10000, nrow(climate_df))), ]

# Create base 2D density plot
p <- ggplot(climate_df, aes(x = temperature, y = precipitation)) +
  geom_point(alpha = 0.1, size = 0.5, color = "black") +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", color = NA, alpha = 0.6) +
  scale_fill_viridis_c() +
  labs(title="clim space Himalaya",
    x = "Temperature (C)",
    y = "Precipitation (mm)",
    fill = "Density"
  ) +
  scale_x_continuous(limits = c(-22, 35)) +
  scale_y_continuous(limits = c(0, 7000)) +
  theme_minimal()

# Add marginal distributions (optional)
ggExtra::ggMarginal(p, type = "density", fill = "gray", alpha = 0.5)

#----------------------------------------------------------#
#         future plot: incldude the DEM ... 
#----------------------------------------------------------#

# rayshadder
# contour lines.. 
