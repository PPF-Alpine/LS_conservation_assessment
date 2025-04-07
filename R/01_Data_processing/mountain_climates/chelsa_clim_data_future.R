#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(terra)

# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#         load chelsa dataand mountain range
#----------------------------------------------------------#

annual_temp <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio1_1981-2010_V.2.1.tif"))

annual_prec <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio12_1981-2010_V.2.1.tif"))


# this is for ssp85 2070-2100
annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio1_2071-2100_mpi-esm1-2-hr_ssp585_V.2.1.tif"))

annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio12_2071-2100_mpi-esm1-2-hr_ssp585_V.2.1.tif"))

# this is for ssp85 2040-2070
annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))

annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio12_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))


#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", 
                                     sep = "/"))

mountain_range <- mountain_shapes|>
  filter(MapName=="Northern Andes")

# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                     sep = "/"))

head(alpine_shapes)

mountain_range <- alpine_shapes|>
  filter(Mntn_rn =="Ethiopian Highlands")

#----------------------------------------------------------#
#         crop rasters by mountain range
#----------------------------------------------------------#

# temp
temp_mountain_crop <- crop(annual_temp, mountain_range)

temp_mountain <- mask(temp_mountain_crop, mountain_range)


#prec
prec_mountain_crop <- crop(annual_prec, mountain_range)

prec_mountain <- mask(prec_mountain_crop, mountain_range)


# temp future
temp_mountain_crop_future <- crop(annual_temp_ssp85, mountain_range)

temp_mountain_future <- mask(temp_mountain_crop_future, mountain_range)
plot(temp_mountain_future)


#prec future
prec_mountain_crop_future <- crop(annual_prec_ssp85, mountain_range)

prec_mountain_future <- mask(prec_mountain_crop_future, mountain_range)


#----------------------------------------------------------------------#
#  calculate the shift of climatic space within a certain alpine area
#----------------------------------------------------------------------#

# Stack rasters and convert to data.frame
climate_stack <- c(temp_mountain, prec_mountain)
names(climate_stack) <- c("temperature", "precipitation")

climate_df <- as.data.frame(climate_stack, na.rm = TRUE)
#climate_df <- climate_df[sample(nrow(climate_df), min(10000, nrow(climate_df))), ]


# Stack future climate
climate_stack_future <- c(temp_mountain_future, prec_mountain_future)
names(climate_stack_future) <- c("temperature", "precipitation")

# Convert to df
climate_df_future <- as.data.frame(climate_stack_future, na.rm = TRUE)
#climate_df_future <- climate_df_future[sample(nrow(climate_df_future), min(10000, nrow(climate_df_future))), ]

# combine two dataets
climate_df$period <- "Present"
climate_df_future$period <- "Future"

climate_combined <- rbind(climate_df, climate_df_future)

centroids <- climate_combined |>
  group_by(period) |>
  summarise(
    mean_temp = mean(temperature, na.rm = TRUE),
    mean_prec = mean(precipitation, na.rm = TRUE)
  )
# calculate the shift for the variables 
arrow_df <- data.frame(
  x = centroids$mean_temp[centroids$period == "Present"],
  y = centroids$mean_prec[centroids$period == "Present"],
  xend = centroids$mean_temp[centroids$period == "Future"],
  yend = centroids$mean_prec[centroids$period == "Future"]
)

# this is the euclidean distance (the length of the error)
D <- sqrt((arrow_df$xend - arrow_df$x)^2 + (arrow_df$yend - arrow_df$y)^2)
print(D)

#----------------------------------------------------------------------#
#  Plot the climate space
#----------------------------------------------------------------------#

x11()
ggplot(climate_combined, aes(x = temperature, y = precipitation)) +
  stat_density_2d(aes(fill = period),
                  geom = "polygon",
                  contour = TRUE,
                  alpha = 0.4,
                  color = NA) +
  scale_fill_manual(values = c("Present" = "#1f78b4", "Future" = "#e31a1c")) +
  
  # Arrow showing shift
  geom_segment(data = arrow_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", size = 1.2) +
  
  # Centroid points
  geom_point(data = centroids, aes(x = mean_temp, y = mean_prec, color = period),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_color_manual(values = c("Present" = "#1f78b4", "Future" = "#e31a1c")) +
  
  labs(title = "Climate Space Shift — CEH",
       x = "Temperature (C)",
       y = "Precipitation (mm)",
       fill = "Period",
       color = "Centroid") +
  scale_x_continuous(limits = c(-22, 35)) +
  scale_y_continuous(limits = c(0, 7000)) +
  theme_minimal()
