library(here)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(magrittr)
# Load configuration

source(here::here("R/00_Config_file.R"))

# Load data
annual_temp <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio1_1981-2010_V.2.1.tif"))
annual_prec <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio12_1981-2010_V.2.1.tif"))

annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))
annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio12_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))

alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", sep = "/"))

# get unique mountains
mountain_selection <- alpine_shapes$Mntn_rn %>%
  unique() %>%
  sort() %>%
  gsub("/", "_", .)  # Only replace slashes with underscores



# Initialize an empty dataframe to collect % loss results
loss_results <- data.frame(Mountain = character(), Percent_Lost = numeric(), stringsAsFactors = FALSE)

# Directory to save plots
output_dir <- paste0(data_storage_path, "Outputs/mountain_climates/future_alpine_area/")

#----------------------------------------------------------#
#             LOOP over mountains
#----------------------------------------------------------#
for (mountain_name in mountain_selection) {
  
  cat("Processing: ", mountain_name, "\n")
  
  mountain_range <- alpine_shapes |> filter(Mntn_rn == mountain_name)
  
  # Skip if no shape available
  if (nrow(mountain_range) == 0) next
  
  # Crop/mask climate rasters
  temp_mountain <- mask(crop(annual_temp, mountain_range), mountain_range)
  prec_mountain <- mask(crop(annual_prec, mountain_range), mountain_range)
  
  temp_mountain_future <- mask(crop(annual_temp_ssp85, mountain_range), mountain_range)
  prec_mountain_future <- mask(crop(annual_prec_ssp85, mountain_range), mountain_range)
  
  # Stack temp and prec
  climate_stack <- c(temp_mountain, prec_mountain)
  names(climate_stack) <- c("temperature", "precipitation")
  
  climate_stack_future <- c(temp_mountain_future, prec_mountain_future)
  names(climate_stack_future) <- c("temperature", "precipitation")
  
  # Convert to dataframe for current climate
  climate_df <- as.data.frame(climate_stack, na.rm = TRUE)
  
  # Estimate 5-95% range (avoiding extremes)
  temp_range <- quantile(climate_df$temperature, probs = c(0.05, 0.95), na.rm = TRUE)
  prec_range <- quantile(climate_df$precipitation, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Current alpine climate area
  current_alpine_area <- (temp_mountain >= temp_range[1]) & (temp_mountain <= temp_range[2]) &
    (prec_mountain >= prec_range[1]) & (prec_mountain <= prec_range[2])
  
  # Future alpine climate area
  future_alpine_area <- (temp_mountain_future >= temp_range[1]) & (temp_mountain_future <= temp_range[2]) &
    (prec_mountain_future >= prec_range[1]) & (prec_mountain_future <= prec_range[2])
  
  # Calculate % lost
  current_pixels <- sum(values(current_alpine_area), na.rm = TRUE)
  future_pixels <- sum(values(future_alpine_area), na.rm = TRUE)
  
  percent_retained <- (future_pixels / current_pixels) * 100
  percent_lost <- 100 - percent_retained
  
  # Save result
  loss_results <- rbind(loss_results, data.frame(Mountain = mountain_name, Percent_Lost = percent_lost))
  
  # Create plot
  future_alpine_area_numeric <- classify(future_alpine_area, rcl = matrix(c(0, 1, 1, 2), ncol = 2, byrow = TRUE)) - 1
  future_alpine_df <- as.data.frame(future_alpine_area_numeric, xy = TRUE,na.rm = TRUE)
  
  # Label cells
  future_alpine_df$cells <- factor(future_alpine_df$`CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1`,
                                   levels = c(1, 0),
                                   labels = c("retained", "lost"))
  
  # Plot
  p <- ggplot(future_alpine_df, aes(x = x, y = y, fill = cells)) +
    geom_tile() +
    scale_fill_manual(values = c("retained" = "yellow", "lost" = "darkred")) +
    coord_equal() +
    theme_minimal() +
    labs(title = paste0(mountain_name, ": future alpine climate space (RCP8.5 2040-2070)"),
         fill = "Climate space",
         x = "Longitude", y = "Latitude") +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
  
  # Save plot
  ggsave(filename = paste0(output_dir, "/", gsub(" ", "_", mountain_name), "_climate_loss_map.jpeg"),
         plot = p, width = 8, height = 6)
}

#----------------------------------------------------------#
#  Create Barplot of Climate Loss
#----------------------------------------------------------#

# Sort by most loss
loss_results <- loss_results |> 
  arrange(desc(Percent_Lost))

# Plot
x11()
ggplot(loss_results, aes(x = reorder(Mountain, -Percent_Lost), y = Percent_Lost)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Percent alpine clim space lost",
       x = NULL,
       y = "Percent lost (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


#---------------------------------------------------------------#
# write to csv --
#----------------------------------------------------------------#

today <- format(Sys.Date(), "%Y%m%d")

# Define file path
output_path <- paste0(data_storage_path, "Outputs/mountain_climates/future_alpine_area/all_mountains_perc_loss_85_70_", today, ".csv")

write.csv(loss_results,output_path)
