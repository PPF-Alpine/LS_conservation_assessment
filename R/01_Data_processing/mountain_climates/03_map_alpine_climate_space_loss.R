#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(rredlist)
library(stringr)
library(scales)
library(purrr)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)

# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#        read in the LUC summaries 
#----------------------------------------------------------#
#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", 
                                     sep = "/"))
# Load both datasets
climate_shifts <- read_csv(file.path(data_storage_path, "Outputs/mountain_climates/future_alpine_area/all_mountains_perc_loss_85_70_20250428.csv")) 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
#----------------------------------------------------------#
#     prep the data
#----------------------------------------------------------#

mountain_data <- mountain_shapes %>%
  left_join(climate_shifts, by = c("MapName" = "Mountain"))
#----------------------------------------------------------#
#   create the map
#----------------------------------------------------------#

map <- ggplot(mountain_data) +
  geom_sf(data = world, fill = "grey95", color = "grey95") +
  geom_sf(aes(fill = Percent_Lost, color = Percent_Lost), size = 0.1) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 100), na.value = "lightgrey",direction = -1) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 100), na.value = "lightgrey", guide = "none",direction = -1
                        ) +
  theme_void() +
  labs(fill = "alpine climate loss %")


x11()
plot(map)
#----------------------------------------------------------#
#   save the maps
#----------------------------------------------------------#
today <- format(Sys.Date(), "%Y%m%d")

# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/mountain_climates/alpine_climate_loss__ssp585_70_", today, ".jpeg")

# Save last plot as PNG
ggsave(filename = plot_path, plot=map,width = 10, height = 6, dpi = 300)


