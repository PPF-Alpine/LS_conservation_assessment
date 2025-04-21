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
luc_summary_ssp5_85 <- read_csv(file.path(data_storage_path, "Outputs/land_use_change/all_mountains_luc_summary_ssp5_85_20250421.csv")) |>
  mutate(scenario = "SSP5-8.5_2100")

luc_summary_ssp1_26 <- read_csv(file.path(data_storage_path, "Outputs/land_use_change/all_mountains_luc_summary_ssp1_26_20250421.csv")) |>
  mutate(scenario = "SSP5-8.5_2100")

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
#----------------------------------------------------------#
#     prep the data
#----------------------------------------------------------#

map_data_85 <- luc_summary_ssp5_85|>
  select(Mntn_rn,sum_luc)|>
  distinct()

map_data_26 <- luc_summary_ssp1_26|>
  select(Mntn_rn,sum_luc)|>
  distinct()

mountain_data <- mountain_shapes %>%
  left_join(map_data_85, by = c("MapName" = "Mntn_rn"))

mountain_data_26 <- mountain_shapes %>%
  left_join(map_data_26, by = c("MapName" = "Mntn_rn"))
#----------------------------------------------------------#
#   create the map
#----------------------------------------------------------#

luc_map <- ggplot(mountain_data) +
  geom_sf(data = world, fill = "grey95", color = "grey95") +
  geom_sf(aes(fill = sum_luc, color = sum_luc), size = 0.1) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, 1), na.value = "lightgrey") +
  scale_color_viridis_c(option = "plasma", limits = c(0, 1), na.value = "lightgrey", guide = "none") +
  theme_void() +
  labs(fill = "Land Use Sum")

luc_map_26 <- ggplot(mountain_data_26) +
  geom_sf(data = world, fill = "grey95", color = "grey95") +
  geom_sf(aes(fill = sum_luc, color = sum_luc), size = 0.1) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, 1), na.value = "lightgrey") +
  scale_color_viridis_c(option = "plasma", limits = c(0, 1), na.value = "lightgrey", guide = "none") +
  theme_void() +
  labs(fill = "Land Use Sum")

x11()
plot(luc_map_26)
#----------------------------------------------------------#
#   save the maps
#----------------------------------------------------------#
today <- format(Sys.Date(), "%Y%m%d")

# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/land_use_change/all_mountains_luc_ssp5_85_", today, ".png")

# Save last plot as PNG
ggsave(filename = plot_path, plot=luc_map_85,width = 10, height = 6, dpi = 300)


# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/land_use_change/luc_ssp1_26_", today, ".png")

# Save last plot as PNG
ggsave(filename = plot_path, plot=luc_map_26,width = 10, height = 6, dpi = 300)
