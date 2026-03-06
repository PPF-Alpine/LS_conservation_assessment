


# all artiodactyla are depreciated.need to downlowd the artiodactyla seperately


#----------------------------------------------------------#
# 0. Set up  -----
#----------------------------------------------------------#
library(here)
#devtools::install_github("alrobles/mdd")
library(mdd)
library(terra) # for plot
library(purrr)
library(tidyverse)
library(sf)

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
# Load configuration (e.g., data_storage_path)
source(here::here("R/00_Config_file_HKH.R"))


# species list
species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

# for wide ranged elevations
# full list (global)
verts_alpine_generalists <- readxl::read_excel("~/Desktop/Datasets/Data_Schultz_et_al_global_alpine_biodiversity/Checklists/Vertebrates/verts_alpine_generalists.xlsx")



# endemism file (contains all species that are processed)
total_endemism_join <- read.csv(
  paste0(data_storage_path, "Datasets/species_list/species_endemism/total_species_endemism.csv")
)|>
  rename(sciname=species)

dem <- rast("~/Desktop/Datasets/Mountains/DEM/DEM for Lotta/GMTED2010_30.tiff")
plot(dem)

missing_species_df <- species_list|>
  filter(!sciname %in% total_endemism_join$sciname)|>
  distinct(sciname)

writexl::write_xlsx(missing_species_df,paste0(data_storage_path,"Datasets/species_list/rasterfiles/missing_species/missing.species.xlsx"))


#----------------------------------------------------------#
# Download mammal range from MDD  -----
#----------------------------------------------------------#
arto <- st_read("~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/species_list/Artiodactyla/MDD_Artiodactyla/MDD_Artiodactyla.gpkg")

arto_subset<-
  arto|>
  filter(sciname %in%mdd_check_log$sciname)


target_sciname <- "Cervus nippon"

mammal<- arto_subset|>
  filter(sciname==target_sciname)|>
  select(geom)|>
  sf::st_as_sf()

x11()
plot(mammal)



#----------------------------------------------------------#
# elevational mask  -----
#----------------------------------------------------------#

# crop dem to mammal extent 
dem_sci_crop <- crop(dem,mammal)

# set the value to 1
mammal$val <- 1

# rasterize
mammal_raster <- rasterize(mammal, dem_sci_crop, field = "val", background = 0, touches = TRUE)

# crop to remove extra 0s
mammal_raster <- mask(mammal_raster, mammal)
plot(mammal_raster)

#----------------------------------------------------------#
# elevation mask for mammal distribution  -----
#----------------------------------------------------------#
# species list
species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

sciname_elev <- species_list|>
  filter(sciname == target_sciname)|>
  select(sciname, average_min_elevation, average_max_elevation)|>
  distinct()

elev_min <- as.numeric(sciname_elev$average_min_elevation[1])
elev_max <- as.numeric(sciname_elev$average_max_elevation[1])


# for wide ranged spec

elev_global <- verts_alpine_generalists |>
  filter(sciname==target_sciname)|>
  summarise(
    glob_avg_min_elevation = min(min_elevation, na.rm = TRUE),
    glob_avg_max_elevation = max(max_elevation, na.rm = TRUE))


elev_min <- as.numeric(elev_global$glob_avg_min_elevation[1])
elev_max <- as.numeric(elev_global$glob_avg_max_elevation[1])


# cut range 

if (!compareGeom(mammal_raster, dem_sci_crop, stopOnError = FALSE)) {
  # Reproject if CRS differs, then resample to the mammal grid
  if (!identical(crs(mammal_raster), crs(dem_sci_crop))) {
    dem_sci_crop <- project(dem_sci_crop, mammal_raster)
  }
  dem_sci_crop <- resample(dem_sci_crop, mammal_raster, method = "bilinear")
}

# build elevation "outside" mask 
outside <- is.na(dem_sci_crop) | dem_sci_crop < elev_min | dem_sci_crop > elev_max

# zero-out presence where outside the elevation range 
mammal_elev_masked <- ifel((mammal_raster > 0) & outside, 0, mammal_raster)

#
mammal_elev_masked[mammal_elev_masked != 1] <- NA

# view
x11()
plot(mammal_elev_masked)

# save file 
writeRaster(mammal_elev_masked, file.path(data_storage_path, "Datasets", "species_list","rasterfiles","missing_species", paste0(gsub(" ", "_", target_sciname), "_elev_masked.tif")), overwrite = TRUE)


