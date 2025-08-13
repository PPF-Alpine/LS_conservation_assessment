
# workflow for one species to 
# 1. download mammal range
# 2. get the dem
# 3. retrieve elevational ranges
# 4. remove elevational ranges outside mammal range
# 5. get land cover classes of each mammal range
# 6. ecoregions and mammal ranges within HKH
#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
#devtools::install_github("alrobles/mdd")
library(mdd)
library(terra) # for plot

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

# MDD package 
# https://alrobles.github.io/mdd/

#----------------------------------------------------------#
# 1. Download mammal range  -----
#----------------------------------------------------------#

target_sciname <- "Marmota himalayana"

mammal <- get_mdd_map(target_sciname)
plot(mammal)

#----------------------------------------------------------#
# 2. species data, land cover and DEM  -----
#----------------------------------------------------------#
lc <- rast(paste0(data_storage_path, "Datasets/land_cover/rlcms_2021/data/hkh_lc-2021.tif")) 

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

lc_data_descr <- readxl::read_excel(paste0(data_storage_path,"Datasets/land_cover/lc_data_description.xlsx"))

# ❗ rewrite config file for both datastorage paths
source(here::here("R/00_Config_file.R"))
dem <- rast(paste0(data_storage_path, "Datasets/Mountains/DEM/GMTED2010_30.tiff"))

# crop the dem to hkh boundary
dem_crop <- crop(dem, lc)
plot(dem_crop)

writeRaster(dem_crop, file.path(data_storage_path, "Datasets", "DEM_HKH","DEM_HKH.tif"), overwrite = TRUE)

#----------------------------------------------------------#
# 3. rasterize species distribution  -----
#----------------------------------------------------------#

mammal$val <- 1
mammal_raster <- rasterize(mammal, dem_crop, field = "val", background = 0, touches = TRUE)
# crop to remove extra 0s
mammal_raster <- mask(mammal_raster, mammal)
plot(mammal_raster)

#----------------------------------------------------------#
# 4. elevation mask for mammal distribution  -----
#----------------------------------------------------------#
sciname_elev <- species_list|>
  filter(sciname == target_sciname)|>
  select(sciname, average_min_elevation, average_max_elevation)|>
  distinct()

elev_min <- as.numeric(sciname_elev$average_min_elevation[1])
elev_max <- as.numeric(sciname_elev$average_max_elevation[1])


if (!compareGeom(mammal_raster, dem_crop, stopOnError = FALSE)) {
  # Reproject if CRS differs, then resample to the mammal grid
  if (!identical(crs(mammal_raster), crs(dem_crop))) {
    dem_crop <- project(dem_crop, mammal_raster)
  }
  dem_crop <- resample(dem_crop, mammal_raster, method = "bilinear")
}

# build elevation "outside" mask 
outside <- is.na(dem_crop) | dem_crop < elev_min | dem_crop > elev_max

# zero-out presence where outside the elevation range 
mammal_elev_masked <- ifel((mammal_raster > 0) & outside, 0, mammal_raster)

# view
plot(mammal_elev_masked)

writeRaster(mammal_elev_masked, file.path(data_storage_path, "Datasets", "species_list", paste0(gsub(" ", "_", target_sciname), "_elev_masked_new.tif")), overwrite = TRUE)


mammal_elev_masked[mammal_elev_masked != 1] <- NA

#----------------------------------------------------------#
# 5. pixels of ecoregions within distribution range  -----
#----------------------------------------------------------#
eco <- rast(paste0(data_storage_path, "Datasets/ecoregions/ecoreg_HKH.tif")) 
plot(eco)

# resample to the eco grid
mammal_elev_masked_res <- resample(mammal_elev_masked,eco,method = "near")
plot(mammal_elev_masked_res)

# get Cell area (km²) on the presence grid 
cell_km2 <- cellSize(mammal_elev_masked_res, unit = "km")

# Total cell count per ecoregion
ones <- mammal_elev_masked_res; values(ones) <- 1

tot_cells <- zonal(ones, eco, fun = "sum", na.rm = TRUE)        # number of cells

# Total area per ecoregion in km2
tot_area  <- zonal(cell_km2, eco, fun = "sum", na.rm = TRUE)     # km²

# Count of presence cells 
pres_cells <- zonal(mammal_elev_masked_res, eco, fun = "sum", na.rm = TRUE)

# Area of presence within each ecoregion in km2
pres_area  <- zonal(cell_km2 * mammal_elev_masked_res, eco, fun = "sum", na.rm = TRUE)

# Combine results ---
res <- tot_cells |>
  rename(total_cells = val) |>
  full_join(tot_area |> rename(total_area_km2 = area), by = "ECO_NAME") |>
  full_join(pres_cells |> rename(presence_cells = val), by = "ECO_NAME") |>
  full_join(pres_area |> rename(presence_area_km2 = area), by = "ECO_NAME") |>
  # Replace NAs with 0
  mutate(across(c(total_cells, total_area_km2, presence_cells, presence_area_km2),
                ~replace_na(.x, 0))) |>
  rename(ecoregion_id = ECO_NAME)

# Add Percentages ---

# total species area
total_species_area_km2 <- as.numeric(
  global(cell_km2 * mammal_elev_masked_res, fun = "sum", na.rm = TRUE)[1,1]
)

# 
res_eco <- res |>
  mutate(
    share_of_species_range_pct = 100 * presence_area_km2 / total_species_area_km2,
    ecoregion_occupied_pct = if_else(total_area_km2 > 0,
                                     100 * presence_area_km2 / total_area_km2, 
                                     NA_real_)
  )

res_eco_plot <- res_eco|>
  filter(share_of_species_range_pct>0)
#----------------------------------------------------------#
# PLOT ----
#----------------------------------------------------------#
p <- ggplot(res_eco_plot, aes(x = factor(ecoregion_id), 
                              y = share_of_species_range_pct)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  labs(
    x = "WWF ecoregion",
    y = "Share of species range (%)",
    title = "Proportion of Marmota himalayana range by WWF ecoregion"
  ) +
  theme_minimal() +
  coord_flip()  # flips the axes

plot(p)

#----------------------------------------------------------#
# save as csv ----
#----------------------------------------------------------#

write.csv(res_lc,paste0(data_storage_path,"Output/land_cover/lc_test_marmota_himalayana.csv"))


ggsave(paste0(data_storage_path,"Visualizations/ecoregions/eco_test_marmota_himalayana.jpg"), plot = p,
       width = 10, height = 6, dpi = 300)





#----------------------------------------------------------#
# 5. pixels of land cover classes within distribution range  -----
#----------------------------------------------------------#
eco <- rast(paste0(data_storage_path, "Datasets/ecoregions/ecoreg_HKH.tif")) 
plot(eco)

# resample to the eco grid
mammal_elev_masked_res <- resample(mammal_elev_masked,lc,method = "near")
plot(mammal_elev_masked_res)

# get Cell area (km²) on the presence grid 
cell_km2 <- cellSize(mammal_elev_masked_res, unit = "km")

# Total cell count per ecoregion
ones <- mammal_elev_masked_res; values(ones) <- 1

tot_cells <- zonal(ones, lc, fun = "sum", na.rm = TRUE)        # number of cells

# Total area per ecoregion in km2
tot_area  <- zonal(cell_km2, lc, fun = "sum", na.rm = TRUE)     # km²

# Count of presence cells 
pres_cells <- zonal(mammal_elev_masked_res, lc, fun = "sum", na.rm = TRUE)

# Area of presence within each ecoregion in km2
pres_area  <- zonal(cell_km2 * mammal_elev_masked_res, lc, fun = "sum", na.rm = TRUE)

# Combine results ---
res <- tot_cells |>
  rename(total_cells = val) |>
  full_join(tot_area |> rename(total_area_km2 = area), by = "hkh_lc-2021") |>
  full_join(pres_cells |> rename(presence_cells = val), by = "hkh_lc-2021") |>
  full_join(pres_area |> rename(presence_area_km2 = area), by = "hkh_lc-2021") |>
  # Replace NAs with 0
  mutate(across(c(total_cells, total_area_km2, presence_cells, presence_area_km2),
                ~replace_na(.x, 0))) |>
  rename(landcover_id = "hkh_lc-2021")


# Add Percentages ---

# total species area
total_species_area_km2 <- as.numeric(
  global(cell_km2 * mammal_elev_masked_res, fun = "sum", na.rm = TRUE)[1,1]
)

# 
res_lc <- res |>
  mutate(
    share_of_species_range_pct = 100 * presence_area_km2 / total_species_area_km2,
    ecoregion_occupied_pct = if_else(total_area_km2 > 0,
                                     100 * presence_area_km2 / total_area_km2, 
                                     NA_real_)
  ) |>
  left_join(lc_data_descr, by = "landcover_id") |>
  bind_rows(
    summarise(.,
              landcover_id = NA_integer_,
              lc_class = "z_range outside HKH",
              share_of_species_range_pct = 100 - sum(share_of_species_range_pct, na.rm = TRUE),
              .groups = "drop"
    )
  ) |>
  mutate(.outside = if_else(is.na(landcover_id), 1L, 0L)) |>   # ensure the outside row is last
  arrange(.outside, desc(presence_area_km2)) |>
  select(-.outside)


#----------------------------------------------------------#
# PLOT ----
#----------------------------------------------------------#
p<-ggplot(res_lc, aes(x = factor(lc_class), 
                   y = share_of_species_range_pct)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  labs(
    x = "Land Cover Type",
    y = "Share of species range (%)",
    title = "Proportion of Marmota himalayana range by land cover type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60,vjust = 0.8))
plot(p)
#----------------------------------------------------------#
# save as csv ----
#----------------------------------------------------------#

write.csv(res_lc,paste0(data_storage_path,"Output/land_cover/lc_test_marmota_himalayana.csv"))


ggsave(paste0(data_storage_path,"Visualizations/land_cover/lc_test_marmota_himalayana.jpg"), plot = p,
       width = 8, height = 6, dpi = 300)
#----------------------------------------------------------#
# 5. polygon approach -----
#----------------------------------------------------------#

# Convert to polygons
# dissolve=TRUE merges touching presence cells into larger polygons
sp_poly <- as.polygons(mammal_elev_masked, dissolve = TRUE, values = FALSE)  
plot(sp_poly)

eco <- sf::st_read(paste0(data_storage_path, "Datasets/ecoregions/ecoregions_HKH.shp"))|>
  select(ECO_NAME)
plot(eco)






