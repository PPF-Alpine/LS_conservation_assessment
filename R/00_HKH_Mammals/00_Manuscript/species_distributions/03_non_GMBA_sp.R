library(leaflet)
library(htmlwidgets)
library(sf)
# Load configuration file


source(here::here("R/00_Config_file_HKH.R"))

# 
hkh_gmba <- sf::st_read("C:/Users/losch5089/OneDrive - University of Bergen/Desktop/ICIMOD_work/HKH_Boundary/HKH_GMBA_clip.shp")

hkh_gmba_mountains <- c("Himalaya",
                        "Hindu Kush",
                        "Balochistan Ranges",
                        "Karakoram",
                        "Tibetan Plateau")

hkh_gmba_subset <- hkh_gmba|>
  filter(MapName%in%hkh_gmba_mountains)

hkh_boundaries <- sf::st_read("C:/Users/losch5089/OneDrive - University of Bergen/Desktop/ICIMOD_work/HKH_Boundary/HKH_Boundary.shp")


pal <- colorFactor(palette = "Set2", domain = hkh_gmba$MapName)

# leaflet map to check out HKH boundary

leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo Map") %>%
  addPolygons(data = hkh_boundaries,
              color = "darkblue",
              weight = 2,
              fillOpacity = 0.1,
              group = "HKH Boundaries") %>%
  addPolygons(data = hkh_gmba_subset,
              fillColor = ~pal(MapName),
              color = "black",
              weight = 1,
              fillOpacity = 0.6,
              popup = ~MapName,
              group = "HKH GMBA") %>%
  addLegend("bottomright",
            pal = pal,
            values = hkh_gmba_subset$MapName,
            title = "GMBA names") %>%
  addLayersControl(
    overlayGroups = c("HKH Boundaries", "HKH GMBA"),
    options = layersControlOptions(collapsed = FALSE)
  )


# make CRS identical
hkh_gmba_subset <- st_transform(hkh_gmba_subset, st_crs(hkh_boundaries))

sf_use_s2(FALSE)

hkh_gmba_subset_clean <- hkh_gmba_subset |>
  st_make_valid() |>
  st_buffer(0)

gmba_mountains_union <- hkh_gmba_subset_clean |>
  summarise(geometry = st_union(geometry)) |>
  st_make_valid() |>
  st_buffer(0)


hkh_boundaries_clean <- hkh_boundaries |>
  st_make_valid() |>
  st_buffer(0)

hkh_non_gmba <- st_difference(
  hkh_boundaries_clean,
  gmba_mountains_union
) |>
  st_make_valid() |>
  st_buffer(0)

plot(hkh_non_gmba$geometry)

plot(hkh_non_gmba_dissolved$geometry)
plot(st_geometry(hkh_non_gmba), col = "red", border = NA)
plot(st_geometry(hkh_boundaries), add = TRUE, border = "black")

#----------------------------------------------------------#
# write as gpkg  -----
#----------------------------------------------------------#
sf::st_write(
  hkh_non_gmba,
  paste0(data_storage_path, "Datasets/HKH_boundary/hkh_non_gmba.gpkg")
)


#----------------------------------------------------------#
# read mammal geom and filter those in HKH overlapping area  --
#----------------------------------------------------------#

mammals <- readRDS(
  "~/Desktop/Datasets/Mammals/MDD/mammals_shapes_2023-05-15__1f2260484545061aa485b31d106b18ec__.rds"
)

# make CRS match
mammals <- st_transform(mammals, st_crs(hkh_non_gmba))

# make bounding box polygon around your area
hkh_bbox <- st_as_sfc(st_bbox(hkh_non_gmba)) |>
  st_as_sf() |>
  st_set_crs(st_crs(hkh_non_gmba))

# quick rough spatial subset by bbox
bb <- st_bbox(hkh_non_gmba)

bbox_df <- do.call(
  rbind,
  lapply(st_geometry(mammals), function(g) {
    x <- st_bbox(g)
    data.frame(xmin = x["xmin"], xmax = x["xmax"],
               ymin = x["ymin"], ymax = x["ymax"])
  })
)

keep <- bbox_df$xmax >= bb["xmin"] &
  bbox_df$xmin <= bb["xmax"] &
  bbox_df$ymax >= bb["ymin"] &
  bbox_df$ymin <= bb["ymax"]

mammals_bbox <- mammals[keep, ]

nrow(mammals_bbox)

# now precise clip/intersection
hkh_non_gmba_union <- hkh_non_gmba |>
  st_make_valid() |>
  summarise(geometry = st_union(geometry)) |>
  st_make_valid()

mammals_hkh_non_gmba <- purrr::map_dfr(
  seq_len(nrow(mammals_bbox)),
  function(i) {
    tryCatch(
      st_intersection(mammals_bbox[i, ], hkh_non_gmba_union),
      error = function(e) NULL
    )
  }
)

# species list dataframe
species_df <- mammals_hkh_non_gmba |>
  st_drop_geometry() 

# save as excel
writexl::write_xlsx(
  species_df,
  path = file.path(
    data_storage_path,
    "Datasets/species_list/HKH_non_GMBA_species_list.xlsx"
  )
)
#----------------------------------------------------------#
# write as gpkg  -----
#----------------------------------------------------------#
sf::st_write(
  mammals_hkh_non_gmba,
  paste0(data_storage_path, "Datasets/species_list/mammals_hkh_non_gmba.gpkg"),
  layer = "hkh_mammals",
  delete_layer = TRUE
)

#----------------------------------------------------------#
# cross check with species list -----
#----------------------------------------------------------#

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))|>
  distinct(sciname)


species_not_in_assessment <- species_df |>
  anti_join(
    species_list,
    by = "sciname")


species_not_in_assessment_shp <- mammals_hkh_non_gmba |>
  filter(sciname %in% species_not_in_assessment$sciname)

sf::st_write(
  species_not_in_assessment_shp,
  paste0(data_storage_path, "Datasets/species_list/mammals_missing.shp")
)


species_not_in_assessment_shp <- sf::st_read( paste0(data_storage_path, "Datasets/species_list/mammals_missing.gpkg"))

# species list dataframe
species_df <- species_not_in_assessment_shp |>
  st_drop_geometry() 

# save as excel
writexl::write_xlsx(
  species_df,
  path = file.path(
    data_storage_path,
    "Datasets/species_list/HKH_non_GMBA_species.xlsx"
  )
)


