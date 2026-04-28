

# look up table ndlsa 

#---------------------------------------------#
# get borders GADM
#---------------------------------------------#

IND_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_IND_shp/gadm41_IND_0_clean.shp"))|>
  dplyr::filter(GID_0 != "IND")
# 5 obs


PAK_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/gadm41_PAK_0_clean.shp"))|>
  dplyr::filter(GID_0 != "PAK")
# 1 obs


CHN_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/cleaned_CHN.shp"))|>
  dplyr::filter(GID_0 != "CHN")

ndlsa_all <- bind_rows(
  PAK_ndlsa,
  IND_ndlsa,
  CHN_ndlsa,
)|>
  st_make_valid()

# names from worldbank 

borders_WB <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/World Bank Official Boundaries - Admin 0_all_layers/WB_GAD_ADM0_complete.shp"))

hkh_boundary <- sf::st_read("~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/HKH_boundary/HKH_Boundary.shp")

# high resolution but doesnt work with PA which is lower resolution 
# get the hkh countries and also areas with non determined legal status 
hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh")

ndls <- borders_WB |>
  filter(
    NAM_0 %in% hkh_countries |
      WB_STATUS == "Non-determined legal status area"
  )

ndls_crop <- sf::st_intersection(ndls, hkh_boundary)|>
  filter( WB_STATUS == "Non-determined legal status area")|>
  dplyr::select(NAM_0,geometry)



# intersect 
intersections_ndlsa <- st_intersection(ndlsa_all, ndls_crop) 

intersections_ndlsa$overlap_area <- st_area(intersections_ndlsa)

intersections_ndlsa_clean <- intersections_ndlsa |>
  group_by(GID_0) |>
  slice_max(overlap_area, n = 1, with_ties = FALSE) |>
  ungroup()|>
  select(GID_0,NAM_0)|>
  rename(ndlsa_name=NAM_0)|>
  sf::st_drop_geometry()


writexl::write_xlsx(intersections_ndlsa_clean,paste0(data_storage_path, "Datasets/transboundary/GADM/ndlsa_clean.xlsx"))
