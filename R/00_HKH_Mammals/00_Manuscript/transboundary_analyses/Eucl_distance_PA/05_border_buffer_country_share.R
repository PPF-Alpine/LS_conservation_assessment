
buffer_folder <- paste0(
  data_storage_path,
  "Datasets/transboundary/GADM/pairwise_border_buffers/"
)

buffer_files <- list.files(
  buffer_folder,
  pattern = "\\.gpkg$",
  full.names = TRUE
)

border_buffer_sides <- buffer_files |>
  map(st_read, quiet = TRUE) |>
  bind_rows() |>
  st_make_valid() |>
  st_transform(st_crs(pa)) |>
  mutate(
    buffer_side_id = row_number(),
    buffer_area_m2 = as.numeric(st_area(geom)),
    buffer_area_km2 = buffer_area_m2 / 1e6
  )

#---------------------------------------------#
# get country borders GADM for each country individually
#---------------------------------------------#
AFG <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_AFG_shp/gadm41_AFG_0.shp"))
# 1 obs 

IND_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_IND_shp/gadm41_IND_0_clean.shp"))|>
  dplyr::filter(GID_0 == "IND")
# 1 obs

IND_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_IND_shp/gadm41_IND_0_clean.shp"))|>
  dplyr::filter(GID_0 != "IND")
# 5 obs

NPL <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_NPL_shp/gadm41_NPL_0.shp"))
# 1 obs 

PAK_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/gadm41_PAK_0_clean.shp"))|>
  dplyr::filter(GID_0 == "PAK")
# 1 obs 

PAK_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_PAK_shp/gadm41_PAK_0_clean.shp"))|>
  dplyr::filter(GID_0 != "PAK")
# 1 obs

BTN <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_BTN_shp/gadm41_BTN_0.shp"))

# GADM
CHN_country <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/cleaned_CHN.shp"))|>
  dplyr::filter(GID_0 == "CHN")

CHN_ndlsa <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_CHN_shp/cleaned_CHN.shp"))|>
  dplyr::filter(GID_0 != "CHN")

MMR <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_MMR_shp/gadm41_MMR_0.shp"))

BGD <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/gadm41_BGD_shp/gadm41_BGD_0.shp"))


plot(CHN_country$geometry)
#---------------------------------------------#
# 1. combine and clean names
#---------------------------------------------#

countries_all <- bind_rows(
  AFG, 
  NPL, 
  PAK_country, 
  PAK_ndlsa,
  IND_country, 
  IND_ndlsa,
  CHN_country, 
  CHN_ndlsa,
  MMR, 
  BGD, BTN
)|>
  st_make_valid()

#---------------------------------------------#
# how much of country is covered by border buffer ? 
#---------------------------------------------#

plot(pa_distance_coverage_summary$geom)

plot(border_buffer_sides$geom)

st_write(
  border_buffer_sides,
  paste0(data_storage_path, "Datasets/transboundary/GADM/pairwise_border_buffers/borderbuffer_combined.gpkg")
)


buffers_union <- border_buffer_sides |>
  st_make_valid() |>
  st_union()

plot(buffers_union)


countries_eq <- countries_all |>
  st_make_valid() |>
  st_transform(st_crs(buffers_union))

country_buffers <- countries_eq |>
  st_intersection(buffers_union)


country_buffers_area <- country_buffers |>
  mutate(buffer_area_km2 = as.numeric(st_area(geometry)) / 1e6)

country_area <- countries_eq |>
  mutate(country_area_km2 = as.numeric(st_area(geometry)) / 1e6)

coverage_df <- country_area |>
  st_drop_geometry() |>
  select(GID_0, COUNTRY, country_area_km2) |>
  left_join(
    country_buffers_area |>
      st_drop_geometry() |>
      select(GID_0, buffer_area_km2),
    by = "GID_0"
  ) |>
  mutate(
    buffer_area_km2 = tidyr::replace_na(buffer_area_km2, 0),
    border_share_of_country = buffer_area_km2 / country_area_km2
  )


writexl::write_xlsx(coverage_df, paste0(data_storage_path, "Datasets/transboundary/GADM/country_share_borderbuffers.xlsx"))
