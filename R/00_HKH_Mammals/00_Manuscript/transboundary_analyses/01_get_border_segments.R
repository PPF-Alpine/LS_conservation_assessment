library(sf)
library(terra)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(units)
library(lwgeom)

# cut borders in 50 km segments
source(here::here("R/00_Config_file_HKH.R"))
#---------------------------------------------#
# get the hkh national borders
#---------------------------------------------#
species_richness_total<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))

# get the hkh national borders for plots

borders <- ne_countries(scale = "medium", returnclass = "sf")
hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal", "Bhutan", "China", "Myanmar", "Bangladesh")

borders <- borders[borders$name %in% hkh_countries, ]

# keep attributes, not geometry only
borders_vect <- vect(borders)

# crop borders to raster extent
borders_hkh <- crop(borders_vect, species_richness_total)
borders_hkh_sf <- sf::st_as_sf(borders_hkh)

borders_hkh_sf <- borders_hkh_sf |>
  st_make_valid() |>
  st_cast("MULTILINESTRING", warn = FALSE) |>
  st_cast("LINESTRING", warn = FALSE)

borders_hkh_sf <- st_transform(borders_hkh_sf, 8857)


#---------------------------------------------#
# split into xkm long segments
#---------------------------------------------#

split_line_100km <- function(line, segment_length = 100000, crs = st_crs(borders_hkh_sf)) {
  
  line <- st_sfc(line, crs = crs)
  total_length <- as.numeric(st_length(line))
  
  starts <- seq(0, total_length, by = segment_length)
  ends   <- pmin(starts + segment_length, total_length)
  
  segs <- map2(starts, ends, function(s, e) {
    if (e <= s) return(NULL)
    lwgeom::st_linesubstring(line, from = s / total_length, to = e / total_length)
  })
  
  segs <- segs[!sapply(segs, is.null)]
  
  # make sure each segment is its own feature row
  segs_sf <- do.call(rbind, lapply(seq_along(segs), function(i) {
    st_sf(seg_part = i, geometry = segs[[i]])
  }))
  
  segs_sf
}

#---------------------------------------------#
# get the segments
#---------------------------------------------#
border_segments <- map_dfr(seq_len(nrow(borders_hkh_sf)), function(i) {
  
  segs <- split_line_100km(
    borders_hkh_sf$geometry[i],
    segment_length = 100000
  )
  
  segs$border_id <- borders_hkh_sf$adm0_a3[i]   # or $name[i]
  segs
})


#---------------------------------------------#
# inspect
#---------------------------------------------#
border_segments$seg_id <- seq_len(nrow(border_segments))
border_segments$len_km  <- as.numeric(st_length(border_segments)) / 1000


summary(border_segments$len_km)



#---------------------------------------------#
# safe
#---------------------------------------------#

sf::st_write(
  border_segments,
  paste0(data_storage_path, "Datasets/protected_areas/borders_segments_100.shp"),
  delete_layer = TRUE
)
