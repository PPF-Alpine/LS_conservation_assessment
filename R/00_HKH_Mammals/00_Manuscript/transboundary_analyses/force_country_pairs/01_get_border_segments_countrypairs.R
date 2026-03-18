library(sf)
library(dplyr)
library(purrr)
library(rnaturalearth)
library(lwgeom)
library(terra)

#---------------------------------------------#
# 1. get HKH country polygons
#---------------------------------------------#

hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh")

countries_hkh <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name %in% hkh_countries) |>
  st_make_valid()

# use same projected CRS for lengths in metres
countries_hkh <- st_transform(countries_hkh, 8857)


#---------------------------------------------#
# 2. find touching country pairs
#---------------------------------------------#

touch_list <- st_touches(countries_hkh)

country_pairs <- tibble(
  i = rep(seq_along(touch_list), lengths(touch_list)),
  j = unlist(touch_list)
) |>
  filter(i < j)

#---------------------------------------------#
# 3. extract shared border for each country pair
#---------------------------------------------#

pairwise_borders <- map_dfr(seq_len(nrow(country_pairs)), function(k) {
  
  i <- country_pairs$i[k]
  j <- country_pairs$j[k]
  
  # shared boundary between exactly these two countries
  g <- st_intersection(
    st_boundary(countries_hkh[i, ]),
    st_boundary(countries_hkh[j, ])
  )
  
  if (nrow(g) == 0) return(NULL)
  
  # keep only line geometries
  g <- st_collection_extract(g, "LINESTRING")
  if (nrow(g) == 0) return(NULL)
  
  g$country_a <- countries_hkh$name[i]
  g$country_b <- countries_hkh$name[j]
  g
}) |>
  st_as_sf()


pairwise_borders_merged <- pairwise_borders |>
  st_transform(8857) |>
  group_by(country_a, country_b) |>
  summarise(
    geometry = st_line_merge(st_union(geometry)),
    .groups = "drop"
  )

#---------------------------------------------#
# 4. function to split one LINESTRING into ~100 km segments
#---------------------------------------------#

border_segments <- map_dfr(seq_len(nrow(pairwise_borders_merged)), function(i) {
  
  # take one country-pair border
  one_pair <- pairwise_borders_merged[i, ]
  
  # if geometry is MULTILINESTRING, split into LINESTRING parts
  one_pair_lines <- one_pair |>
    st_cast("LINESTRING", warn = FALSE)
  
  # apply your function to each LINESTRING part
  segs <- map_dfr(seq_len(nrow(one_pair_lines)), function(j) {
    
    out <- split_line_100km(
      line = st_geometry(one_pair_lines)[[j]],
      segment_length = 100000,
      crs = st_crs(one_pair_lines)
    )
    
    out$country_a <- one_pair$country_a[1]
    out$country_b <- one_pair$country_b[1]
    out$pair_id <- paste(one_pair$country_a[1],
                         one_pair$country_b[1],
                         sep = "_")
    out$line_id <- j
    
    out
  })
  
  segs
})

#---------------------------------------------#
# 6. inspect
#---------------------------------------------#
border_segments$seg_id <- seq_len(nrow(border_segments))
border_segments$len_km <- as.numeric(st_length(border_segments)) / 1000

summary(border_segments$len_km)


#---------------------------------------------#
# safe
#---------------------------------------------#

sf::st_write(
  border_segments,
  paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"),
  delete_layer = TRUE
)

