
#----------------------------------------------------------#
#     clean PAs
#----------------------------------------------------------#

clean_PAs <- wdpa_clean(wdpa_mountain_range) 

#----------------------------------------------------------#
#     overlap with mountain
#----------------------------------------------------------#

# Reproject 
if (!sf::st_crs(clean_PAs) == sf::st_crs(mountain_range)) {
  mountain_range <- sf::st_transform(mountain_range, sf::st_crs(clean_PAs))
}

# intersect the mountain range
clean_PAs_himalaya <- clean_PAs |> 
  dplyr::filter(sf::st_intersects(geometry, mountain_range, sparse = FALSE))

clean_PAs_himalaya <- st_transform(clean_PAs_himalaya, 4326)
mountain_range <- st_transform(mountain_range, 4326)
