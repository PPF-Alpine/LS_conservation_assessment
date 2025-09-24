# validate shapes ✅
validate_shapes <- function(species_shapes) {
  species_shapes <- sf::st_as_sf(species_shapes)
  message("Checking for invalid shapes...")
  invalids <- which(!sf::st_is_valid(species_shapes))
  
  if (length(invalids) > 0) {
    message("Invalid shapes found. Making invalid species valid.")
    species_shapes[invalids, ] <- sf::st_make_valid(species_shapes[invalids, ])
  } else {
    message("No invalid shapes found.")
  }
  
  return(species_shapes)
}


validate_shapes_individually <- function(species_shapes) {
  species_shapes <- sf::st_as_sf(species_shapes)
  message("Checking for invalid shapes...")
  
  for (i in 1:nrow(species_shapes)) {
    if (!sf::st_is_valid(species_shapes[i, ])) {
      message(paste("Invalid shape found at row", i, ". Making invalid species valid."))
      species_shapes[i, ] <- sf::st_make_valid(species_shapes[i, ])
    }
  }
  
  message("Shape validation complete.")
  return(species_shapes)
}


# to check if there are invalid geometries if yes make them valid

make_shapes_valid <- function(shp) {
  message("Checking for invalid geometries...")
  
  # Check for invalid geometries
  invalids <- which(!st_is_valid(shp))
  # If invalid geometries are found, make them valid
  if (length(invalids) > 0) {
    shp[invalids, ] <- st_make_valid(shp[invalids, ])
    message("Invalid geometries were found and made valid.")
  } else {
    message("No invalid geometries found.")
  }
  
  return(shp)
}