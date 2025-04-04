# Fetch IUCN data for all species
get_iucn_assessment <- function(genus, species) {
  tryCatch({
    res <- rl_species_latest(genus = genus, species = species, key = iucn_key)
    if (is.null(res$result) || length(res$result) == 0) {
      return(tibble(genus = genus, species = species))  # NA for all other fields
    } else {
      return(as_tibble(res$result))
    }
  }, error = function(e) {
    message(paste("Error for", genus, species, ":", e$message))
    return(tibble(genus = genus, species = species))  # NA for all other fields
  })
}