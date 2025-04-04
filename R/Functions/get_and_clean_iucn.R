flatten_assessment <- function(assessment, genus, species) {
  if (is.null(assessment) || length(assessment) == 0) {
    return(tibble(
      sciname = paste(genus, species),
      year_published = NA,
      criteria = NA,
      citation = NA,
      red_list_version = NA,
      red_list_code = NA,
      threats_code = NA,
      threats_description = NA,
      stresses_code = NA,
      stresses_description = NA
    ))
  }
  
  get_or_na <- function(x) if (is.null(x)) NA else x
  
  year <- get_or_na(assessment$year_published)
  criteria <- get_or_na(assessment$criteria)
  citation <- get_or_na(assessment$citation)
  rl_version <- get_or_na(assessment$red_list_category$version)
  rl_code <- get_or_na(assessment$red_list_category$code)
  
  threats_codes <- if (!is.null(assessment$threats)) paste(get_or_na(assessment$threats$code), collapse = "; ") else NA
  threats_desc <- if (!is.null(assessment$threats)) paste(get_or_na(assessment$threats$description), collapse = "; ") else NA
  stresses_codes <- if (!is.null(assessment$stresses)) paste(get_or_na(assessment$stresses$code), collapse = "; ") else NA
  stresses_desc <- if (!is.null(assessment$stresses)) paste(get_or_na(assessment$stresses$description), collapse = "; ") else NA
  
  tibble(
    sciname = paste(genus, species),
    year_published = year,
    criteria = criteria,
    citation = citation,
    red_list_version = rl_version,
    red_list_code = rl_code,
    threats_code = threats_codes,
    threats_description = threats_desc,
    stresses_code = stresses_codes,
    stresses_description = stresses_desc
  )
}



get_and_clean_iucn <- function(genus, species) {
  tryCatch({
    res <- rl_species_latest(genus = genus, species = species, key = iucn_key,
                             pause_base = 2,   # start retry wait at 2 seconds
                             pause_cap = 10,   # max wait time is 10 seconds
                             pause_min = 1,    # minimum wait of 1 second
                             times = 5         # retry up to 5 times
                             )
    flatten_assessment(res, genus, species)
  }, error = function(e) {
    message(paste("No IUCN assessment available for", genus, species, ":", e$message))
    flatten_assessment(NULL, genus, species)
  })
}


# added pause in between to avoid too many requests at the same time. 
get_and_clean_iucn <- function(genus, species) {
  Sys.sleep(1.5)  # pause for 1.5 seconds to avoid rate limiting
  tryCatch({
    res <- rl_species_latest(genus = genus, species = species, key = iucn_key)
    flatten_assessment(res, genus, species)
  }, error = function(e) {
    message(paste("No IUCN assessment available for", genus, species, ":", e$message))
    flatten_assessment(NULL, genus, species)
  })
}

