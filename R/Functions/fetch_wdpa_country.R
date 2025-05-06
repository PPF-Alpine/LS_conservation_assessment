# Function to fetch WDPA data for a given country and return as an sf object
fetch_wdpa_country <- function(country_iso) {
  message(paste("Downloading WDPA data for:", country_iso))
  wdpa_fetch(country_iso, wait = TRUE) %>%
    st_as_sf()
}
