library(terra)
library(dplyr)
library(stringr)
library(readxl)

# lookup table for land cover descriptions
lc_data_descr <- read_excel(
  paste0(data_storage_path, "Datasets/land_cover/lc_data_description.xlsx")
)

process_species <- function(file, eco, lc, lc_data_descr) {
  
  # species name from file
  species_name <- str_remove(basename(file), "_elev_masked\\.tif$")
  message("Processing: ", species_name)
  
  # try/catch block
  out <- tryCatch({
    # read species raster
    sp_rast <- rast(file)
    
    # polygonize distribution range
    mammal_poly <- as.polygons(sp_rast, na.rm = TRUE, dissolve = TRUE)
    
    # ecoregions inside range
    eco_in_poly <- mask(crop(eco, mammal_poly), mammal_poly)
    
    # land cover inside range
    lc_in_poly <- mask(crop(lc, mammal_poly), mammal_poly)
    
    # counts per ecoregion
    eco_counts <- freq(eco_in_poly) |> 
      as.data.frame() |> 
      rename(category = value) |>
      mutate(
        source = "ecoregion",
        proportion = round(count / sum(count, na.rm = TRUE) * 100, 1)
      ) |>
      dplyr::select(category, count, proportion, source)
    
    # counts per land cover
    lc_counts <- freq(lc_in_poly) |> 
      as.data.frame() |> 
      rename(landcover_id = value) |>
      left_join(lc_data_descr, by = "landcover_id") |>
      dplyr::select(category = lc_class, count, landcover_id) |>
      mutate(
        source = "land_cover",
        proportion = round(count / sum(count, na.rm = TRUE) * 100, 1)
      ) |>
      dplyr::select(category, count, proportion, source)
    
    # combine
    bind_rows(eco_counts, lc_counts) |>
      mutate(species = species_name)
    
  }, error = function(e) {
    message("⚠️ Skipping ", species_name, " (", e$message, ")")
    return(NULL)  # species is skipped
  })
  
  return(out)
}


# --- Loop over all rasters ---
rdir <- paste0(data_storage_path, "Datasets/species_list/rasterfiles/subset_5/")
files <- list.files(rdir, pattern = "_elev_masked\\.tif$", full.names = TRUE)

results <- lapply(files, process_species, eco = eco, lc = lc, lc_data_descr = lc_data_descr)

# drop NULLs before combining
all_species_results <- bind_rows(results[!sapply(results, is.null)])


# save for later
write.csv(all_species_results,
          file = paste0(data_storage_path, "Datasets/species_list/rasterfiles/00_mammal_summaries/mammal_ecoregion_landcover_subset_5.csv"),
          row.names = FALSE)
