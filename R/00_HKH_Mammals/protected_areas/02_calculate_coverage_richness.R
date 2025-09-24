


# what proportion of richness is covered by PA
# how well are the top 30% of values represented in PA
# which unprotected areas have high rrichness


richness<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
pa_path <- paste0(data_storage_path, "Datasets/protected_areas/PA_HKH/pa_in_hkh.gpkg")
pa <- st_read(pa_path)


pa <- st_transform(pa, crs(richness))   # make sure both have the same projection
total_richness <- global(richness, "sum", na.rm = TRUE)[[1]]

richness_in_pa <- mask(richness, vect(pa))
covered_richness <- global(richness_in_pa, "sum", na.rm = TRUE)[[1]]
proportion_covered <- covered_richness / total_richness
proportion_covered


