
#----------------------------------------------------------#
# combine results into a dataframe -----
#----------------------------------------------------------#

# Combine results ---
res <- tot_cells |>
  rename(total_cells = val) |>
  full_join(tot_area |> rename(total_area_km2 = area), by = "ECO_NAME") |>
  full_join(pres_cells |> rename(presence_cells = val), by = "ECO_NAME") |>
  full_join(pres_area |> rename(presence_area_km2 = area), by = "ECO_NAME") |>
  # Replace NAs with 0
  mutate(across(c(total_cells, total_area_km2, presence_cells, presence_area_km2),
                ~replace_na(.x, 0))) |>
  rename(ecoregion_id = ECO_NAME)

# Add Percentages ---

# total species area
total_species_area_km2 <- as.numeric(
  global(cell_km2 * mammal_elev_masked_res, fun = "sum", na.rm = TRUE)[1,1]
)

# 
res_eco <- res |>
  mutate(
    share_of_species_range_pct = 100 * presence_area_km2 / total_species_area_km2,
    ecoregion_occupied_pct = if_else(total_area_km2 > 0,
                                     100 * presence_area_km2 / total_area_km2, 
                                     NA_real_)
  )