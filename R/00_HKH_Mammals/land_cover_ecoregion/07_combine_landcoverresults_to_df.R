# Combine results ---
res <- tot_cells |>
  rename(total_cells = val) |>
  full_join(tot_area |> rename(total_area_km2 = area), by = "hkh_lc-2021") |>
  full_join(pres_cells |> rename(presence_cells = val), by = "hkh_lc-2021") |>
  full_join(pres_area |> rename(presence_area_km2 = area), by = "hkh_lc-2021") |>
  # Replace NAs with 0
  mutate(across(c(total_cells, total_area_km2, presence_cells, presence_area_km2),
                ~replace_na(.x, 0))) |>
  rename(landcover_id = "hkh_lc-2021")


# Add Percentages ---

# total species area
total_species_area_km2 <- as.numeric(
  global(cell_km2 * mammal_elev_masked_res, fun = "sum", na.rm = TRUE)[1,1]
)

# 
res_lc <- res |>
  mutate(
    share_of_species_range_pct = 100 * presence_area_km2 / total_species_area_km2,
    ecoregion_occupied_pct = if_else(total_area_km2 > 0,
                                     100 * presence_area_km2 / total_area_km2, 
                                     NA_real_)
  ) |>
  left_join(lc_data_descr, by = "landcover_id") |>
  bind_rows(
    summarise(.,
              landcover_id = NA_integer_,
              lc_class = "z_range outside HKH",
              share_of_species_range_pct = 100 - sum(share_of_species_range_pct, na.rm = TRUE),
              .groups = "drop"
    )
  ) |>
  mutate(.outside = if_else(is.na(landcover_id), 1L, 0L)) |>   # ensure the outside row is last
  arrange(.outside, desc(presence_area_km2)) |>
  select(-.outside)