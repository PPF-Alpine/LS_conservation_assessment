
#---------------------------------------------#
# overview dataframes  
#---------------------------------------------#

# clean species names once
presence_long_clean <- presence_long |>
  mutate(
    species = species |>
      gsub("_elev_masked", "", x = _) |>
      gsub("_", " ", x = _)
  )

# which and how many species are shared per country pair 
shared_by_country_pair <- presence_long_clean |>
  select(seg_id, side, country, species, presence) |>
  pivot_wider(
    names_from = side,
    values_from = c(country, presence),
    values_fn = list(country = first, presence = max),
    values_fill = list(presence = 0)
  ) |>
  filter(presence_a == 1, presence_b == 1) |>
  mutate(
    country_pair = paste(
      pmin(country_a, country_b),
      pmax(country_a, country_b),
      sep = " - "
    )
  ) |>
  group_by(country_pair) |>
  summarise(
    countries = first(country_pair),
    n_shared_species = n_distinct(species),
    shared_species_text = paste(sort(unique(species)), collapse = "; "),
    seg_ids_text = paste(sort(unique(seg_id)), collapse = "; "),
    .groups = "drop"
  )


# which and how many species are shared per seg id
shared_by_segment <- presence_long_clean |>
  select(seg_id, side, country, species, presence) |>
  pivot_wider(
    names_from = side,
    values_from = c(country, presence),
    values_fn = list(country = first, presence = max),
    values_fill = list(presence = 0)
  ) |>
  filter(presence_a == 1, presence_b == 1) |>
  group_by(seg_id, country_a, country_b) |>
  summarise(
    n_shared_species = n_distinct(species),
    shared_species_text = paste(sort(unique(species)), collapse = "; "),
    .groups = "drop"
  )


# how many distinct transboundary species are there in total 
transboundary_species <- presence_long_clean |>
  select(seg_id, side, species, presence) |>
  pivot_wider(
    names_from = side,
    values_from = presence,
    values_fill = 0
  ) |>
  filter(a == 1, b == 1) |>
  distinct(species) |>
  arrange(species)

n_transboundary_species <- nrow(transboundary_species)

n_transboundary_species
# 101 species 

transboundary_species_list <- transboundary_species$species

#---------------------------------------------#
# save the dataframes   
#---------------------------------------------#

writexl::write_xlsx(
  shared_by_country_pair,
  path = file.path(
    data_storage_path,
    "Datasets/transboundary/transb_species/GLOBAL/shared_by_country_pair.xlsx"
  )
)

writexl::write_xlsx(
  shared_by_segment,
  path = file.path(
    data_storage_path,
    "Datasets/transboundary/transb_species/GLOBAL/shared_by_segment.xlsx"
  )
)


writexl::write_xlsx(
  transboundary_species,
  path = file.path(
    data_storage_path,
    "Datasets/transboundary/transb_species/GLOBAL/transboundary_species.xlsx"
  )
)
