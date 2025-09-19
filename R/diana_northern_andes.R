

gen<- read.csv("~/Desktop/Datasets/Data_Schultz_et_al_global_alpine_biodiversity/Checklists/Plants/plants_alpine_generalists.csv")|>
  distinct()|>
  filter(grepl("Colombia", geo_entity, ignore.case = TRUE)) |>
  arrange(desc(max_elev), desc(min_elev))



write.csv(
  gen,
  "~/Desktop/Datasets/Data_Schultz_et_al_global_alpine_biodiversity/Checklists/Plants/northern_andes_GIFT_filtered.csv",
  row.names = FALSE
)
