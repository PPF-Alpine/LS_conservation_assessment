library(terra)
library(stringr)
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(tools)

source(here::here("R/00_Config_file_HKH.R"))

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

se_1 <- read.csv(paste0(data_storage_path, "Datasets/species_list/species_endemism/species_endemism_1_100.csv"))
se_2 <- read.csv(paste0(data_storage_path, "Datasets/species_list/species_endemism/species_endemism_101_200.csv"))
se_3 <- read.csv(paste0(data_storage_path, "Datasets/species_list/species_endemism/species_endemism_201_300.csv"))
se_4 <- read.csv(paste0(data_storage_path, "Datasets/species_list/species_endemism/species_endemism_301_400.csv"))
se_5 <- read.csv(paste0(data_storage_path, "Datasets/species_list/species_endemism/species_endemism_401_497.csv"))

total_endemism <- bind_rows(se_1,se_2,se_3,se_4,se_5)

# bind species elevational ranges
elevations <- species_list|>
  select(sciname,average_min_elevation,average_max_elevation)|>
  rename(species=sciname)|>
  unique()

total_endemism_join <- total_endemism %>%
  mutate(species = str_replace_all(species, "_", " "))|>
  left_join(elevations, by= "species")|>
  mutate(elev_range = as.numeric(average_max_elevation) - as.numeric(average_min_elevation))


# define output path
out_path <- file.path(data_storage_path, "Datasets/species_list/species_endemism/total_species_endemism.csv")

# Save as CSV
write_csv(total_endemism_join, out_path)
