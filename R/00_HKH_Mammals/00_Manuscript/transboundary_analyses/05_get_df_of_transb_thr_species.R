
library(sf)
library(terra)
library(dplyr)
library(purrr)
library(tidyr)
library(rnaturalearth)

source(here::here("R/00_Config_file_HKH.R"))

#---------------------------------------------#
# get border segments
#---------------------------------------------#
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/transb_glob_threat_new.shp"))

library(dplyr)
library(tidyr)
library(stringr)

unique_species <- border_segments %>%
  st_drop_geometry() %>%                     # drop geometry
  separate_rows(spcs_ls, sep = ",") %>%      # split into rows
  mutate(spcs_ls = str_trim(spcs_ls)) %>%    # remove spaces
  distinct(spcs_ls) %>%                      # keep unique
  pull(spcs_ls)

unique_species


species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))|>
  rename(species = sciname)

species_list %>%
  summarise(n_species = n_distinct(species))


threatened <- species_list|>
  filter(status_summary_global=="threatened")

threatened <- species_list|>
  filter(status_summary_national=="threatened")|>
  distinct(sciname)

threatened_glob <- species_list|>
  filter(status_summary_global=="threatened")|>
  distinct(sciname)


123/293
