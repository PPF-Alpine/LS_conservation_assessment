#----------------------------------------------------------#
# 0. Set up  -----
#----------------------------------------------------------#
library(here)
#devtools::install_github("alrobles/mdd")
library(mdd)
library(terra) # for plot
library(purrr)
library(tidyverse)
library(sf)

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# 0. load files  -----
#----------------------------------------------------------#

# load files 
# dem 
# lc 
# species list 
# ecoregions


species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))
#----------------------------------------------------------#
# filter
#----------------------------------------------------------#


small_ranged <- species_list |>
  select(
    sciname,
    average_min_elevation,
    average_max_elevation
  ) |>
  mutate(
    average_min_elevation = as.numeric(average_min_elevation),
    average_max_elevation = as.numeric(average_max_elevation),
    elev_range = average_max_elevation - average_min_elevation
  )|>
  distinct()
