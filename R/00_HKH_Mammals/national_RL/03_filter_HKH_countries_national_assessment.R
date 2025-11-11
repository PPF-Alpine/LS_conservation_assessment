
# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

library(tidyverse)
library(rgbif)
library(taxize)

RL_national_complete <- read.csv(paste0(data_storage_path,"RL_assessments/national_IUCN_RL_assessments_23052025.csv"))

HKH_list <- read.csv(paste0(data_storage_path,"HKH_list/HKH_mammals_LS_23052025.csv"))|>
  select(-X)

#----------------------------------------------------------#
# filter for HKH countries -
#----------------------------------------------------------#

HKH_countries <- c("Nepal", 
                   "Afghanistan", 
                   "Bhutan", 
                   "Pakistan", 
                   "Myanmar", 
                   "India", 
                   "Bangladesh", 
                   "China")

RL_national_filter <- RL_national_complete |>
  filter(str_detect(countries_iso, 
                    str_c(HKH_countries, collapse = "|")))

unique(RL_national_filter$countries_iso)

#----------------------------------------------------------#
#   save
#----------------------------------------------------------#

write.csv(RL_national_filter,paste0(data_storage_path,"RL_assessments/HKH_RL_assessments_23052025.csv"))