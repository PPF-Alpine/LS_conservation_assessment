#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(rredlist)
library(stringr)
library(scales)
library(purrr)
library(patchwork)

# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#        read in the LUC summaries 
#----------------------------------------------------------#

# Load both datasets
mountains_hfi <- read_csv(file.path(data_storage_path, "Outputs/IUCN_assessment_lists/mountain_hfi_summary.csv")) 
