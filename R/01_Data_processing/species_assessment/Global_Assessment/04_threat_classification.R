#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(rredlist)
library(rlist)
library(purrr)

# Load configuration file
source(here::here("R/00_Config_file.R"))

iucn_key <-"nG711EY6sxQrJKeV18Epiv5ngaeeMMTDFMNm"

#----------------------------------------------------------#
#         Load and prepare species data 
#----------------------------------------------------------#
# mammal checklist 
checklist_mammals <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_mammal_database.xlsx"))

# birds checklist 
checklist_birds <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_bird_database.xlsx"))

# reptiles checklist 
checklist_reptiles <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_reptile_database.xlsx"))

checklist <- bind_rows(checklist_mammals,checklist_reptiles,checklist_birds)

checklist <-checklist|>
  mutate(max_elevation = if_else(
    Mountain_range == "North European Highlands" & max_elevation > 2000,
    2000,
    max_elevation
  ))

# Join with treeline elevations
Treeline_Elevations <- readxl::read_excel(file.path(data_storage_path, "Datasets/Mountains/Treeline_Lapse_Rate_04_05.xlsx"))|>
  select(Mountain_range,
         Mean_elevation_2_degree,
         Mean_elevation_4_degree,
         Mean_elevation_6_degree)


# filter alpine species
checklist_reduced <-checklist|>
  select(source,sciname,Mountain_range,min_elevation,max_elevation,mean_treeline)|>
  left_join(Treeline_Elevations,by="Mountain_range")|>
  filter(max_elevation >= mean_treeline)




checklist_test <- checklist_reduced|>
  filter(Mountain_range=="Northern Andes")

  


#----------------------------------------------------------#
#        get the species names in format
#----------------------------------------------------------#

# get dataframe with genus and species
spp <- checklist_reduced %>%
  select(sciname) %>%
  # Separate into genus and species by space
  separate(sciname, into = c("genus", "species"), sep = " ", remove = FALSE)

# does not include subspecies 

#----------------------------------------------------------#
#       check out for one species 
#----------------------------------------------------------#

# for one species
# sp_assessment <- rl_species_latest(genus = "Ovis", species = "ammon",key=iucn_key)

# Extract and flatten columns
year <- sp_assessment$year_published
criteria <- sp_assessment$criteria
citation <- sp_assessment$citation
rl_version <- sp_assessment$red_list_category$version
rl_code <- sp_assessment$red_list_category$code

# collapse multiple entries
threats_codes <- paste(sp_assessment$threats$code, collapse = "; ")
threats_desc <- paste(sp_assessment$threats$description, collapse = "; ")
stresses_codes <- paste(sp_assessment$stresses$code, collapse = "; ")
stresses_desc <- paste(sp_assessment$stresses$description, collapse = "; ")

# Create a data frame with one row
sp_df <- data.frame(
  year_published = year,
  criteria = criteria,
  citation = citation,
  red_list_version = rl_version,
  red_list_code = rl_code,
  threats_code = threats_codes,
  threats_description = threats_desc,
  stresses_code = stresses_codes,
  stresses_description = stresses_desc,
  stringsAsFactors = FALSE
)

# View result
print(sp_df)



#----------------------------------------------------------#
#   return a list of entire assessment for all species
#----------------------------------------------------------#

#results_list <- pmap(spp[, c("genus", "species")], get_iucn)

#----------------------------------------------------------#
#   get a dataframe with info of interest for all species 
#----------------------------------------------------------#

# Apply across all species
assessment_threats_complete <- pmap_dfr(spp[, c("genus", "species")], get_and_clean_iucn)

write.csv(assessment_threats_complete,paste0(data_storage_path,"Outputs/IUCN_assessment_lists/assessment_threats_complete.csv"))


