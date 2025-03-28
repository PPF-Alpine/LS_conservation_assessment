#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)

# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#         Load and arrange data 
#----------------------------------------------------------#

# IUCN assessment
assessments <- read.csv(paste0(data_storage_path,"Datasets/species_assessment/IUCN/birds_mammals_reptiles/assessments.csv"))

assessments_reduced <- assessments|>
  select(scientificName,redlistCategory,redlistCriteria,
         yearPublished,rationale,habitat,threats,
         population,populationTrend,range,conservationActions,possiblyExtinct,possiblyExtinctInTheWild)|>
  rename(sciname=scientificName)

# mammal checklist 
checklist_mammals <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_mammal_database.xlsx"))

# birds checklist 
checklist_birds <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_bird_database.xlsx"))

# reptiles checklist 
checklist_reptiles <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_reptile_database.xlsx"))

checklist <- bind_rows(checklist_mammals,checklist_reptiles,checklist_birds)

#----------------------------------------------------------#
#        Categorise alpine groups 
#----------------------------------------------------------#

# Join with treeline elevations
Treeline_Elevations <- readxl::read_excel(file.path(data_storage_path, "Datasets/Mountains/Treeline_Lapse_Rate_04_05.xlsx"))|>
  select(Mountain_range,
         Mean_elevation_2_degree,
         Mean_elevation_4_degree,
         Mean_elevation_6_degree)


checklist_reduced <-checklist|>
  select(source,sciname,Mountain_range,min_elevation,max_elevation,mean_treeline)|>
  left_join(Treeline_Elevations,by="Mountain_range")

# leftjoin
assessment_complete <- checklist_reduced|>
  left_join(assessments_reduced,by="sciname")|>
  mutate(redlistCategory = coalesce(redlistCategory, "Not assessed"))|>
  mutate(populationTrend = coalesce(populationTrend,"Not assessed"))


#----------------------------------------------------------#
#        lowland vs generalists vs specialists
#----------------------------------------------------------#

assessment_generalists <- assessment_complete|>
  filter(max_elevation >= mean_treeline & min_elevation <= mean_treeline)|>
  mutate(Group = "Generalists")


assessment_specialists <- assessment_complete|>
  filter(max_elevation >= mean_treeline & min_elevation >= mean_treeline)|>
  mutate(Group = "Specialists")


assessment_bm_alp <- assessment_complete|>
  filter(max_elevation >= mean_treeline & min_elevation >= Mean_elevation_6_degree)|>
  anti_join(assessment_specialists, by = "sciname") |>
  mutate(Group = "Montane")


# Combine all datasets into one
assessment_combined <- bind_rows(assessment_generalists, 
                                 assessment_specialists, 
                                 assessment_bm_alp)

#----------------------------------------------------------#
#        write assessment as csv
#----------------------------------------------------------#

write.csv(assessment_combined,paste0(data_storage_path,"Outputs/IUCN_assessment_lists/mammals_birds_reptiles_IUCN_Global.csv"))



