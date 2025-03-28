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
assessments <- read.csv(paste0(data_storage_path,"Datasets/species_assessment/IUCN/mammals/assessments.csv"))

assessments_reduced <- assessments|>
  select(scientificName,redlistCategory,redlistCriteria,
         yearPublished,rationale,habitat,threats,
         population,populationTrend,range,conservationActions,possiblyExtinct,possiblyExtinctInTheWild)|>
  rename(sciname=scientificName)

# mammal checklist for Himalaya
checklist <- read.csv(paste0(data_storage_path,"Datasets/species_assessment/checklists/verts_alpine_generalists.csv"))|>
  filter(group=="mammals")|>
  filter(Mountain_range=="Himalaya")

# mammal checklist for Himalaya
checklist <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_mammal_database.xlsx"))|>
  filter(Mountain_range=="Himalaya")


# Join with treeline elevations
Treeline_Elevations <- readxl::read_excel(file.path(data_storage_path, "Datasets/Mountains/Treeline_Lapse_Rate_04_05.xlsx"))|>
  select(Mountain_range,Mean_elevation_2_degree,Mean_elevation_4_degree,Mean_elevation_6_degree)


checklist_reduced <-checklist|>
  select(sciname,Mountain_range,min_elevation,max_elevation,mean_treeline)|>
  left_join(Treeline_Elevations,by="Mountain_range")

# leftjoin
assessment_himalaya <- checklist_reduced|>
  left_join(assessments_reduced,by="sciname")|>
  mutate(redlistCategory = coalesce(redlistCategory, "Not assessed"))|>
  mutate(populationTrend = coalesce(populationTrend,"Not assessed"))
  

#----------------------------------------------------------#
#        lowland vs generalists vs specialists
#----------------------------------------------------------#
assessment_lowland <- assessment_himalaya|>
  filter(max_elevation <= mean_treeline & min_elevation <= mean_treeline)|>
  mutate(Group = "Lowland")

assessment_generalists <- assessment_himalaya|>
  filter(max_elevation >= mean_treeline & min_elevation <= mean_treeline)|>
  mutate(Group = "Generalists")


assessment_specialists <- assessment_himalaya|>
  filter(max_elevation >= mean_treeline & min_elevation >= mean_treeline)|>
  mutate(Group = "Specialists")


assessment_bm_alp <- assessment_himalaya|>
  filter(max_elevation >= mean_treeline & min_elevation >= Mean_elevation_6_degree)|>
  anti_join(assessment_specialists, by = "sciname") |>
  mutate(Group = "Montane")



# dont do this for now: 
assessment_mm_alp <- assessment_himalaya|>
  filter(max_elevation >= mean_treeline & min_elevation >= Mean_elevation_4_degree)|>
  anti_join(assessment_specialists, by = "sciname") |>
  anti_join(assessment_UFL_alp, by = "sciname") |>
  mutate(Group = "mid_montane_alpine")

assessment_UFL_alp <- assessment_himalaya|>
  filter(max_elevation >= mean_treeline & min_elevation >= Mean_elevation_2_degree)|>
  anti_join(assessment_specialists, by = "sciname") |>
  mutate(Group = "UFL_alpine")


