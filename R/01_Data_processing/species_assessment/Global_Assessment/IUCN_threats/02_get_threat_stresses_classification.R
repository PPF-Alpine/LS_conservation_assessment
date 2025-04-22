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
#        read in the threats
#----------------------------------------------------------#

assessment_threats_complete <- read.csv(paste0(data_storage_path,"Outputs/IUCN_assessment_lists/assessment_threats_complete.csv"))|>
  distinct(sciname, .keep_all = TRUE)


# count if there are NA for threats for species that are listed as threathened 
num_na <- assessment_threats_complete |>
  filter(red_list_code != "LC", is.na(threats_code)) |>
  nrow()
  
count_na <- assessment_threats_complete |>
  filter(red_list_code != "LC", is.na(threats_code)) 

# all species that are not Least Concern, Near Threathened, or Data Deficient have a threat assigned 

# Sum up all duplicates (even if one row is repeated multiple times)
total_rows <- assessment_threats_complete |>
  filter(red_list_code != "LC", is.na(threats_code)) |>
  count(across(everything())) |>
  summarise(total = sum(n)) 

dupes <- duplicated(assessment_threats_complete)

#----------------------------------------------------------#
# Get IUCN threats and stresses description and classification
#----------------------------------------------------------#

threat_classification <- rl_threats(key=iucn_key)

threat_classification_codes <- as.data.frame(threat_classification$threats)

threat_classification_codes <- threat_classification$threats|>
  filter(code %in% 1:12)|>
  mutate(category="threat")

str(threat_classification$threats, max.level = 2)

# get a stresses description df

stress_classification <- rl_stresses(key=iucn_key)

stress_classification_codes <- stress_classification$stresses |>
  filter(code %in% c("1_1", "1_2", "1_3", "2_1", "2_2", "2_3")) |>
  mutate(code = str_replace_all(code, "_", "."))|>
  mutate(category="stresses")

# Combine both into one data frame
classification_code_description <- bind_rows(threat_classification_codes, stress_classification_codes)


#----------------------------------------------------------#
#         Load and prepare species data 
#----------------------------------------------------------#
# mammal checklist 
checklist_mammals <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_mammal_database.xlsx"))|>
  mutate(tax_group = "mammals")

# birds checklist 
checklist_birds <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_bird_database.xlsx"))|>
  mutate(tax_group = "birds")

# reptiles checklist 
checklist_reptiles <- readxl::read_xlsx(paste0(data_storage_path,"Datasets/species_assessment/checklists/alpine_reptile_database.xlsx"))|>
  mutate(tax_group = "reptiles")

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
  select(tax_group,source,sciname,Mountain_range,min_elevation,max_elevation,mean_treeline)|>
  left_join(Treeline_Elevations,by="Mountain_range")|>
  filter(max_elevation >= mean_treeline)|>
  mutate(range_size = max_elevation - min_elevation)|>
  mutate(mid_elevation = (max_elevation + min_elevation)/2)

#----------------------------------------------------------#
#        mutate lowland vs generalists vs specialists
#----------------------------------------------------------#

checklist_reduced <- checklist_reduced |>
  group_by(Mountain_range) |>
  mutate(
    alpine_category = case_when(
      
      
      # Specialists: entirely above the treeline
      max_elevation >= mean_treeline & min_elevation >= mean_treeline ~ "alpine specialists",
      
      # Montane: entirely below treeline and above a lower elevation threshold,
      # but NOT specialists (they’re already handled above)
      max_elevation >= mean_treeline & min_elevation >= Mean_elevation_6_degree ~ "alpine montane",
      
      # Generalists:  across the treeline
      max_elevation >= mean_treeline & min_elevation <= mean_treeline ~ "alpine generalists",
      
      TRUE ~ "Other"
    )
  )


#----------------------------------------------------------#
#         left join assessments with species
#----------------------------------------------------------#

threats_stresses_species <- checklist_reduced|>
  select(tax_group,source,sciname,Mountain_range,min_elevation,max_elevation,range_size,mid_elevation,alpine_category,mean_treeline)|>
  left_join(assessment_threats_complete,by="sciname")


threats_stresses_species <- threats_stresses_species|>
  mutate(threats_broad = threats_code |>
           str_replace_all("_", ".") |>                   # Replace underscores with dots
           str_split(";\\s*") |>                          # Split into list by ";"
           lapply(function(x) str_extract(x, "^\\d+")) |> # Extract first number from each part
           lapply(unique) |>                              # Keep unique top-level threats
           sapply(paste, collapse = "; ")                 # Collapse back into a single string
  ) |>
  mutate(stresses_broad = stresses_code |>
           str_replace_all("_", ".") |>                            # Normalize to dots
           str_split(";\\s*") |>                                   # Split by ;
           lapply(function(x) str_extract(x, "^\\d+\\.\\d+")) |>   # Extract first two levels (e.g., 2.3)
           lapply(unique) |>                                       # Keep unique values
           sapply(paste, collapse = "; ")                          # Collapse back into one string
  )|>
  mutate(red_list_code = coalesce(red_list_code, "not assessed"))


#----------------------------------------------------------#
#        mutate elevational belts 
#----------------------------------------------------------#

# calculate the max mid_elevation per mountain range
elev_band_max <- threats_stresses_species |>
  group_by(Mountain_range) |>
  summarise(max_elev = max(mid_elevation, na.rm = TRUE))

#  max elevation to the main data
threats_stresses_species <- threats_stresses_species |>
  left_join(elev_band_max, by = "Mountain_range") |>
  rowwise() |>
  mutate(
    # Create breaks from 0 to max mid elevation + 500, by 500
    elevation_band = cut(
      mid_elevation,
      breaks = seq(0, max_elev + 500, by = 500),
      include.lowest = TRUE,
      right = FALSE,  # optional: makes intervals [0–500), [500–1000), etc.
      labels = paste0(head(seq(0, max_elev + 500, by = 500), -1), "–", tail(seq(0, max_elev + 500, by = 500), -1))
    )
  ) |>
  ungroup() |>
  select(-max_elev)  

#----------------------------------------------------------#
#      write as csv
#----------------------------------------------------------#

write.csv(threats_stresses_species,paste0(data_storage_path,"Outputs/IUCN_assessment_lists/threats_stresses_species.csv"))

# save to CSV
write_csv(classification_code_description, paste0(data_storage_path, "Outputs/IUCN_classification_codes.csv"))