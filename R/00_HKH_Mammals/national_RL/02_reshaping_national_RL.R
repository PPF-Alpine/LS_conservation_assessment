
# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# set up-----
#----------------------------------------------------------#

library(tidyverse)
library(rgbif)
library(taxize)

RL_national <- read.csv(paste0(data_storage_path,"RL_assessments/national_assessment_webscraping_output.csv"))|>
  janitor::clean_names()

#----------------------------------------------------------#
# reshape data  -----
#----------------------------------------------------------#

RL_national <- RL_national|>
  mutate(
    genus = word(species_or_taxon, 1),
    species = word(species_or_taxon, 2),
    sciname = paste(genus, species),
    status_code = str_extract(status, "[A-Z]{2}(?=[^A-Za-z]*$)"))|> # clean characters
  select(sciname,
         genus,species,
         species_or_taxon,
         common_names,
         year_assessed,
         countries_iso,
         locality,
         publication,
         criteria_system,
         status,status_code,publication_citation)|>
  mutate(
    status_summary = case_when( # add a summary column for IUCN status
      status_code %in% c("CR", "EN", "VU", "NT", "LE", "RE") ~ "threatened",
      status_code == "NE" ~ "not evaluated",
      status_code == "LC" ~ "not threatened",
      status_code == "DD" ~ "data deficient",
      TRUE ~ "NA"  
    )
  )

#----------------------------------------------------------#
# get the higher taxonomy for the species--
#----------------------------------------------------------#

# Extract the vector of scientific names
scinames <- RL_national|>
  distinct(sciname)|>
  pull(sciname)

# get classifications with taxize
classifications <- classification(scinames, db = "itis")

# flatten to df
classification_df <- map_dfr(
  names(classifications),
  function(sciname) {
    entry <- classifications[[sciname]]
    if (is.data.frame(entry)) {
      entry$sciname <- sciname  # add sciname name as a column
      return(entry)
    } else {
      return(NULL)  
    }
  }
)

# pivot
classification_df_wide <- classification_df %>%
  select(sciname, rank, name) %>%
  pivot_wider(names_from = rank, values_from = name)|>
  select(sciname,kingdom,phylum,class,order,family,genus)

# left join
RL_national_join <- RL_national|>
  left_join(classification_df_wide,by= c("sciname","genus"))

# select relevant columns and bring in order
RL_national_reshape <- RL_national_join|>
  select(kingdom,
         phylum,class,order,family,genus,species,sciname,species_or_taxon,common_names,year_assessed,countries_iso,locality,publication,criteria_system,status,status_code,status_summary,publication_citation)


#----------------------------------------------------------#
#   save
#----------------------------------------------------------#

write.csv(RL_national_reshape,paste0(data_storage_path,"RL_assessments/national_IUCN_RL_assessments_23052025.csv"))