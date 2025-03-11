
#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)

# Load configuration file
source(here::here("R/00_Config_file.R"))
# test text mining cloud for threats

library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)



assessment_generalists_filter <- assessment_generalists|>
  filter(!(redlistCategory=="Least Concern"))|>
  mutate(threats = coalesce(threats, "unknown"))|>
  select(sciname,threats)

writexl::write_xlsx(assessment_generalists_filter,paste0(data_storage_path,"species_assessment/himalaya_mammals.xlsx"))

#----------------------------------------------------------#
# chatgpt cleaning and summarizing test
#----------------------------------------------------------#
# 
generalist_threats<- readxl::read_xlsx(paste0(data_storage_path,"species_assessment/himalaya_mammals_processed_new.xlsx"))


generalist_threats_long <- generalist_threats |> 
  mutate(threats_summary = strsplit(threats_summary, "; ")) |> 
  unnest(threats_summary)


# Count the occurrences of each threat
threat_counts <- generalist_threats_long |> 
  count(threats_summary, sort = TRUE)

#----------------------------------------------------------#
# Plot most common threats
#----------------------------------------------------------#

# Create the bar plot
ggplot(threat_counts, aes(x = reorder(threats_summary, n), y = n, fill = threats_summary)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip for better readability
  labs(
    title = "Threats (ref IUCN) to Himalayan mammals (generalists)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "none"  # Remove legend since it's redundant
  ) +
  scale_fill_brewer(palette = "Set3")  # Nice color palette



