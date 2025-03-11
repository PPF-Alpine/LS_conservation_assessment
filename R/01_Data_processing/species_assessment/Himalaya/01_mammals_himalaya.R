

#----------------------------------------------------------#
#         Load and arrange data 
#----------------------------------------------------------#

# IUCN assessment
assessments <- read.csv(paste0(data_storage_path,"species_assessment/IUCN/mammals/assessments.csv"))

assessments_reduced <- assessments|>
  select(scientificName,redlistCategory,redlistCriteria,
         yearPublished,rationale,habitat,threats,
         population,populationTrend,range,conservationActions,possiblyExtinct,possiblyExtinctInTheWild)|>
  rename(sciname=scientificName)

# mammal checklist for Himalaya
checklist <- read.csv(paste0(data_storage_path,"species_assessment/checklists/verts_alpine_generalists.csv"))|>
  filter(group=="mammals")|>
  filter(Mountain_range=="Himalaya")

# mammal checklist for Himalaya
checklist <- readxl::read_xlsx(paste0(data_storage_path,"species_assessment/checklists/alpine_mammal_database.xlsx"))|>
  filter(Mountain_range=="Himalaya")


checklist_reduced <-checklist|>
  select(sciname,Mountain_range,min_elevation,max_elevation,mean_treeline)

# leftjoin
assessment_himalaya <- checklist_reduced|>
  left_join(assessments_reduced,by="sciname")|>
  mutate(redlistCategory = coalesce(redlistCategory, "Not assessed"))
  

#----------------------------------------------------------#
#        lowland vs generalists vs specialists
#----------------------------------------------------------#

assessment_generalists <- assessment_himalaya|>
  filter(max_elevation >= mean_treeline & min_elevation <= mean_treeline)|>
  mutate(Group = "Generalists")


assessment_specialists <- assessment_himalaya|>
  filter(max_elevation >= mean_treeline & min_elevation >= mean_treeline)|>
  mutate(Group = "Specialists")

assessment_lowland <- assessment_himalaya|>
  filter(max_elevation <= mean_treeline & min_elevation <= mean_treeline)|>
  mutate(Group = "Lowland")


# Combine all datasets into one
assessment_combined <- bind_rows(assessment_generalists, assessment_specialists, assessment_lowland)

# get tje proportions for the categories
assessment_proportions <- assessment_combined|>
  group_by(Group,redlistCategory)|>
  summarise(Count=n(), .groups="drop")|>
  mutate(Proportion =Count/sum(Count))

# 
criteria_proportions <- assessment_combined|>
  group_by(Group,redlistCriteria)|>
  summarise(Count=n(), .groups="drop")|>
  mutate(Proportion =Count/sum(Count))

#----------------------------------------------------------#
#       plot the data
#----------------------------------------------------------#

# Compute total count per group
group_counts <- assessment_proportions |> 
  group_by(Group) |> 
  summarise(n = sum(Count), .groups = "drop")  

# Create ggplot
ggplot(assessment_proportions, aes(x = factor(Group, levels = c("Lowland", "Generalists", "Specialists")), 
                                   y = Proportion, fill = redlistCategory)) +
  geom_bar(stat = "identity", position = "fill") +  
  scale_y_continuous(labels = scales::percent_format()) +  
  labs(
    title = "Himalayan Mammals Red List Categories",  
    x = NULL,  # Removes x-axis title
    y = "Proportion",
    fill = NULL  # Removes legend title
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set3") +  
  
  # Add group counts 
  annotate("text", x = 1, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Lowland"]), size = 5) +
  annotate("text", x = 2, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Generalists"]), size = 5) +
  annotate("text", x = 3, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Specialists"]), size = 5)

