
# get tje proportions for the categories
assessment_proportions <- assessment_combined|>
  group_by(Mountain_range,Group,redlistCategory)|>
  summarise(Count=n(), .groups="drop")|>
  mutate(Proportion =Count/sum(Count))|>
  mutate(redlistCategory = factor(redlistCategory, 
                                  levels = c("Critically Endangered",
                                             "Endangered",
                                             "Vulnerable",
                                             "Near Threatened",
                                             "Least Concern", 
                                             "Data Deficient", "Not assessed")))

#----------------------------------------------------------#
#        Filter by mountain range 
#----------------------------------------------------------#

# Define mountain selection
mountain_selection <- c("Himalaya", 
                        "Northern Andes", 
                        "Central Andes", 
                        "Central European Highlands", 
                        "Intermountain West",
                        "Hindu Kush", 
                        "Ethiopian Highlands", 
                        "Albertine Rift Mountain",
                        "South Island",
                        "North European Highlands",
                        "Tibetan Plateau",
                        "Great Escarpment",
                        "Malay Archipelago",
                        "Caucasus Mountains",
                        "East European Highlands",
                        "Rocky Mountains",
                        "Pacific Coast Ranges",
                        "Eastern Rift mountains",
                        "Mexican Highlands")

# Filter data for selected mountains
assessment_mountain <- assessment_combined |> 
  filter(Mountain_range %in% mountain_selection)

# Compute total count per group
group_counts <- assessment_combined |> 
  filter(Mountain_range %in% mountain_selection) |> 
  count(Mountain_range, Group, name = "Count")

#----------------------------------------------------------#
#        Plot for each mountain range 
#----------------------------------------------------------#


x11()
ggplot(assessment_proportions |> filter(Mountain_range %in% mountain_selection),
       aes(x = factor(Group, levels = c("Generalists", "Montane", "Specialists")),
           y = Proportion, fill = redlistCategory)) +
  geom_bar(stat = "identity", position = "fill",width=0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "IUCN Red List Categories by Mountain Range",
       x = NULL, 
       y = "Proportion",
       fill = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold")) +
  scale_fill_manual(values = c(
    "Not assessed" = "gray92",
    "Data Deficient" = "#A9A9A9",
    "Least Concern" = "gold",
    "Near Threatened" = "orange2",
    "Vulnerable" = "#FF4500",
    "Endangered" = "#DC143C",
    "Critically Endangered" = "red4"
  )) +
  facet_wrap(~ Mountain_range,nrow=6) 
