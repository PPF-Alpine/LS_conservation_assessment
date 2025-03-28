# flow diagramm
library(ggalluvial)

# Combine all datasets into one
assessment_combined <- bind_rows(assessment_lowland,
                                 assessment_generalists, 
                                 assessment_bm_alp,
                                 assessment_specialists
)

# get tje proportions for the categories
assessment_proportions <- assessment_combined|>
  group_by(Group,populationTrend)|>
  summarise(Count=n(), .groups="drop")|>
  mutate(Proportion =Count/sum(Count))|>
  mutate(populationTrend = factor(populationTrend, 
                                  levels = c("Increasing",
                                             "Stable",
                                             "Decreasing",
                                             "Unknown",
                                             "Not assessed")))

# 
criteria_proportions <- assessment_combined|>
  group_by(Group,populationTrend)|>
  summarise(Count=n(), .groups="drop")|>
  mutate(Proportion =Count/sum(Count))


# Compute total count per group
group_counts <- assessment_proportions |> 
  group_by(Group) |> 
  summarise(n = sum(Count), .groups = "drop")  



# Create ggplot
ggplot(assessment_proportions, aes(x = factor(Group, 
                                              levels = c("Lowland",
                                                         "Generalists",
                                                         "Montane",
                                                         "Specialists")), 
                                   y = Proportion, fill = populationTrend)) +
  geom_bar(stat = "identity", position = "fill") +  
  scale_y_continuous(labels = scales::percent_format()) +  
  labs(
    title = "IUCN population trends",  
    x = NULL,  # Removes x-axis title
    y = "Proportion",
    fill = NULL  # Removes legend title
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  # Custom color scale
  scale_fill_manual(values = c(
    "Not assessed" = "gray92",       # White
    "Stable" = "yellow3",     # Grey
    "Increasing" = "springgreen4",      # Light Orange
    "Unknown" = "gray60",         # Orange-Red
    "Decreasing" = "red3"  # Dark Red
  )) +  
  
  # Add group counts 
  annotate("text", x = 1, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Lowland"]), size = 5) +
  annotate("text", x = 2, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Generalists"]), size = 5) +
  annotate("text", x = 4, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Specialists"]), size = 5) +
  annotate("text", x = 3, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Montane"]), size = 5) 

