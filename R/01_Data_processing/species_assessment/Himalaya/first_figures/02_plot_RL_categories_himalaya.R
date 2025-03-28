

#----------------------------------------------------------#
#       plot the data
#----------------------------------------------------------#
# Combine all datasets into one
assessment_combined <- bind_rows(assessment_generalists, 
                                 assessment_specialists, 
                                 assessment_lowland,
                                 assessment_bm_alp)

# get tje proportions for the categories
assessment_proportions <- assessment_combined|>
  group_by(Group,redlistCategory)|>
  summarise(Count=n(), .groups="drop")|>
  mutate(Proportion =Count/sum(Count))|>
  mutate(redlistCategory = factor(redlistCategory, 
                                  levels = c("Critically Endangered",
                                             "Endangered",
                                             "Vulnerable",
                                             "Near Threatened",
                                             "Least Concern", "Data Deficient", "Not assessed")))

# 
criteria_proportions <- assessment_combined|>
  group_by(Group,redlistCriteria)|>
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
                                   y = Proportion, fill = redlistCategory)) +
  geom_bar(stat = "identity", position = "fill") +  
  scale_y_continuous(labels = scales::percent_format()) +  
  labs(
    title = "IUCN Red List Categories",  
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
    "Data Deficient" = "#A9A9A9",     # Grey
    "Least Concern" = "gold",      # Light Orange
    "Near Threatened" = "orange2",    # Orange
    "Vulnerable" = "#FF4500",         # Orange-Red
    "Endangered" = "#DC143C",         # Crimson
    "Critically Endangered" = "red4"  # Dark Red
  )) +  
  
  # Add group counts 
  annotate("text", x = 1, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Lowland"]), size = 5) +
  annotate("text", x = 2, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Generalists"]), size = 5) +
  annotate("text", x = 4, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Specialists"]), size = 5) +
  annotate("text", x = 3, y = 1.05, label = paste0("n = ", group_counts$n[group_counts$Group == "Montane"]), size = 5) 
