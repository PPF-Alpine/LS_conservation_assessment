# flow diagramm
library(ggalluvial)

# Combine all datasets into one
assessment_combined <- bind_rows(assessment_generalists, 
                                 assessment_bm_alp,
                                 assessment_specialists)



# get tje proportions for the categories
assessment_proportions <- assessment_combined|>
  group_by(Group,redlistCategory)|>
  summarise(Count=n(), .groups="drop")|>
  mutate(Proportion =Count/sum(Count))|>
  mutate(redlistCategory = factor(redlistCategory, 
                                  levels = c("Critically Endangerned",
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


# Convert data for an alluvial plot
x11()
ggplot(assessment_proportions, 
       aes(axis1 = Group, axis2 = redlistCategory, y = Proportion)) +
  geom_alluvium(aes(fill = redlistCategory)) +
  geom_stratum(aes(fill = Group), alpha = 1) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Group", "Red List Category")) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "IUCN Status Himalayan Mammals",
    x = NULL, y = "Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Removes the legend





