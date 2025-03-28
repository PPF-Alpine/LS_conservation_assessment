library(ggplot2)
library(treemapify)


# Combine all datasets into one
assessment_combined <- bind_rows(assessment_generalists, 
                                 assessment_bm_alp,
                                 assessment_specialists
)

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



ggplot(assessment_proportions, aes(area = Proportion, fill = redlistCategory, label = Group)) +
  geom_treemap() +
  geom_treemap_text(grow = TRUE, colour = "black", place = "centre") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(title = "Himalayan Mammal Conservation Status")
