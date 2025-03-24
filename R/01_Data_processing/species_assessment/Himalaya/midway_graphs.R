library(ggplot2)
library(ggalluvial)


# Combine all datasets into one
assessment_combined <- bind_rows(assessment_generalists, 
                                 assessment_bm_alp,
                                 assessment_specialists)

# Filter only species in certain IUCN categories
assessment_combined <- assessment_combined |> 
  filter(redlistCategory %in% c("Endangered", 
                                "Vulnerable", 
                                "Near Threatened", 
                                "Least Concern"))



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



ggplot(assessment_proportions, 
       aes(axis1 = Group, axis2 = redlistCategory, y = Proportion)) +
  
  # Flowing connections
  geom_alluvium(aes(fill = redlistCategory)) +
  
  # Left bars (Group) - Assign different grey tones
  geom_stratum(aes(fill = Group), alpha = 1,fill="lightgrey") +
  
  # Right bars (Red List Categories) - Match their respective colors
  geom_stratum(aes(fill = redlistCategory), alpha = 1) +
  
  # Add text labels on stratum
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  
  # Define order of axes
  scale_x_discrete(limits = c("Group", "Red List Category")) +
  
  # Custom color scheme
  scale_fill_manual(values = c(
    # Right bars (Red List Categories) - Match category colors
    "Least Concern" = "#FFD700",      # Light Orange
    "Near Threatened" = "#FFA500",    # Orange
    "Vulnerable" = "#FF4500",         # Orange-Red
    "Endangered" = "#DC143C"
  )) +
  
  labs(title = "IUCN Red List Categories",
    x = NULL, y = "Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



library(scales)  # Load scales package for percentage formatting

ggplot(assessment_proportions, 
       aes(axis1 = Group, axis2 = redlistCategory, y = Proportion)) +
  
  # Flowing connections
  geom_alluvium(aes(fill = redlistCategory)) +
  
  # Left bars (Group) - Assign different grey tones
  geom_stratum(aes(fill = Group), alpha = 1, fill = "lightgrey") +
  
  # Right bars (Red List Categories) - Match their respective colors
  geom_stratum(aes(fill = redlistCategory), alpha = 1) +
  
  # Add text labels on stratum
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  
  # Define order of axes
  scale_x_discrete(limits = c("Group", "Red List Category")) +
  
  # Format y-axis labels as percentages
  scale_y_continuous(labels = percent_format()) +
  
  # Custom color scheme
  scale_fill_manual(values = c(
    # Right bars (Red List Categories) - Match category colors
    "Least Concern" = "#FFD700",      # Light Orange
    "Near Threatened" = "#FFA500",    # Orange
    "Vulnerable" = "#FF4500",         # Orange-Red
    "Endangered" = "#DC143C"
  )) +
  
  labs(title = "IUCN Red List Categories",
       x = NULL, y = "Proportion") +
  
  theme_minimal() +
  theme(legend.position = "none")  # Removes legend



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
                                  levels = c("Critically Endangered",
                                             "Endangered",
                                             "Vulnerable",
                                             "Near Threatened",
                                             "Least Concern", "Data Deficient", "Not assessed")))



# Reclassify Red List categories into 3 broader categories
assessment_summary <- assessment_proportions |>
  mutate(assessmentGroup = case_when(
    redlistCategory %in% c("Least Concern", "Near Threatened", "Vulnerable", "Endangered") ~ "Assessed by IUCN",
    redlistCategory == "Data Deficient" ~ "Data Deficient",
    redlistCategory == "Not assessed" ~ "Not Assessed"
  )) |>
  group_by(Group, assessmentGroup) |>
  summarise(Proportion = sum(Proportion), .groups = "drop")

# Create bar plot
ggplot(assessment_summary, aes(x = Group, y = Proportion, fill = assessmentGroup)) +
  geom_bar(stat = "identity", position = "fill") +  
  scale_y_continuous(labels = scales::percent_format()) +  
  labs(
    title = "Proportion of assessed vs. unassessed species",  
    x = NULL,  
    y = "Proportion",
    fill = NULL  
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  # Custom color scale for summary categories
  scale_fill_manual(values = c(
    "Assessed by IUCN" = "#FF4500",          
    "Data Deficient" = "#A9A9A9",    
    "Not Assessed" = "gray92"       
  ))
