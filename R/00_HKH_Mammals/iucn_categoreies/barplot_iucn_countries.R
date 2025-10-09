# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# join red list assesments -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(patchwork)

# load RL and HKH list
full_assessment_hkh_mammals <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

full_ass_work_data <- full_assessment_hkh_mammals|>
  filter(str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore")|>
  select(sciname,status_code_national,status_summary_national,year_assessed_national, year_assessed_global,status_code_global,countries_iso)

plot_output_path <- paste0(data_storage_path, "Visualizations/")
#----------------------------------------------------------#
# plot assesments numbers per category-----
#----------------------------------------------------------#

library(tidyverse)
library(tidyverse)

status_levels <- c("DD", "NE", "RE", "CR", "EN", "VU", "NT", "LC")

# National-level data (drop NA status codes here)
national_plot <- full_ass_work_data |>
  select(sciname, region = countries_iso, status_code = status_code_national) |>
  filter(!status_code %in% c("NA", NA))

# Global-level data
global_plot <- full_ass_work_data |>
  distinct(sciname, status_code_global) |>
  transmute(sciname, region = "Global", status_code = status_code_global)

# Combine
plot_data <- bind_rows(national_plot, global_plot) |>
  mutate(status_code = factor(status_code, levels = status_levels))

# Region ordering (Global last)
region_order <- plot_data |>
  group_by(region) |>
  summarise(total_species = n_distinct(sciname), .groups = "drop") |>
  arrange(desc(total_species)) |>
  pull(region) |>
  setdiff("Global") |>
  c(., "Global")

# Add Myanmar manually with empty values
plot_data <- plot_data |>
  mutate(region = factor(region, levels = region_order)) 

agg_plot <- plot_data |>
  group_by(region, status_code) |>
  summarise(species_count = n_distinct(sciname), .groups = "drop")

# Add an empty Myanmar row for plotting
agg_plot <- agg_plot |>
  bind_rows(
    tibble(region = "Myanmar", status_code = factor(status_levels, levels = status_levels), species_count = 0)
  )

# Make sure Myanmar is in the factor levels at the right place
agg_plot <- agg_plot |>
  mutate(region = factor(region, levels = c(setdiff(region_order, "Global"), "Myanmar", "Global")))

# Custom labels
region_labels <- setNames(as.list(levels(agg_plot$region)), levels(agg_plot$region))
region_labels[["Bhutan"]] <- "Bhutan\n(no national RL, assessed under South Asian plan)"
region_labels[["Afghanistan"]] <- "Afghanistan\n(no national RL, assessed under South Asian plan)"
region_labels[["Myanmar"]] <- "Myanmar\n(no national RL)"

# Plot
plot1 <- ggplot(agg_plot, aes(x = region, y = species_count, fill = status_code)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "inferno",na.value="lightgrey", direction = 1) +
  scale_x_discrete(labels = region_labels) +
  labs(
    x = NULL,
    y = "Number of species",
    fill = "Status Code"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1


#----------------------------------------------------------#
# plot assesments relative categories -----
#----------------------------------------------------------#
# Calculate proportions per region
rel_plot <- plot_data |>
  group_by(region, status_code) |>
  summarise(species_count = n_distinct(sciname), .groups = "drop") |>
  group_by(region) |>
  mutate(prop = species_count / sum(species_count)) |>
  ungroup()|>
  bind_rows(
    tibble(region = "Myanmar", status_code = factor(status_levels, levels = status_levels), species_count = 0)
  )|>
  mutate(region = factor(region, levels = c(setdiff(region_order, "Global"), "Myanmar", "Global")))


# Plot relative proportions
plot2<-ggplot(rel_plot, aes(x = region, y = prop, fill = status_code)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "inferno", na.value = "lightgrey", direction = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = region_labels) +
  labs(
    title = NULL,
    x = NULL,
    y = "Percentage of Species",
    fill = "Status Code"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Combine plots with a shared legend and title alignment
combined_plot <- plot1 + plot2 +
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom")

# Show the plot
print(combined_plot)

# Save the combined plot
# optional: save a high-res imageggsave
ggsave(
  filename = paste0(data_storage_path, "Datasets/species_list/species_richness/RL_assessments.png"),
  plot = combined_plot,
  width = 12,
  height = 6,
  dpi = 300
)
