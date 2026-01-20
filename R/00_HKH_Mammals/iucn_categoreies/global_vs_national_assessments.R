# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# load red list assesments -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(patchwork)

# load RL and HKH list
full_assessment_hkh_mammals <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

full_ass_work_data <- full_assessment_hkh_mammals|>
  filter(str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore")|>
  select(sciname,status_code_national,countries_iso,status_summary_national,year_assessed_national, year_assessed_global,status_code_global)|>
  mutate(across(everything(), ~na_if(.x, "NA")))

#----------------------------------------------------------#
# add national global assessment -----
#----------------------------------------------------------#

rl_full <- full_ass_work_data |>
  mutate(
    global_assessed   = !is.na(status_code_global),
    national_assessed = !is.na(status_code_national),
    
    assessment_status = case_when(
      global_assessed & national_assessed ~ "Global + National",
      global_assessed & !national_assessed ~ "Global only",
      !global_assessed & national_assessed ~ "National only",
      !global_assessed & !national_assessed ~ "Not assessed"
    )
  )

rl_full_species <- rl_full |>
  group_by(sciname) |>
  summarise(
    global_assessed   = any(global_assessed),
    national_assessed = any(national_assessed),
    assessment_status = case_when(
      global_assessed & national_assessed ~ "Global + National",
      global_assessed & !national_assessed ~ "Global only",
      !global_assessed & national_assessed ~ "National only",
      TRUE ~ "Not assessed"
    ),
    .groups = "drop"
  )
table(rl_full_species$assessment_status)
prop.table(table(rl_full_species$assessment_status))


#----------------------------------------------------------#
# upgraded or downgraded ? -----
#----------------------------------------------------------#
rl_comp <- rl_full |>
  filter(assessment_status== "Global + National")


rl_levels <- c("LC"=1, "NT"=2, "VU"=3, "EN"=4, "CR"=5,"RE"=6)

rl_comp <- rl_comp |>
  mutate(
    rl_global_num   = rl_levels[status_code_global],
    rl_national_num = rl_levels[status_code_national]
  )


rl_comp <- rl_comp |>
  mutate(
    rl_change = case_when(
      
      # DD cases (explicit, non-ordinal)
      status_code_national == "DD" & status_code_global != "DD" ~ 
        "Data deficient nationally",
      
      status_code_global == "DD" & status_code_national != "DD" ~ 
        "Data deficient globally",
      
      status_code_global == "DD" & status_code_national == "DD" ~ 
        "Data deficient both",
      
      # Ordinal comparisons (only LC–CR on both sides)
      !is.na(rl_global_num) & !is.na(rl_national_num) & 
        rl_national_num > rl_global_num ~ 
        "Upgraded nationally",
      
      !is.na(rl_global_num) & !is.na(rl_national_num) & 
        rl_national_num < rl_global_num ~ 
        "Downgraded nationally",
      
      !is.na(rl_global_num) & !is.na(rl_national_num) & 
        rl_national_num == rl_global_num ~ 
        "Same category",
      
      # Catch remaining cases (e.g. NA assessments)
      TRUE ~ "Not comparable / not assessed"
    )
  )


#----------------------------------------------------------#
#   save
#----------------------------------------------------------#

write.csv(rl_comp,paste0(data_storage_path,"Datasets/species_list/national_global_threat_comparison.csv"))



#----------------------------------------------------------------------------------------#
# plot upgrade and downgrade of those that have been assessed nationally and globally -----
#-------------------------------------------------------------------------------------------#

plot_df <- rl_comp |>
  filter(countries_iso != "NA") |>   # keep real countries
  group_by(countries_iso, rl_change) |>
  summarise(n = n(), .groups = "drop")

# add proportions
plot_df <- plot_df |>
  group_by(countries_iso) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

# as factors
plot_df$rl_change <- factor(
  plot_df$rl_change,
  levels = c(
    "Upgraded nationally",
    "Same category",
    "Downgraded nationally",
    "Data deficient nationally",
    "Data deficient globally",
    "Data deficient both",
    "Not comparable / not assessed"
  )
)

# plot rl change all 
ggplot(plot_df, aes(x = countries_iso, y = prop, fill = rl_change)) +
  geom_col(color = "black", width = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Country",
    y = "Proportion of species",
    fill = "National vs global status"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

# plot only up or downgrade
plot_simple <- rl_comp |>
  filter(rl_change %in% c("Upgraded nationally", "Downgraded nationally")) |>
  group_by(countries_iso, rl_change) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(countries_iso) |>
  mutate(prop = n / sum(n)) |>
  ungroup()


ggplot(plot_simple, aes(x = countries_iso, y = prop, fill = rl_change)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Country",
    y = "Proportion of assessed species",
    fill = "Directional change"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
