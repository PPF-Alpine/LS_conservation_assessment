

cons_prio <- rast(paste0(data_storage_path, "Output/priority_indices/priority_mapp_all_combinations.tif"))
plot(cons_prio)

levels_df <- data.frame(
  value = c(111,110,121,120,131,130,
            211,210,221,220,231,230,
            311,310,321,320,331,330),
  biodiv = rep(c("low","medium","high"), each = 6),
  climate = rep(c("low","low","medium","medium","high","high"), times = 3),
  protection = rep(c("protected","unprotected"), 9)
)


library(terra)
library(dplyr)
library(ggplot2)

# extract raster values
df <- as.data.frame(cons_prio, na.rm = TRUE)
names(df) <- "value"

df <- df |>
  left_join(levels_df, by = "value")


df <- df |>
  mutate(
    category = case_when(
      biodiv == "high" & climate == "high"   ~ "High biodiv + High climate",
      biodiv == "high" & climate == "medium" ~ "High biodiv + Med climate",
      biodiv == "high" & climate == "low" ~ "High biodiv + Low climate",
      TRUE ~ "Other"
    )
  )

summary_df <- df |>
  group_by(category, protection) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(category) |>
  mutate(
    perc = n / sum(n) * 100
  ) |>
  ungroup()


protected_df <- summary_df |>
  filter(protection == "protected")

ggplot(protected_df, aes(x = category, y = perc)) +
  geom_point(size = 4, color = "darkgreen") +
  geom_segment(aes(xend = category, y = 0, yend = perc),
               color = "darkgreen") +
  labs(
    x = NULL,
    y = "% protected",
    title = "Protection across priority categories"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(tidyr)

# reshape
wide_df <- summary_df |>
  select(category, protection, perc) |>
  pivot_wider(names_from = protection, values_from = perc)



# set correct order
wide_df$category <- factor(
  wide_df$category,
  levels = c(
    "High biodiv + High climate",
    "High biodiv + Med climate",
    "High biodiv + Low climate",
    "Med biodiv + High climate",
    "Other"
  )
)

# convert to long for legend
plot_df <- wide_df |>
  tidyr::pivot_longer(
    cols = c(protected, unprotected),
    names_to = "protection",
    values_to = "perc"
  )


plot_protection <- ggplot() +
  # connecting lines (gap)
  geom_segment(
    data = wide_df,
    aes(x = category, xend = category,
        y = protected, yend = unprotected),
    color = "grey50"
  ) +
  
  # points WITH legend
  geom_point(
    data = plot_df,
    aes(x = category, y = perc, color = protection),
    size = 4
  ) +
  
  scale_color_manual(
    values = c(
      "protected" = "darkgreen",
      "unprotected" = "red"
    ),
    name = NULL
  ) +
  
  labs(
    x = NULL,
    y = "Percent of cells",
    title = NULL
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(
  filename = paste0(data_storage_path, "Output/priority_indices/protection_status.jpeg"),
  plot = plot_protection,
  width = 7,
  height = 5,
  dpi = 300
)
