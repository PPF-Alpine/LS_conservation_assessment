

# Data preparation
assessment_mountain <- assessment_mountain %>%
  mutate(assessment_summary = factor(
    assessment_summary,
    levels = c("DD/NA", "Least Concern", "Threathened")
  ))

mountain <- assessment_mountain|>
  filter(Mountain_range=="Northern Andes")


#----------------------------------------------------------#
#       linear model - but this predicts the range size by IUCN STATUS
#----------------------------------------------------------#

# relationship range size and midpoint
lm_range <- lm(assessment_summary ~ range_size, data = assessment_mountain)
summary(lm_range)
anova(lm_range)



lm_midpoint <- lm(mid_elevation ~ assessment_summary, data = assessment_mountain)
summary(lm_midpoint)
anova(lm_midpoint)

ggplot(assessment_mountain, aes(x = assessment_summary, y = range_size, fill = assessment_summary)) +
  geom_boxplot() +
  labs(
    title = "Range Size by Assessment Category",
    x = "Assessment Summary",
    y = "Range Size"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#----------------------------------------------------------#
# can we explain IUCN category by their elev range size
#----------------------------------------------------------#

assessment_mountain <- assessment_mountain %>%
  mutate(assessment_binary = if_else(assessment_summary == "Threathened", 1, 0))

# Fit logistic regression model
model_logit <- glm(assessment_binary ~ range_size, data = assessment_mountain, family = binomial)

summary(model_logit)


# rescale the coefficient: how much decrease the likelyhood of being threathed by 500 m increase in range size
rescale_coef <- exp(coef(model_logit)["range_size"] * 500)

# 500 m increase in range size --> probability of being threathened decrease by 11% 
(1-rescale_coef)*100


# Create a new data frame with a sequence of range_size values
range_vals <- tibble(range_size = seq(
  min(assessment_mountain$range_size, na.rm = TRUE),
  max(assessment_mountain$range_size, na.rm = TRUE),
  length.out = 300
))

# Predict probabilities using the logistic model
range_vals <- range_vals %>%
  mutate(predicted_prob = predict(model_logit, newdata = ., type = "response"))

# Plot predicted probability of being Threathened
ggplot(range_vals, aes(x = range_size, y = predicted_prob)) +
  geom_line(size = 1.2, color = "darkred") +
  labs(
    title = "predicted probability of being 'threathened' by range size",
    x = "Range Size",
    y = "Probability of Being Threathened"
  ) +
  theme_minimal()
#----------------------------------------------------------#
# can we explain IUCN category by their elev midpoint
#----------------------------------------------------------#


# Fit logistic regression model
model_logit_midpoint <- glm(assessment_binary ~ mid_elevation, data = assessment_mountain, family = binomial)

summary(model_logit_midpoint)


# rescale the coefficient: how much decrease the likelyhood of being threathed by 500 m increase in range size
rescale_coef <- exp(coef(model_logit_midpoint)["mid_elevation"] * 500)

# 500 m increase in mid elevation --> probability of being threathened dincrease
(rescale_coef - 1) * 100 




# Create a new data frame with a sequence of mid_elevation values
range_vals_midpoint <- tibble(mid_elevation = seq(
  min(assessment_mountain$mid_elevation, na.rm = TRUE),
  max(assessment_mountain$mid_elevation, na.rm = TRUE),
  length.out = 300
))

# Predict probabilities using the logistic model
range_vals_midpoint <- range_vals_midpoint %>%
  mutate(predicted_prob = predict(model_logit_midpoint, newdata = ., type = "response"))

# Plot predicted probability of being Threathened
ggplot(range_vals_midpoint, aes(x = mid_elevation, y = predicted_prob)) +
  geom_line(size = 1.2, color = "darkred") +
  labs(
    title = "predicted probability of being 'threathened' by range size",
    x = "Midpoint elevation",
    y = "Probability of Being Threathened"
  ) +
  theme_minimal()


