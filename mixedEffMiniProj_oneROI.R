library(lme4)
library(ggplot2)

setwd("D:/Study/CON 2/Research/Connectivity")
data<-read.csv("fmri_features_ALFF_full_table.csv")


roi_data <- subset(
  data,
  roi == "17Networks_RH_SomMotA_4" &
    grepl("msl", session) &
    gsr == "no gsr"
)

# Linear regression model
linear_model <- lm(ALFF ~ sma.mrs, data = roi_data)

# Mixed model: fixed slope + random intercept
mixed_random_intercept <- lmer(
  ALFF ~ sma.mrs + (1 | subject),
  data = roi_data
)

# Mixed model: random intercept + random slope
mixed_random_slope <- lmer(
  ALFF ~ sma.mrs + (1 + sma.mrs | subject),
  data = roi_data
)

summary(linear_model)
summary(mixed_random_intercept)
summary(mixed_random_slope)

# Predictions
roi_data$ri_pred <- predict(mixed_random_intercept)
roi_data$fixed_pred_ri <- predict(mixed_random_intercept, re.form = NA)

# Plot
# ggplot(roi_data, aes(x = sma.mrs, y = ALFF, color = subject)) +
#   geom_point(size = 3) +
#   geom_line(aes(y = ri_pred, group = subject), linewidth = 1) +
#   geom_line(aes(y = fixed_pred_ri, group = 1), color = "black", linewidth = 1.3) +
#   labs(
#     title = "Mixed Model: Random Intercept",
#     x = "SMA MRS",
#     y = "ALFF"
#   ) +
#   theme_minimal()
# 
# # Predictions
# roi_data$rs_pred <- predict(mixed_random_slope)
# roi_data$fixed_pred_rs <- predict(mixed_random_slope, re.form = NA)
# 
# # Plot
# ggplot(roi_data, aes(x = sma.mrs, y = ALFF, color = subject)) +
#   geom_point(size = 3) +
#   geom_line(aes(y = rs_pred, group = subject), linewidth = 1) +
#   geom_line(aes(y = fixed_pred_rs, group = 1), color = "black", linewidth = 1.3) +
#   labs(
#     title = "Mixed Model: Random Intercept and Random Slope",
#     x = "SMA MRS",
#     y = "ALFF"
#   ) +
#   theme_minimal()


