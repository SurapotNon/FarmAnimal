# Evaluate the model performance

# Hierarchical Linear Models (HLM)
fit_hlm <- lmer(Weight ~ AgeMonths + Sire + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate + (1 | No.), data = combined_lamb_removemissing)

# Linear Mixed-Effects Model
fit_lmm <- lmer(Weight ~ AgeMonths + Sire + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate + (1 + AgeMonths | No.), data = combined_lamb_removemissing)

# Growth curve model
fit_gcm_poly3 <- lmer(Weight ~ AgeMonths + poly(AgeMonths, 3) + (AgeMonths | No.), data = combined_lamb_removemissing)

# GEE Model
gee_model <- geeglm(Weight ~ AgeMonths + Sex + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate, 
                    id = BirthType, 
                    data = combined_lamb_removemissing, 
                    family = gaussian(), 
                    corstr = "exchangeable")

# Auto regressive model
fit_ar3 <- lme(
  Weight ~ AgeMonths + Sire + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate, 
  random = ~ 1 | No., 
  correlation = corCompSymm(form = ~ AgeMonths | No.), 
  data = combined_lamb_removemissing
)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check for and handle missing values
combined_lamb_removemissing <- na.omit(combined_lamb)

# Function to drop unused factor levels
drop_unused_levels <- function(df) {
  df[] <- lapply(df, function(x) if (is.factor(x)) factor(x) else x)
  return(df)
}

# Perform cross-validation
set.seed(132)  # For reproducibility

# Number of iterations for cross-validation
iterations <- 100

# Initialize a matrix to store the performance metrics for each model
performance_metrics <- matrix(NA, nrow = iterations, ncol = 5)
colnames(performance_metrics) <- c("HLM", "LMM", "GCM", "GEE", "AR")

# Control parameters for lmer
lmer_control <- lmerControl(optCtrl = list(maxfun = 20000), check.conv.grad = .makeCC("warning", tol = 0.002))

# Perform cross-validation
for (iteration in 1:iterations) {
  # Split data 80/20 without replacement
  set.seed(iteration)
  idx <- sample(seq_len(nrow(combined_lamb_removemissing)), size = 0.8 * nrow(combined_lamb_removemissing))
  train_data <- combined_lamb_removemissing[idx, ]
  test_data <- combined_lamb_removemissing[-idx, ]
  
  # Drop unused levels in factors for both training and test data
  train_data <- drop_unused_levels(train_data)
  test_data <- drop_unused_levels(test_data)
  
  # Fit the HLM model
  fit_hlm <- lmer(Weight ~ AgeMonths + Sire + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate + (1 | No.), 
                  data = train_data, control = lmer_control)
  predictions_hlm <- tryCatch({
    predict(fit_hlm, newdata = test_data, allow.new.levels = TRUE)
  }, error = function(e) rep(NA, nrow(test_data)))
  mse_hlm <- mean((test_data$Weight - predictions_hlm)^2, na.rm = TRUE)
  performance_metrics[iteration, "HLM"] <- mse_hlm
  
  # Fit the LMM model
  fit_lmm <- lmer(Weight ~ AgeMonths + Sire + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate + (1 + AgeMonths | No.), 
                  data = train_data, control = lmer_control)
  predictions_lmm <- tryCatch({
    predict(fit_lmm, newdata = test_data, allow.new.levels = TRUE)
  }, error = function(e) rep(NA, nrow(test_data)))
  mse_lmm <- mean((test_data$Weight - predictions_lmm)^2, na.rm = TRUE)
  performance_metrics[iteration, "LMM"] <- mse_lmm
  
  # Fit the GCM model
  fit_gcm_poly3 <- lmer(Weight ~ AgeMonths + poly(AgeMonths, 3) + (AgeMonths | No.), 
                        data = train_data, control = lmer_control)
  predictions_gcm_poly3 <- tryCatch({
    predict(fit_gcm_poly3, newdata = test_data, allow.new.levels = TRUE)
  }, error = function(e) rep(NA, nrow(test_data)))
  mse_gcm_poly3 <- mean((test_data$Weight - predictions_gcm_poly3)^2, na.rm = TRUE)
  performance_metrics[iteration, "GCM"] <- mse_gcm_poly3
  
  # Fit the GEE model
  gee_model <- geeglm(Weight ~ AgeMonths + Sex + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate, 
                      id = BirthType, 
                      data = train_data, 
                      family = gaussian(), 
                      corstr = "exchangeable")
  predictions_gee <- tryCatch({
    predict(gee_model, newdata = test_data, type = "response")
  }, error = function(e) rep(NA, nrow(test_data)))
  mse_gee <- mean((test_data$Weight - predictions_gee)^2, na.rm = TRUE)
  performance_metrics[iteration, "GEE"] <- mse_gee
  
  # Fit the AR model
  fit_ar3 <- lme(
    Weight ~ AgeMonths + Sire + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate, 
    random = ~ 1 | No., 
    correlation = corCompSymm(form = ~ AgeMonths | No.), 
    data = train_data
  )
  predictions_ar3 <- tryCatch({
    predict(fit_ar3, newdata = test_data, level = 0)
  }, error = function(e) rep(NA, nrow(test_data)))
  mse_ar3 <- mean((test_data$Weight - predictions_ar3)^2, na.rm = TRUE)
  performance_metrics[iteration, "AR"] <- mse_ar3
}

# Convert the performance metrics to a data frame for plotting
performance_metrics_df <- as.data.frame(performance_metrics)

# Reshape the data frame for plotting
performance_metrics_long <- melt(performance_metrics_df, variable.name = "Model", value.name = "MSE")
performance_metrics_long$Iteration <- rep(1:iterations, times = 5)

# Filter out NA values
performance_metrics_long <- na.omit(performance_metrics_long)

# Boxplot
ggplot(performance_metrics_long, aes(x = Model, y = MSE)) +
  geom_boxplot() +
  labs(title = "Model Performance Comparison", x = "Model", y = "Mean Squared Error (MSE)") +
  theme_minimal()

# Line graph
ggplot(performance_metrics_long, aes(x = Iteration, y = MSE, color = Model, group = Model)) +
  geom_line(alpha = 0.6) +
  labs(title = "Model Performance Across Iterations", x = "Iteration", y = "Mean Squared Error (MSE)") +
  theme_minimal()

# Summary of performance metrics to investigate potential issues
summary(performance_metrics_df)

# Determine the best model based on the lowest mean MSE
mean_performance <- colMeans(performance_metrics_df, na.rm = TRUE)
best_model <- names(which.min(mean_performance))

# Print the best model
print(paste("The best model is:", best_model))
 