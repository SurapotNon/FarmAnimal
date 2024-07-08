# 7. fit the Hierarchical Linear Models (HLM)

combined_lamb_removemissing$DW = as.numeric(combined_lamb_removemissing$DW)
combined_lamb_removemissing$DurationBeforeSlaughter = as.numeric(combined_lamb_removemissing$DurationBeforeSlaughter)
combined_lamb_removemissing$MotherAgeOnBornDate = as.numeric(combined_lamb_removemissing$MotherAgeOnBornDate)

fit_hlm <- lmer(Weight ~ AgeMonths + Sire + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate + (1 | No.), data = combined_lamb_removemissing)
summary(fit_hlm)

#--------------------------------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(lme4)
library(ggplot2)

# Perform cross-validation
set.seed(123)  # For reproducibility

# Number of iterations for cross-validation
iterations <- 100

# Initialize a vector to store the winning model for each iteration
winner <- rep(NA, iterations)

# Define a list of clustering effect variables to test
clustering_vars <- c("Sex", "type", "BirthType", "Dam", "Sire","No.")

# Perform cross-validation
for (iteration in 1:iterations) {
  # Split data 80/20 without replacement
  set.seed(iteration)
  idx <- sample(seq_len(nrow(combined_lamb_removemissing)), size = 0.8 * nrow(combined_lamb_removemissing))
  train_data <- combined_lamb_removemissing[idx, ]
  test_data <- combined_lamb_removemissing[-idx, ]
  
  # Ensure all factor levels in test data match those in train data
  for (var in clustering_vars) {
    if (is.factor(train_data[[var]])) {
      test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
    }
  }
  
  # Initialize a vector to store the performance metric for each model
  performance_metrics <- rep(NA, length(clustering_vars))
  
  # Loop through the clustering effect variables
  for (i in seq_along(clustering_vars)) {
    clustering_var <- clustering_vars[i]
    
    # Remove the clustering variable from the fixed effects if it is the same as the random effect
    fixed_effects <- setdiff(c("AgeMonths", "Sire", "type", "BirthType", "DW", "DurationBeforeSlaughter", "MotherAgeOnBornDate"), clustering_var)
    formula <- as.formula(paste("Weight ~", paste(fixed_effects, collapse = " + "), "+ (1 |", clustering_var, ")"))
    
    gc_model <- tryCatch({
      lmer(formula, data = train_data)
    }, error = function(e) NULL)
    
    # If the model fitting fails, assign Inf to the performance metric
    if (is.null(gc_model)) {
      performance_metrics[i] <- Inf
      next
    }
    
    # Predict on the test set
    predictions <- tryCatch({
      predict(gc_model, newdata = test_data, allow.new.levels = TRUE)
    }, error = function(e) rep(NA, nrow(test_data)))
    
    # Calculate the performance metric (e.g., Mean Squared Error)
    mse <- mean((test_data$Weight - predictions)^2, na.rm = TRUE)
    
    # Store the performance metric
    performance_metrics[i] <- mse
  }
  
  # Determine the winning model for this iteration
  min_index <- which.min(performance_metrics)
  if (length(min_index) > 0 && !is.infinite(performance_metrics[min_index])) {
    winner[iteration] <- min_index
  }
}

# Filter out NA values from the winner vector
winner <- winner[!is.na(winner)]

# Count the number of times each model wins
winning_counts <- table(winner)

# Print the winning counts
print(winning_counts)

# Map the winning counts to the clustering variables
winning_labels <- clustering_vars[as.numeric(names(winning_counts))]

# Plot a histogram of how often each model wins
barplot(winning_counts, names.arg = winning_labels, xlab = "Model", ylab = "Frequency", main = "Winning Models in Cross-Validation")

# Identify the best clustering variable
best_clustering_var_index <- as.integer(names(which.max(winning_counts)))
best_clustering_var <- clustering_vars[best_clustering_var_index]

# Fit the final Growth Curve model using the best clustering variable, excluding it from fixed effects
fixed_effects <- setdiff(c("AgeMonths", "Sire", "type", "BirthType", "DW", "DurationBeforeSlaughter", "MotherAgeOnBornDate"), best_clustering_var)
final_formula <- as.formula(paste("Weight ~", paste(fixed_effects, collapse = " + "), "+ (1 |", best_clustering_var, ")"))

final_gc_model <- lmer(final_formula, data = combined_lamb_removemissing)

# Summary of the final model
summary(final_gc_model)

# Compute 95% confidence intervals for the model coefficients
conf_intervals <- confint(final_gc_model, level = 0.95)

# Print the confidence intervals
print(conf_intervals)

# Calculate the 95% confidence interval
# Number of observations
n <- nrow(combined_lamb_removemissing)

# Degrees of freedom: n - number of predictors - 1 (for intercept)
df <- n - length(fixef(final_gc_model))

# Critical t value for 95% confidence interval
t_critical <- qt(0.975, df)

# Extract summary table of the model
summary_table <- summary(final_gc_model)$coefficients

# Compute confidence intervals for each coefficient
conf_intervals <- data.frame(
  Estimate = summary_table[, "Estimate"],
  Std_Error = summary_table[, "Std. Error"],
  Lower_95 = summary_table[, "Estimate"] - t_critical * summary_table[, "Std. Error"],
  Upper_95 = summary_table[, "Estimate"] + t_critical * summary_table[, "Std. Error"]
)

# Print the confidence intervals
print(conf_intervals)

#----------------------------------------------------------------------------------------------------------
# Extract fitted values and residuals from the model
fitted_values <- fitted(final_gc_model)
residuals <- resid(final_gc_model)

# Create a data frame for plotting
plot_data <- data.frame(Fitted = fitted_values, Residuals = residuals)

# Plot Residuals vs. Fitted Values
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()


