# 5. fit Generalized Estimating Equations (GEEs)

# Load Necessary Packages
install.packages("geepack")
library(geepack)

# Check for and handle missing values
combined_lamb_removemissing <- na.omit(combined_lamb)

# Fit the GEE Model
gee_model <- geeglm(Weight ~ AgeMonths + Sex + type + BirthType + DW + DurationBeforeSlaughter + MotherAgeOnBornDate, 
                    id = Sire, 
                    data = combined_lamb_removemissing, 
                    family = gaussian(), 
                    corstr = "exchangeable")

# Summary of the model
summary(gee_model)

#----------------------------------------------------------------------------------------------------------
# Perform cross-validation
set.seed(123)  # For reproducibility

# Number of iterations for cross-validation
iterations <- 500

# Initialize a vector to store the winning model for each iteration
winner <- rep(NA, iterations)

# Define a list of clustering effect variables to test
clustering_vars <- c("Sex", "type", "BirthType", "Dam", "Sire", "No.")

# Perform cross-validation
for (iteration in 1:iterations) {
  # Split data 80/20 without replacement
  set.seed(iteration)
  idx <- sample(seq_len(nrow(combined_lamb_removemissing)), size = 0.8 * nrow(combined_lamb_removemissing))
  train_data <- combined_lamb_removemissing[idx, ]
  test_data <- combined_lamb_removemissing[-idx, ]
  
  # Initialize a vector to store the performance metric for each model
  performance_metrics <- rep(NA, length(clustering_vars))
  
  # Loop through the clustering effect variables
  for (i in seq_along(clustering_vars)) {
    clustering_var <- clustering_vars[i]
    
    # Define fixed effects excluding the clustering variable
    fixed_effects <- setdiff(c("AgeMonths", "Sex", "type", "BirthType", "DW", "DurationBeforeSlaughter", "MotherAgeOnBornDate"), clustering_var)
    
    # Fit the GEE Model
    gee_model <- tryCatch({
      geeglm(as.formula(paste("Weight ~", paste(fixed_effects, collapse = " + "))), 
             id = train_data[[clustering_var]], 
             data = train_data, 
             family = gaussian(), 
             corstr = "exchangeable")
    }, error = function(e) NULL)
    
    # If the model fitting fails, skip to the next iteration
    if (is.null(gee_model)) {
      performance_metrics[i] <- Inf
      next
    }
    
    # Predict on the test set
    predictions <- predict(gee_model, newdata = test_data, type = "response")
    
    # Calculate the performance metric (e.g., Mean Squared Error)
    mse <- mean((test_data$Weight - predictions)^2, na.rm = TRUE)
    
    # Store the performance metric
    performance_metrics[i] <- mse
  }
  
  # Determine the winning model for this iteration
  winner[iteration] <- which.min(performance_metrics)
}

# Count the number of times each model wins
winning_counts <- table(winner)

# Print the winning counts
print(winning_counts)

# Plot a histogram of how often each model wins
barplot(winning_counts, names.arg = clustering_vars, xlab = "Model", ylab = "Frequency", main = "Winning Models in Cross-Validation")
#---------------------------------------------------------------------------------------------------------
# Identify the best clustering variable
best_clustering_var_index <- which.max(winning_counts)
best_clustering_var <- clustering_vars[best_clustering_var_index]

# Define fixed effects excluding the best clustering variable
fixed_effects <- setdiff(c("AgeMonths", "Sex", "type", "BirthType", "DW", "DurationBeforeSlaughter", "MotherAgeOnBornDate"), best_clustering_var)

# Fit the final GEE model using the best clustering variable
final_gee_model <- geeglm(as.formula(paste("Weight ~", paste(fixed_effects, collapse = " + "))), 
                          id = combined_lamb_removemissing[[best_clustering_var]], 
                          data = combined_lamb_removemissing, 
                          family = gaussian(), 
                          corstr = "exchangeable")

# Summary of the final model
summary(final_gee_model)

# Compute 95% confidence intervals for the model coefficients manually

# Extract summary table of the model
summary_table <- summary(final_gee_model)$coefficients

# Number of observations
n <- nrow(combined_lamb_removemissing)

# Degrees of freedom: n - number of predictors - 1 (for intercept)
df <- n - length(coefficients(final_gee_model))

# Critical t value for 95% confidence interval
t_critical <- qt(0.975, df)

# Compute confidence intervals for each coefficient
conf_intervals <- data.frame(
  Estimate = summary_table[, "Estimate"],
  Std_Error = summary_table[, "Std.err"],
  Lower_95 = summary_table[, "Estimate"] - t_critical * summary_table[, "Std.err"],
  Upper_95 = summary_table[, "Estimate"] + t_critical * summary_table[, "Std.err"]
)

# Print the confidence intervals
print(conf_intervals)

#----------------------------------------------------------------------------------------------------------
# Extract fitted values and residuals from the model
fitted_values <- fitted(final_gee_model)
residuals <- resid(final_gee_model)

# Create a data frame for plotting
plot_data <- data.frame(Fitted = fitted_values, Residuals = residuals)

# Plot Residuals vs. Fitted Values
library(ggplot2)
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
#----------------------------------------------------------------------------------------------------------
