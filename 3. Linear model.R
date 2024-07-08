# 3. Fitting the linear regression models

# Plot the data
ggplot(combined_lamb, aes(x = AgeMonths, y = Weight, color = Sex)) +
  geom_point() +
  labs(title = "Lamb Weight vs Age by Sex",
       x = "Age in Months",
       y = "Weight in kg",
       color = "Sex") +
  theme_minimal()

# Create the line plot
ggplot(combined_lamb, aes(x = Day, y = Weight, color = Year, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Weight at Day record in different year",
       x = "Day",
       y = "Weight",
       color = "Year") +
  theme_minimal()



# Ensure all levels of factors in the entire dataset are included
combined_lamb$Sire <- factor(combined_lamb$Sire)
combined_lamb$Sex <- factor(combined_lamb$Sex)
combined_lamb$type <- factor(combined_lamb$type)
combined_lamb$BirthType <- factor(combined_lamb$BirthType)
combined_lamb$Season.Born <- factor(combined_lamb$Season.Born)

# Check levels to ensure they have more than one level
summary(combined_lamb$Sire)
summary(combined_lamb$Sex)
summary(combined_lamb$type)
summary(combined_lamb$BirthType)
summary(combined_lamb$Season.Born)

clean_test <- na.omit(combined_lamb)

summary(clean_test$Sire)
summary(clean_test$Sex)
summary(clean_test$type)
summary(clean_test$BirthType)
summary(clean_test$Season.Born)

# Fitting the linear regression model
Allmodel <- glm(Weight ~ AgeMonths + Sex + type + BirthType + Sire + DW + DurationBeforeSlaughter + MotherAgeOnBornDate, 
                data = combined_lamb, family = gaussian())

# Summary of the model
summary(Allmodel)

# Compute 95% confidence intervals for the model coefficients
conf_intervals <- confint(Allmodel, level = 0.95)

# Print the confidence intervals
print(conf_intervals)

# Calculate the 95% confident interval
# Number of observations
n <- nrow(combined_lamb)

# Degrees of freedom: n - number of predictors - 1 (for intercept)
df <- n - length(coefficients(Allmodel))

# Critical t value for 95% confidence interval
t_critical <- qt(0.975, df)

# Extract summary table of the model
summary_table <- summary(Allmodel)$coefficients

# Compute confidence intervals for each coefficient
conf_intervals <- data.frame(
  Estimate = summary_table[, "Estimate"],
  Std_Error = summary_table[, "Std. Error"],
  Lower_95 = summary_table[, "Estimate"] - t_critical * summary_table[, "Std. Error"],
  Upper_95 = summary_table[, "Estimate"] + t_critical * summary_table[, "Std. Error"]
)

# Print the confidence intervals
print(conf_intervals)
#----------------------------------------------------------------------------------------------------#
# CV which different input of model to see which model perform best
# Define predictor variables
predictors <- c("AgeMonths", "Sex", "type", "BirthType", "Sire", "DW", "DurationBeforeSlaughter", "MotherAgeOnBornDate")

# Function to generate all combinations of predictor variables
all_combinations <- function(x) {
  unlist(lapply(1:length(x), function(i) combn(x, i, simplify = FALSE)), recursive = FALSE)
}

combinations <- all_combinations(predictors)

# Create a list of all formulas
formulas <- sapply(combinations, function(combo) {
  as.formula(paste("Weight ~", paste(combo, collapse = " + ")))
})

# Initialize a vector to store the winning model for each iteration
winner <- rep(NA, 100)

# Perform cross-validation
iterations <- 100
for (iteration in 1:iterations) {
  # Split data 80/20 without replacement
  idx <- sample(seq_len(nrow(combined_lamb)), size = 0.8 * nrow(combined_lamb))
  train_data <- combined_lamb[idx, ]
  test_data <- combined_lamb[-idx, ]
  
  # Ensure all levels of factors in the test set match the training set
  for (var in predictors) {
    if (is.factor(combined_lamb[[var]])) {
      train_data[[var]] <- factor(train_data[[var]], levels = levels(combined_lamb[[var]]))
      test_data[[var]] <- factor(test_data[[var]], levels = levels(combined_lamb[[var]]))
    }
  }
  
  # Initialize a vector to store the log-likelihood for each model
  predictive_log_likelihood <- rep(NA, length(formulas))
  
  # Loop through the formulas
  for (i in 1:length(formulas)) {
    formula <- formulas[[i]]
    
    # Fit the GLM model
    current_model <- glm(formula, data = train_data, family = gaussian())
    
    # Extract the dispersion parameter
    sigma <- sqrt(summary(current_model)$dispersion)
    
    # Predict the values using the test set
    ypredict_mean <- predict(current_model, newdata = test_data, type = "response")
    
    # Calculate the predictive log-likelihood
    predictive_log_likelihood[i] <- sum(dnorm(test_data$Weight, ypredict_mean, sigma, log = TRUE), na.rm = TRUE)
  }
  
  # Find the winning model for this iteration
  winner[iteration] <- which.max(predictive_log_likelihood)
}

# Plot a histogram of how often each model wins
hist(winner, breaks = seq(0.5, length(formulas) + 0.5, 1), xlab = "Model", ylab = "Frequency", main = "")

#----------------------------------------------------------------------------------------------------#
# Identify the most frequently winning model
best_model_index <- which.max(table(winner))

# Get the best model's formula
best_model_formula <- formulas[[best_model_index]]

# Print the best model formula
print(best_model_formula)
#----------------------------------------------------------------------------------------------------#
# Fit the best model using the entire dataset
best_model <- glm(best_model_formula, data = combined_lamb, family = gaussian())

# Print the summary of the best model
summary(best_model)
#----------------------------------------------------------------------------------------------------#
