### Install Libraries ###
install.packages("tseries") 
install.packages("boot")
install.packages("Metrics")
install.packages("caret") 
install.packages("dyplyr")
install.packages("lmtest") 

### Load libraries ###
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(MASS)
library(tseries)
library(caret)
library(Metrics)
library(lmtest)

#### Data Clean-up ####

# Load the dataset
superstore_df <- read.csv(file.choose())
superstore_df <- as.data.frame(superstore_df)

# Remove irrelevant variables (7 Variables)
superstore_df <- superstore_df %>%
  dplyr::select(-Row.ID, -Order.ID, -Customer.ID, -Customer.Name, -Product.ID, -Product.Name, -Country)

# Convert categorical variables into factors
superstore_df$Segment <- as.factor(superstore_df$Segment)
superstore_df$Category <- as.factor(superstore_df$Category)
superstore_df$Sub.Category <- as.factor(superstore_df$Sub.Category)
superstore_df$Region <- as.factor(superstore_df$Region)
superstore_df$Order.Date <- as.Date(superstore_df$Order.Date, format = "%m/%d/%Y")
superstore_df$Ship.Date <- as.Date(superstore_df$Ship.Date, format = "%m/%d/%Y")

# Create new variable 'Shipping.Delay' to show number of days between shipping/ordering
superstore_df$Shipping.Delay <- as.numeric(difftime(superstore_df$Ship.Date, superstore_df$Order.Date, units = "days"))

# Column names
colnames(superstore_df)

# Check format of the data
str(superstore_df)

# # View basic summary of the entire dataset
# summary(superstore_df)
# 
# # Correlation/VIF for numeric values
# cor(superstore_df %>% select(Sales, Quantity, Discount, Shipping.Delay))
# 
# # Sub Category removed due to similarity with Category
# vif_model <- lm(Sales ~ Quantity + Discount + Shipping.Delay +
#                   Segment + Ship.Mode + Region + Category,
#                 data = superstore_df)
# vif(vif_model)

#### Data Clean-up ENDED ####


### Linear Regression ###

# # For Sales
# lm_profit_sales <- lm(Profit ~ Sales, data = superstore_df)
# plot(lm_profit_sales, which = 1, main = "Residuals vs Fitted - Sales Model")
# 
# summary(lm_profit_sales)
# 
# # For Quantity
# lm_profit_quantity <- lm(Profit ~ Quantity, data = superstore_df)
# plot(lm_profit_quantity, which = 1, main = "Residuals vs Fitted - Quantity Model")
# 
# summary(lm_profit_quantity)
# 
# # For Discount
# lm_profit_discount <- lm(Profit ~ Discount, data = superstore_df)
# plot(lm_profit_discount, which = 1, main = "Residuals vs Fitted - Discount Model")
# 
# summary(lm_profit_discount)
# 
# # For Ship.Mode
# lm_profit_ship_mode <- lm(Profit ~ Ship.Mode, data = superstore_df)
# plot(lm_profit_ship_mode, which = 1, main = "Residuals vs Fitted - Ship.Mode Model")
# 
# summary(lm_profit_ship_mode)
# 
# # For Segment
# lm_profit_segment <- lm(Profit ~ Segment, data = superstore_df)
# plot(lm_profit_segment, which = 1, main = "Residuals vs Fitted - Segment Model")
# 
# summary(lm_profit_segment)
# 
# # For Region
# lm_profit_region <- lm(Profit ~ Region, data = superstore_df)
# plot(lm_profit_region, which = 1, main = "Residuals vs Fitted - Region Model")
# 
# summary(lm_profit_region)
# 
# # For Category
# lm_profit_category <- lm(Profit ~ Category, data = superstore_df)
# plot(lm_profit_category, which = 1, main = "Residuals vs Fitted - Category Model")
# 
# summary(lm_profit_category)
# 
# # For Sub.Category
# lm_profit_sub_category <- lm(Profit ~ Sub.Category, data = superstore_df)
# summary(lm_profit_sub_category)
# 
# # For Shipping.Delay
# lm_profit_shipping_delay <- lm(Profit ~ Shipping.Delay, data = superstore_df)
# plot(lm_profit_shipping_delay, which = 1, main = "Residuals vs Fitted - Shipping Delay Model")
# 
# summary(lm_profit_shipping_delay)
# 
# # Linear Regression - Profit; All other applicable variables
# lm_profit_model <- lm(Profit ~ Sales + Quantity + Discount + Ship.Mode + Segment + Region + Category + Sub.Category  + Shipping.Delay, 
#                 data = superstore_df)
# plot(lm_profit_model, which = 1, main = "Residuals vs Fitted - Full Model")
# 
# summary(lm_profit_model)

### Linear Regression ENDED ###



### Linear Regression Interaction ###

# Interaction between Sales, Region, Quantity, and Discount
lm_model_three_way <- lm(Profit ~ Sales * Region * Discount* Quantity,
                         data = superstore_df)

summary(lm_model_three_way)

vif(lm_model_three_way)
vif(lm_model_three_way, type = "predictor")

plot(lm_model_three_way, which = 1, main = "Residuals vs Fitted - Stepwise Model")
plot(lm_model_three_way, which = 2, main = "Normal Q-Q Plot - Stepwise Model")

### Log Transformation of Profit ###
# Step 1: Shift the Profit variable to make all values positive
superstore_df$Profit_shifted <- superstore_df$Profit + abs(min(superstore_df$Profit)) + 1  # Adding 1 to ensure no zero values

# Step 2: Apply the log transformation
superstore_df$Profit_log <- log(superstore_df$Profit_shifted)
summary(superstore_df$Profit_log)

# Step 3: Fit the linear model with the transformed Profit variable
lm_model_log_transformed <- lm(Profit_log ~ Sales * Region * Discount * Quantity, data = superstore_df)

# Step 4: View the summary of the model
summary(lm_model_log_transformed)

# Calculate residuals
superstore_df$residuals <- lm_model_log_transformed$residuals

# Check the range of residuals to detect any extreme negative values
summary(superstore_df$residuals)

# Extract residuals from the model
superstore_df$residuals <- lm_model_log_transformed$residuals

# Separate positive and negative residuals
# Ensure you have the fitted values for each subset of residuals
positive_residuals$fitted_values <- lm_model_log_transformed$fitted.values[superstore_df$residuals > 0]
negative_residuals$fitted_values <- lm_model_log_transformed$fitted.values[superstore_df$residuals < 0]

# Plot for positive residuals
ggplot(positive_residuals, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  labs(title = "Positive Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals")

# Plot for negative residuals
ggplot(negative_residuals, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  labs(title = "Negative Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals")

# Q-Q plot for positive residuals
qqnorm(positive_residuals$residuals)
qqline(positive_residuals$residuals, col = "red")

# Q-Q plot for negative residuals
qqnorm(negative_residuals$residuals)
qqline(negative_residuals$residuals, col = "red")

### Log Transformation Profit ENDED ###


## Fit a Lasso regression model with cross-validation ###
library(glmnet)

# Convert categorical variable to factors (if applicable)
superstore_df$Region <- as.numeric(as.factor(superstore_df$Region))

# Create interaction terms manually
X <- model.matrix(~ Sales * Region * Discount * Quantity - 1, data = superstore_df)
y <- superstore_df$Profit

# Set a seed for reproducibility
set.seed(123)  
cv_fit <- cv.glmnet(X, y, alpha = 1, nfolds = 5)

# Get best lambda
best_lambda <- cv_fit$lambda.min
print(best_lambda)

# Fit final model
final_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)
coef(final_model)
plot(cv_fit)


# Compute the baseline (mean of Profit)
y_baseline <- mean(y)
print(y_baseline)
rmse_baseline <- sqrt(mean((y - y_baseline)^2))
print(rmse_baseline)

# Compute RMSE of lasso model
y_pred <- predict(final_model, X, s = best_lambda) # Generate predictions using the final Lasso model
rmse <- sqrt(mean((y - y_pred)^2))
print(rmse)

# MAE for Lasso Model
mae_lasso <- mean(abs(y - y_pred))
print(mae_lasso)

# Plot RMSE for each fold in the cross-validation
plot(cv_fit$lambda, cv_fit$cvm, type = "b", log = "x", 
     xlab = "Lambda", ylab = "RMSE (Cross-validation)", 
     main = "RMSE Across λ")
abline(v = best_lambda, col = "red", lty = 2)


# New model excluding the zeroed-out interaction terms
lm_model_refined <- lm(Profit ~ Sales + Discount + Quantity + Sales:Discount + Sales:Quantity + Region:Quantity + Sales:Region:Discount:Quantity, 
                       data = superstore_df)
lm_model_refined_2 <- lm(Profit ~ Sales + Discount + Quantity + Sales:Discount + Discount:Quantity + Sales:Discount:Quantity, 
                       data = superstore_df)

# View model summary
summary(lm_model_refined)


# View model summary
summary(lm_model_refined)
summary(lm_model_refined_2)

# View the summary of the new model
summary(lm_model_refined)

## Group Lasso
# Load necessary libraries
library(grpreg)

# Convert categorical variable to factors (if applicable)
superstore_df$Region <- as.numeric(as.factor(superstore_df$Region))

# Create interaction terms manually
X <- model.matrix(~ Sales * Region * Discount * Quantity - 1, data = superstore_df)
y <- superstore_df$Profit

# Define groups: Group 1 for main effects, Group 2 for interactions (adjust according to your model)
group <- c(rep(1, 4), rep(2, ncol(X) - 4))  # 4 main effects, rest are interaction terms

# Set seed for reproducibility
set.seed(123)

# Run 5-fold Cross-validation for Group Lasso
cv_group_lasso <- cv.grpreg(X, y, group = group, penalty = "grLasso", nfolds = 5)

# Get best lambda from cross-validation
best_lambda_group <- cv_group_lasso$lambda.min
print(paste("Best lambda for Group Lasso: ", best_lambda_group))

# Fit the final Group Lasso model using the best lambda
final_group_lasso <- grpreg(X, y, group = group, penalty = "grLasso", lambda = best_lambda_group)

# Coefficients of the final Group Lasso model
print("Final Group Lasso model coefficients:")
print(coef(final_group_lasso))

# Plot the cross-validation results (lambda vs RMSE)
plot(cv_group_lasso$lambda, cv_group_lasso$cvm, type = "b", log = "x", 
     xlab = "Lambda", ylab = "RMSE (Cross-validation)", 
     main = "RMSE Across Cross-validation Folds for Group Lasso")
abline(v = best_lambda_group, col = "red", lty = 2)

# Compute the baseline RMSE (mean of Profit as baseline prediction)
y_baseline <- mean(y)
rmse_baseline <- sqrt(mean((y - y_baseline)^2))
print(paste("Baseline RMSE: ", rmse_baseline))

# Compute RMSE for Group Lasso model (final model)
y_pred_group_lasso <- predict(final_group_lasso, X)  # Predictions using the final Group Lasso model
rmse_group_lasso <- sqrt(mean((y - y_pred_group_lasso)^2))
print(paste("Group Lasso Model RMSE: ", rmse_group_lasso))

# MAE for Group Lasso Model
mae_group_lasso <- mean(abs(y - y_pred_group_lasso))
print(paste("Group Lasso Model MAE: ", mae_group_lasso))

### Lasso Regression ENDED ###

### Winsorization 5th and 95th percentile ###

# Define Winsorization function
winsorize <- function(x, lower = 0.05, upper = 0.95) {
  lower_cap <- quantile(x, lower)
  upper_cap <- quantile(x, upper)
  x[x < lower_cap] <- lower_cap
  x[x > upper_cap] <- upper_cap
  return(x)
}

# Apply Winsorization to the Profit variable
superstore_df$Profit_winsorized <- winsorize(superstore_df$Profit)

# Fit the model again with the Winsorized Profit
model_interaction_final_2_winsorized <- lm(Profit_winsorized ~ Sales + Quantity + Discount + 
                                             Region +
                                             Sales:Region + Discount:Category + 
                                             Quantity:Category + Sales:Discount + 
                                             Discount:Region, data = superstore_df)

# Get the residuals from the new model
residuals_winsorized <- residuals(model_interaction_final_2_winsorized)

# Plot residuals vs. fitted values
par(mfrow = c(1, 2))  # Set up the plotting area to have two plots

# Residual vs Fitted plot
plot(fitted(model_interaction_final_2_winsorized), residuals_winsorized, 
     main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q plot to check for normality of residuals
qqnorm(residuals_winsorized, main = "Q-Q Plot of Residuals")
qqline(residuals_winsorized, col = "red")

# Reset plotting area
par(mfrow = c(1, 1))

### Winsorization 5th and 95th percentile ENDED ###


# library(knitr)
# 
# vif_values <- vif(lm_model_refined) # Compute VIF
# vif_table <- data.frame(Variable = names(vif_values), VIF = vif_values) # Create DataFrame
# kable(vif_table, caption = "Variance Inflation Factor (VIF) Table") # Display as 
# 
# # New model excluding the zeroed-out interaction terms and scaling (TEST)
# superstore_df$Sales_scaled <- scale(superstore_df$Sales, center = TRUE, scale = TRUE)
# superstore_df$Discount_scaled <- scale(superstore_df$Discount, center = TRUE, scale = TRUE)
# superstore_df$Quantity_scaled <- scale(superstore_df$Quantity, center = TRUE, scale = TRUE)
# 
# lm_model_refined_Scaled <- lm(Profit ~ Sales_scaled * Discount_scaled * Quantity_scaled * Region, 
#                        data = superstore_df)
# 
# summary(lm_model_refined_Scaled)
# vif(lm_model_refined_Scaled)
# 
# 
# 
# 
# # Fit the LASSO model over a sequence of lambda values
# fit <- glmnet(X, y, alpha = 1)
# 
# # Extract coefficient paths
# coefs <- as.matrix(fit$beta)
# lambda_values <- fit$lambda
# 
# # Convert to long format for ggplot2
# coefs_df <- data.frame(lambda = rep(log(lambda_values), each = nrow(coefs)),
#                        feature = rep(rownames(coefs), times = length(lambda_values)),
#                        coefficient = as.vector(coefs))
# 
# # Plot coefficient paths
# ggplot(coefs_df, aes(x = lambda, y = coefficient, color = feature)) +
#   geom_line() +
#   scale_x_reverse() +  # Reverse scale for better readability
#   labs(title = "LASSO Coefficient Paths",
#        x = "Log(Lambda)",
#        y = "Coefficient Value") +
#   theme_minimal()
# 
# # Extract lambda.min (lowest error) and lambda.1se (simpler model)
# best_lambda <- cv_fit$lambda.min
# simpler_lambda <- cv_fit$lambda.1se
# 
# cat("Best Lambda (lambda.min):", best_lambda, "\n")
# cat("Simpler Lambda (lambda.1se):", simpler_lambda, "\n")
# 
# # Extract coefficients for lambda.min (best model)
# coef_min <- coef(cv_fit, s = "lambda.min")
# 
# # Extract coefficients for lambda.1se (simpler model)
# coef_1se <- coef(cv_fit, s = "lambda.1se")
# 
# # Print selected coefficients
# cat("\nCoefficients for lambda.min (best model):\n")
# print(coef_min)
# 
# cat("\nCoefficients for lambda.1se (simpler model, fewer features):\n")
# print(coef_1se)
# 
# # Count number of nonzero coefficients for each model
# num_features_min <- sum(coef_min != 0)
# num_features_1se <- sum(coef_1se != 0)
# 
# cat("\nNumber of features in lambda.min:", num_features_min, "\n")
# cat("Number of features in lambda.1se:", num_features_1se, "\n")
# 
# 
# 
# # Get all lambda values from cross-validation
# lambda_values <- cv_fit$lambda
# 
# # Count nonzero coefficients for each lambda
# num_features <- sapply(lambda_values, function(l) sum(coef(cv_fit, s = l) != 0))
# 
# # Create a dataframe for plotting
# features_df <- data.frame(lambda = log(lambda_values), num_features = num_features)
# 
# ggplot(features_df, aes(x = lambda, y = num_features)) +
#   geom_line(color = "blue", size = 1) +
#   geom_vline(xintercept = log(best_lambda), linetype = "dashed", color = "red", size = 1.2) +
#   geom_vline(xintercept = log(simpler_lambda), linetype = "dashed", color = "green", size = 1.2) +
#   labs(title = "Feature Selection Across Lambda Values",
#        x = "Log(Lambda)",
#        y = "Number of Selected Features") +
#   theme_minimal()
# 
# # Split the data into training and testing sets (70/30) Split
# set.seed(42)  # Set seed for reproducibility
# train_indices <- sample(1:nrow(superstore_df), size = 0.7 * nrow(superstore_df))  # 70% training data
# train_data <- superstore_df[train_indices, ]
# test_data <- superstore_df[-train_indices, ]
# 
# # Prepare the X and y for training and testing
# X_train <- model.matrix(~ Sales * Region * Discount * Quantity - 1, data = train_data)
# y_train <- train_data$Profit
# X_test <- model.matrix(~ Sales * Region * Discount * Quantity - 1, data = test_data)
# y_test <- test_data$Profit
# 
# # Fit Lasso model with cross-validation on training data
# cv_fit <- cv.glmnet(X_train, y_train, alpha = 1)
# best_lambda <- cv_fit$lambda.min
# final_model <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda)
# 
# # Make predictions on test data using the best model
# y_pred_min <- predict(final_model, s = "lambda.min", newx = X_test)
# 
# # Calculate RMSE for the best model (lambda.min)
# rmse_min <- sqrt(mean((y_test - y_pred_min)^2))
# cat("RMSE for lambda.min model:", rmse_min, "\n")
# 
# # Fit Lasso model with cross-validation on training data for the simpler model (lambda.1se)
# simpler_lambda <- cv_fit$lambda.1se
# final_model_1se <- glmnet(X_train, y_train, alpha = 1, lambda = simpler_lambda)
# 
# # Make predictions on test data using the simpler model
# y_pred_1se <- predict(final_model_1se, s = "lambda.1se", newx = X_test)
# 
# # Calculate RMSE for the simpler model (lambda.1se)
# rmse_1se <- sqrt(mean((y_test - y_pred_1se)^2))
# cat("RMSE for lambda.1se model:", rmse_1se, "\n")
# 
# # Perform 5-fold cross-validation using cv.glmnet
# cv_fit <- cv.glmnet(X, y, alpha = 1, nfolds = 5)
# 
# # Get the best lambda (lambda.min)
# best_lambda <- cv_fit$lambda.min
# cat("Best Lambda (lambda.min):", best_lambda, "\n")
# 
# # Fit the final model using the best lambda (lambda.min)
# final_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)
# 
# # Make predictions using the final model
# y_pred_min <- predict(final_model, s = "lambda.min", newx = X)
# 
# # Calculate RMSE for the best model (lambda.min)
# rmse_min <- rmse(y, y_pred_min)
# cat("RMSE for lambda.min model:", rmse_min, "\n")
# 
# # Calculate RMSE for lambda.1se (simpler model)
# simpler_lambda <- cv_fit$lambda.1se
# final_model_1se <- glmnet(X, y, alpha = 1, lambda = simpler_lambda)
# 
# # Make predictions using the simpler model
# y_pred_1se <- predict(final_model_1se, s = "lambda.1se", newx = X)
# 
# # Calculate RMSE for the simpler model (lambda.1se)
# rmse_1se <- rmse(y, y_pred_1se)
# cat("RMSE for lambda.1se model:", rmse_1se, "\n")
# 
# # Plot RMSE for each fold during cross-validation for lambda.min and lambda.1se
# cv_rmse_min <- cv_fit$cvm  # RMSE values for lambda.min model
# cv_rmse_1se <- cv_fit$cvm # RMSE values for lambda.1se model
# 
# # Convert to data frame for ggplot
# cv_df <- data.frame(Fold = rep(1:length(cv_rmse_min), 2),
#                     RMSE = c(cv_rmse_min, cv_rmse_1se),
#                     Model = rep(c("lambda.min", "lambda.1se"), each = length(cv_rmse_min)))
# 
# # Plot RMSE for both models across the folds
# ggplot(cv_df, aes(x = Fold, y = RMSE, color = Model)) +
#   geom_line() +
#   geom_point() +
#   labs(title = "RMSE Across Folds for lambda.min and lambda.1se Models",
#        x = "Fold",
#        y = "RMSE") +
#   theme_minimal() +
#   theme(legend.position = "top")
# 
# 
# ### Linear Regression Interaction ENDED ###
# 
# 
# ### Ridge Regression -Interaction model ###

# install.packages("glmnet")  # Install if not already installed
# library(glmnet)
# 
# # Convert categorical variables (Region, Sub.Category) into dummy variables using model.matrix()
# X <- model.matrix(Profit ~ Sales * Region * Discount * Quantity, data = superstore_df)[, -1]  # Removing intercept column
# y <- superstore_df$Profit  # Target variable
# 
# # Fit Ridge Regression
# ridge_model <- glmnet(X, y, alpha = 0)
# 
# # Plot the coefficients to see the impact of different lambda values
# plot(ridge_model, xvar = "lambda", label = TRUE)
# 
# # Cross-validation to find optimal lambda
# cv_ridge_model <- cv.glmnet(X, y, alpha = 0)
# 
# # Plot the cross-validation results
# plot(cv_ridge_model)
# 
# # Get the best lambda value
# best_lambda <- cv_ridge_model$lambda.min
# cat("Best lambda value:", best_lambda, "\n")
# 
# # Fit the model with the optimal lambda
# final_ridge_model <- glmnet(X, y, alpha = 0, lambda = best_lambda)
# 
# # View the coefficients of the final model
# coef(final_ridge_model)
# 
# 
# # Get predictions from the final Ridge model
# predictions <- predict(final_ridge_model, newx = X)
# 
# # Calculate the residuals
# residuals <- y - predictions
# 
# # Calculate the RSS (Residual Sum of Squares)
# RSS <- sum(residuals^2)
# 
# # Calculate the TSS (Total Sum of Squares)
# TSS <- sum((y - mean(y))^2)
# 
# # Number of observations and predictors
# n <- length(y)
# p <- ncol(X)
# 
# # Calculate Adjusted R²
# adjusted_R2 <- 1 - (RSS / (n - p)) / (TSS / (n - 1))
# cat("Adjusted R² for Ridge Model:", adjusted_R2, "\n")
# 
# # Check linearity with scatter plots
# plot(superstore_df$Sales, y)
# plot(superstore_df$Discount, y)
# plot(superstore_df$Quantity, y)
# plot(superstore_df$Region, y)  # This will be tricky since Region is categorical
# 
# # Calculate fitted values (predictions)
# fitted_values <- predictions
# 
# # Plot residuals vs fitted values
# plot(fitted_values, residuals)
# abline(h = 0, col = "red")
# 
# # Plot coefficients for different lambda values
# plot(ridge_model, xvar = "lambda", label = TRUE)
# 
# 
# ## Cross Validation-Ridge
# 
# # Prepare data (dummy variables and target variable)
# X <- model.matrix(Profit ~ Sales * Region * Discount * Quantity, data = superstore_df)[, -1]  # Removing intercept column
# y <- superstore_df$Profit  # Target variable
# 
# # Fit Ridge Regression with Cross-Validation
# cv_ridge_model <- cv.glmnet(X, y, alpha = 0, nfolds = 5)
# 
# # Plot cross-validation results
# plot(cv_ridge_model)
# 
# # Best lambda from cross-validation
# best_lambda <- cv_ridge_model$lambda.min
# cat("Best lambda value:", best_lambda, "\n")
# 
# # RMSE Calculation using Cross-Validation
# cv_rmse <- cv_ridge_model$cvm  # Cross-validation mean error (RMSE per fold)
# 
# # Average RMSE
# average_rmse <- mean(cv_rmse)
# cat("Average RMSE from cross-validation:", average_rmse, "\n")
# 
# # Optional: Calculate RMSE for the final model with the optimal lambda
# final_predictions <- predict(cv_ridge_model, s = "lambda.min", newx = X)
# final_residuals <- y - final_predictions
# 
# # RMSE for the final model
# final_rmse <- sqrt(mean(final_residuals^2))
# cat("RMSE for the final model:", final_rmse, "\n")
# 
# 
# library(glmnet)
# 
# # Prepare data (dummy variables and target variable)
# X <- model.matrix(Profit ~ Sales * Region * Discount * Quantity, data = superstore_df)[, -1]  # Removing intercept column
# y <- superstore_df$Profit  # Target variable
# 
# # Initialize a vector to store RMSE for each training size
# train_sizes <- seq(50, nrow(X), length.out = 10)  # Training sizes from 50 to total number of observations
# train_rmse <- numeric(length(train_sizes))
# 
# # Loop over training sizes
# for (i in 1:length(train_sizes)) {
#   # Subset the data based on the current training size
#   train_X <- X[1:train_sizes[i], ]
#   train_y <- y[1:train_sizes[i]]
#   
#   # Fit Ridge Regression with Cross-Validation on the subset
#   cv_ridge_model <- cv.glmnet(train_X, train_y, alpha = 0, nfolds = 5)
#   
#   # Get the RMSE (cross-validation mean error) for the current subset
#   train_rmse[i] <- mean(cv_ridge_model$cvm)  # Average RMSE over all folds for this training size
# }
# 
# # Plot the learning curve (Training size vs. RMSE)
# plot(train_sizes, train_rmse, type = "b", xlab = "Training Set Size", ylab = "Average RMSE",
#      main = "Learning Curve for Ridge Regression with Cross-Validation", col = "blue", pch = 19)


### Ridge Regression -Interaction ENDED ###


### Winsorization 5th and 95th percentile ###
  
# Define Winsorization function
winsorize <- function(x, lower = 0.05, upper = 0.95) {
  lower_cap <- quantile(x, lower)
  upper_cap <- quantile(x, upper)
  x[x < lower_cap] <- lower_cap
  x[x > upper_cap] <- upper_cap
  return(x)
}

# Apply Winsorization to the Profit variable
superstore_df$Profit_winsorized <- winsorize(superstore_df$Profit)

# Fit the model again with the Winsorized Profit
model_interaction_final_2_winsorized <- lm(Profit_winsorized ~ Sales + Quantity + Discount + 
                                             Region +
                                             Sales:Region + Discount:Category + 
                                             Quantity:Category + Sales:Discount + 
                                             Discount:Region, data = superstore_df)

# Get the residuals from the new model
residuals_winsorized <- residuals(model_interaction_final_2_winsorized)

# Plot residuals vs. fitted values
par(mfrow = c(1, 2))  # Set up the plotting area to have two plots

# Residual vs Fitted plot
plot(fitted(model_interaction_final_2_winsorized), residuals_winsorized, 
     main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q plot to check for normality of residuals
qqnorm(residuals_winsorized, main = "Q-Q Plot of Residuals")
qqline(residuals_winsorized, col = "red")

# Reset plotting area
par(mfrow = c(1, 1))

### Winsorization 5th and 95th percentile ENDED ###


 


