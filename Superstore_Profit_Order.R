### Load libraries ###
library(dplyr)
library(ggplot2)
library(car)

#### Remove datasets created ####
rm()

# Load the dataset
superstore_df <- read.csv(file.choose())

# Remove irrelavent variables
superstore_df <- superstore_df %>%
  select(-Row.ID, -Order.ID, -Customer.ID, -Customer.Name, -Product.ID, -Product.Name, -Country)

# Basic scatter plot: Sales vs Profit
ggplot(superstore_df, aes(x = Sales, y = Profit)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Comparison of Sales and Profit",
    x = "Sales",
    y = "Profit"
  ) +
  theme_minimal()


# Column names
colnames(superstore_df)

# Check format of the data
str(superstore_df)

# Convert categorical variables into factors
superstore_df$Segment <- as.factor(superstore_df$Segment)
superstore_df$Category <- as.factor(superstore_df$Category)
superstore_df$Sub.Category <- as.factor(superstore_df$Sub.Category)
superstore_df$Region <- as.factor(superstore_df$Region)
superstore_df$Order.Date <- as.Date(superstore_df$Order.Date, format = "%m/%d/%Y")
superstore_df$Ship.Date <- as.Date(superstore_df$Ship.Date, format = "%m/%d/%Y")

# Create new variable 'Shipping.Delay' to show number of days between shipping/ordering
superstore_df$Shipping.Delay <- as.numeric(difftime(superstore_df$Ship.Date, superstore_df$Order.Date, units = "days"))


# View basic summary of the entire dataset
summary(superstore_df)

# Correlation/VIF for numeric values
cor(superstore_df %>% select(Sales, Quantity, Discount, Shipping.Delay))

# Sub Category removed due to similarity with Category
vif_model <- lm(Sales ~ Quantity + Discount + Shipping.Delay +
                  Segment + Ship.Mode + Region + Category,
                data = superstore_df)
vif(vif_model)

# For Sales
lm_profit_sales <- lm(Profit ~ Sales, data = superstore_df)
summary(lm_profit_sales)

# For Quantity
lm_profit_quantity <- lm(Profit ~ Quantity, data = superstore_df)
summary(lm_profit_quantity)

# For Discount
lm_profit_discount <- lm(Profit ~ Discount, data = superstore_df)
summary(lm_profit_discount)

# For Ship.Mode
lm_profit_ship_mode <- lm(Profit ~ Ship.Mode, data = superstore_df)
summary(lm_profit_ship_mode)

# For Segment
lm_profit_segment <- lm(Profit ~ Segment, data = superstore_df)
summary(lm_profit_segment)

# For Region
lm_profit_region <- lm(Profit ~ Region, data = superstore_df)
summary(lm_profit_region)

# For Category
lm_profit_category <- lm(Profit ~ Category, data = superstore_df)
summary(lm_profit_category)

# For Sub.Category
lm_profit_sub_category <- lm(Profit ~ Sub.Category, data = superstore_df)
summary(lm_profit_sub_category)

# For Shipping.Delay
lm_profit_shipping_delay <- lm(Profit ~ Shipping.Delay, data = superstore_df)
summary(lm_profit_shipping_delay)

# Linear Regression - Profit; All other applicable variables
lm_profit_model <- lm(Profit ~ Sales + Quantity + Discount + Ship.Mode + Segment + Region + Category + Sub.Category  + Shipping.Delay, 
                data = superstore_df)
summary(lm_profit_model)

# Model with decided interaction terms
model_interaction <- lm(Profit ~ Sales + Quantity + Discount + Region + Category +
                          Sales:Region + Discount:Category + Quantity:Category + Segment:Category, data = superstore_df)
summary(model_interaction)


# Model with only significant interaction terms
model_refined <- lm(Profit ~ Sales + Quantity + Discount + Region + Category +
                      Sales:Region + Discount:Category +
                      Quantity:Category, data = superstore_df)

summary(model_refined)

# Model with interaction terms (Discount interactions included on refined)

model_interaction_discount <- lm(Profit ~ Sales + Quantity + Discount + Region + Category +
                                   Sales:Region + Discount:Category + Quantity:Category  + Discount:Quantity
                                 + Discount:Sales + Discount:Region, data = superstore_df)
summary(model_interaction_discount)

# Model with interaction terms (Discount interaction focused)

model_interaction_discount_1 <- lm(Profit ~ Sales + Quantity + Discount  + Category + Region +
                                  Discount:Category  + Discount:Quantity
                                 + Discount:Sales + Discount:Region, data = superstore_df)
summary(model_interaction_discount_1)

vif(model_interaction_discount)
vif(model_interaction_discount_1)

# Residuals and fitted values for both models
residuals_interaction_discount <- residuals(model_interaction_discount)
fitted_interaction_discount <- fitted(model_interaction_discount)

residuals_interaction_discount_1 <- residuals(model_interaction_discount_1)
fitted_interaction_discount_1 <- fitted(model_interaction_discount_1)

plot(fitted_interaction_discount, residuals_interaction_discount,
     main = "Residuals vs Fitted (Discount Interaction Model)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

plot(fitted_interaction_discount_1, residuals_interaction_discount_1,
     main = "Residuals vs Fitted (Discount Focused Model)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Histogram of residuals
hist(residuals_interaction_discount,
     main = "Histogram of Residuals (Discount Interaction Model)",
     xlab = "Residuals", col = "skyblue", border = "black")

hist(residuals_interaction_discount_1,
     main = "Histogram of Residuals (Discount Focused Model)",
     xlab = "Residuals", col = "lightgreen", border = "black")

# Summary of all R-squared and adjusted r-squared

model_list <- list(
  "Sales only" = lm(Profit ~ Sales, data = superstore_df),
  "Quantity only" = lm(Profit ~ Quantity, data = superstore_df),
  "Discount only" = lm(Profit ~ Discount, data = superstore_df),
  "Ship Mode only" = lm(Profit ~ Ship.Mode, data = superstore_df),
  "Segment only" = lm(Profit ~ Segment, data = superstore_df),
  "Region only" = lm(Profit ~ Region, data = superstore_df),
  "Category only" = lm(Profit ~ Category, data = superstore_df),
  "Sub-Category only" = lm(Profit ~ Sub.Category, data = superstore_df),
  "Shipping Delay only" = lm(Profit ~ Shipping.Delay, data = superstore_df),
  "Full Model (All Variables)" = lm(Profit ~ Sales + Quantity + Discount + Ship.Mode + Segment + Region + Category + Sub.Category  + Shipping.Delay, data = superstore_df),
  "Model with Interactions" = lm(Profit ~ Sales + Quantity + Discount + Region + Category + Sales:Region + Discount:Category + Quantity:Category + Segment:Category, data = superstore_df),
  "Refined Interaction Model" = lm(Profit ~ Sales + Quantity + Discount + Region + Category + Sales:Region + Discount:Category + Quantity:Category, data = superstore_df),
  "Interaction - Discount Focused (Full)" = lm(Profit ~ Sales + Quantity + Discount + Region + Category + Sales:Region + Discount:Category + Quantity:Category + Discount:Quantity + Discount:Sales + Discount:Region, data = superstore_df),
  "Interaction - Discount Focused (Lean)" = lm(Profit ~ Sales + Quantity + Discount + Category + Region + Discount:Category + Discount:Quantity + Discount:Sales + Discount:Region, data = superstore_df)
) # Store all your models in a named list


model_summary_df <- data.frame(
  Model = character(),
  R_Squared = numeric(),
  Adjusted_R_Squared = numeric(),
  stringsAsFactors = FALSE
) # Extract R-squared and Adjusted R-squared for each model

for (model_name in names(model_list)) {
  model <- model_list[[model_name]]
  model_summary <- summary(model)
  model_summary_df <- rbind(model_summary_df, data.frame(
    Model = model_name,
    R_Squared = round(model_summary$r.squared, 4),
    Adjusted_R_Squared = round(model_summary$adj.r.squared, 4)
  ))
}

print(model_summary_df)



# Define a function to calculate RMSE
calculate_rmse <- function(model, data) {
  # Get the predicted values
  predicted_values <- predict(model, data)
  
  # Calculate residuals (difference between actual and predicted)
  residuals <- data$Profit - predicted_values
  
  # Calculate RMSE
  rmse <- sqrt(mean(residuals^2))
  return(rmse)
}

# Calculate RMSE for each model
rmse_sales <- calculate_rmse(lm_profit_sales, superstore_df)
rmse_quantity <- calculate_rmse(lm_profit_quantity, superstore_df)
rmse_discount <- calculate_rmse(lm_profit_discount, superstore_df)
rmse_ship_mode <- calculate_rmse(lm_profit_ship_mode, superstore_df)
rmse_segment <- calculate_rmse(lm_profit_segment, superstore_df)
rmse_region <- calculate_rmse(lm_profit_region, superstore_df)
rmse_category <- calculate_rmse(lm_profit_category, superstore_df)
rmse_sub_category <- calculate_rmse(lm_profit_sub_category, superstore_df)
rmse_shipping_delay <- calculate_rmse(lm_profit_shipping_delay, superstore_df)
rmse_full_model <- calculate_rmse(lm_profit_model, superstore_df)
rmse_interaction <- calculate_rmse(model_interaction, superstore_df)
rmse_refined <- calculate_rmse(model_refined, superstore_df)
rmse_interaction_discount <- calculate_rmse(model_interaction_discount, superstore_df)
rmse_interaction_discount_1 <- calculate_rmse(model_interaction_discount_1, superstore_df)

# Create a table to compare RMSE values
rmse_table <- data.frame(
  Model = c("Sales", "Quantity", "Discount", "Ship Mode", "Segment", "Region", 
            "Category", "Sub.Category", "Shipping.Delay", "Full Model", 
            "Model with Interactions", "Refined Interaction Model", 
            "Interaction - Discount Focused (Full)", "Interaction - Discount Focused (Lean)"),
  RMSE = c(rmse_sales, rmse_quantity, rmse_discount, rmse_ship_mode, rmse_segment,
           rmse_region, rmse_category, rmse_sub_category, rmse_shipping_delay, 
           rmse_full_model, rmse_interaction, rmse_refined, rmse_interaction_discount,
           rmse_interaction_discount_1)
)

print(rmse_table)


# Models you want to compare for AIC and BIC
models <- list(
  lm_profit_sales,
  lm_profit_quantity,
  lm_profit_discount,
  lm_profit_ship_mode,
  lm_profit_segment,
  lm_profit_region,
  lm_profit_category,
  lm_profit_sub_category,
  lm_profit_shipping_delay,
  lm_profit_model,
  model_interaction,
  model_refined,
  model_interaction_discount,
  model_interaction_discount_1
)

# Initialize a data frame to store AIC and BIC for each model
model_comparison <- data.frame(
  Model = c("Sales", "Quantity", "Discount", "Ship Mode", "Segment", "Region", 
            "Category", "Sub.Category", "Shipping.Delay", "Full Model", 
            "Model with Interactions", "Refined Interaction Model", 
            "Interaction - Discount Focused (Full)", 
            "Interaction - Discount Focused (Lean)"),
  AIC = NA,
  BIC = NA
)

# Loop through models to calculate AIC and BIC
for(i in 1:length(models)) {
  model_comparison$AIC[i] <- AIC(models[[i]])
  model_comparison$BIC[i] <- BIC(models[[i]])
}

# Print the model comparison table
print(model_comparison)

# List of top models based on lowest AIC/BIC for variable comparison

top_models <- list(
  "Interaction - Discount Focused (Full)" = model_interaction_discount,
  "Refined Interaction Model" = model_refined,
  "Model with Interactions" = model_interaction
)

# Initialize an empty data frame to store the variables and coefficients for each model
variables_comparison <- data.frame(Model = character(),
                                   Variable = character(),
                                   Coefficient = numeric(),
                                   stringsAsFactors = FALSE)

# Loop through top models and extract coefficients
for(i in 1:length(top_models)) {
  model_coeffs <- summary(top_models[[i]])$coefficients
  model_vars <- rownames(model_coeffs)
  
  # Store the variables and their coefficients
  for (j in 1:length(model_vars)) {
    variables_comparison <- rbind(variables_comparison,
                                  data.frame(Model = names(top_models)[i],
                                             Variable = model_vars[j],
                                             Coefficient = model_coeffs[j, 1]))
  }
}

# Print the variables comparison table
print(variables_comparison)



# Leverage and Cook's Distance and find the indices where Cook's Distance exceeds the threshold
cooks_dist_interaction_discount <- cooks.distance(model_interaction_discount)
cooks_dist_interaction_discount_1 <- cooks.distance(model_interaction_discount_1)

n <- nrow(superstore_df)

# Calculate the threshold (4 / n)
threshold <- 4 / n

influential_points_interaction_discount <- which(cooks_dist_interaction_discount > threshold)
influential_points_interaction_discount
plot(model_interaction_discount, which = 4)
large_cooks_distance_indices <- c(28, 140, 166, 170, 216, 252, 255, 263, 264, 282, 289, 300, 319, 344, 354, 378, 400, 407,
  464, 510, 516, 519, 558, 596, 661, 684, 722, 751, 754, 870, 950, 978, 995, 1046, 1082,
  1086, 1156, 1200, 1234, 1341, 1370, 1434, 1439, 1455, 1517, 1583, 1597, 1645, 1682, 1714,
  1792, 1804, 1812, 1831, 1864, 1906, 1977, 1987, 1996, 2183, 2190, 2274, 2359, 2372, 2392,
  2405, 2482, 2493, 2506, 2511, 2535, 2540, 2557, 2559, 2568, 2624, 2625, 2629, 2655, 2661,
  2674, 2697, 2698, 2729, 2839, 2847, 2849, 2861, 2874, 2895, 2929, 2988, 3012, 3044, 3056,
  3071, 3085, 3119, 3142, 3151, 3152, 3176, 3243, 3274, 3281, 3296, 3317, 3325, 3371, 3444,
  3481, 3520, 3581, 3589, 3591, 3625, 3629, 3724, 3771, 3944, 3984, 3987, 4001, 4010, 4056,
  4094, 4099, 4116, 4129, 4137, 4191, 4219, 4271, 4278, 4298, 4356, 4459, 4492, 4620, 4720,
  4727, 4813, 4821, 4882, 4889, 4890, 4897, 4938, 4992, 5007, 5012, 5056, 5067, 5113, 5127,
  5171, 5186, 5199, 5301, 5302, 5311, 5321, 5342, 5386, 5466, 5531, 5560, 5563, 5613, 5623,
  5627, 5632, 5647, 5711, 5757, 5785, 5885, 5918, 5919, 5991, 6036, 6049, 6117, 6132, 6176,
  6184, 6201, 6210, 6235, 6263, 6341, 6426, 6468, 6521, 6535, 6536, 6567, 6621, 6627, 6640,
  6688, 6801, 6818, 6827, 6877, 6882, 6885, 6902, 6926, 7115, 7133, 7174, 7194, 7244, 7277,
  7281, 7282, 7329, 7330, 7345, 7349, 7423, 7459, 7475, 7488, 7541, 7580, 7584, 7642, 7647,
  7667, 7684, 7687, 7766, 7773, 7819, 7841, 7847, 7899, 7915, 7938, 8035, 8075, 8124, 8154,
  8169, 8181, 8205, 8209, 8236, 8237, 8252, 8272, 8313, 8425, 8469, 8489, 8522, 8555, 8587,
  8633, 8641, 8681, 8735, 8750, 8800, 8833, 8850, 8859, 8871, 8927, 8936, 8943, 8991, 8994,
  9040, 9075, 9149, 9165, 9222, 9232, 9271, 9368, 9399, 9404, 9408, 9413, 9426, 9481, 9491,
  9494, 9533, 9616, 9618, 9636, 9640, 9650, 9733, 9742, 9775, 9858, 9930, 9943, 9949)

# Access the rows in your dataset with the large Cook's Distance
influential_points <- superstore_df[large_cooks_distance_indices, ]

# View the influential points
print(influential_points)


influential_points_interaction_discount_1 <- which(cooks_dist_interaction_discount > threshold)
influential_points_interaction_discount_1
plot(model_interaction_discount_1, which = 4)
large_cooks_distance_indices_1 <- c(28, 166, 170, 216, 252, 255, 263, 300, 319, 344, 
                                  354, 378, 400, 464, 510, 516, 519, 558, 680, 684, 
                                  722, 870, 950, 995, 1014, 1082, 1086, 1156, 1200, 1234, 
                                  1247, 1341, 1364, 1370, 1395, 1434, 1439, 1455, 1517, 
                                  1645, 1682, 1714, 1792, 1804, 1812, 1864, 1906, 1987, 
                                  1996, 2183, 2358, 2359, 2392, 2405, 2482, 2493, 2506, 
                                  2511, 2535, 2540, 2559, 2568, 2624, 2625, 2629, 2655, 
                                  2661, 2697, 2698, 2729, 2839, 2847, 2849, 2929, 2988, 
                                  3012, 3056, 3071, 3085, 3142, 3151, 3152, 3176, 3243, 
                                  3274, 3281, 3317, 3325, 3371, 3444, 3460, 3481, 3520, 
                                  3581, 3589, 3629, 3724, 3771, 3915, 3984, 3987, 4056, 
                                  4083, 4094, 4099, 4129, 4137, 4191, 4219, 4278, 4298, 
                                  4356, 4459, 4492, 4620, 4669, 4720, 4727, 4813, 4821, 
                                  4824, 4882, 4889, 4897, 4992, 5007, 5012, 5056, 5067, 
                                  5113, 5127, 5171, 5186, 5199, 5301, 5311, 5321, 5342, 
                                  5386, 5466, 5531, 5563, 5613, 5627, 5632, 5711, 5757, 
                                  5785, 5885, 5918, 5991, 6015, 6036, 6049, 6117, 6184, 
                                  6201, 6210, 6263, 6341, 6426, 6468, 6521, 6535, 6536, 
                                  6567, 6621, 6627, 6640, 6688, 6801, 6818, 6827, 6877, 
                                  6882, 6885, 6902, 7043, 7115, 7174, 7194, 7244, 7281, 
                                  7282, 7330, 7345, 7349, 7423, 7459, 7475, 7488, 7541, 
                                  7580, 7584, 7642, 7647, 7650, 7667, 7684, 7687, 7766, 
                                  7773, 7819, 7841, 7899, 7907, 7915, 7938, 8035, 8075, 
                                  8124, 8154, 8169, 8181, 8205, 8209, 8236, 8237, 8252, 
                                  8272, 8313, 8425, 8469, 8489, 8587, 8633, 8641, 8735, 
                                  8750, 8800, 8859, 8927, 8943, 8991, 8994, 9022, 9040, 
                                  9075, 9087, 9093, 9165, 9232, 9271, 9368, 9404, 9408, 
                                  9413, 9426, 9481, 9491, 9494, 9593, 9616, 9618, 9636, 
                                  9640, 9650, 9742, 9775, 9858, 9930, 9943)

# Access the rows in your dataset with the large Cook's Distance
influential_points_1 <- superstore_df[large_cooks_distance_indices_1, ]

# View the influential points
print(influential_points_1)

