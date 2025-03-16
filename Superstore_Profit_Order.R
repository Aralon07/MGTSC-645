### Load libraries ###
library(dplyr)
library(ggplot2)
library(car)

#### Remove datasets created ####
rm(superstore_df_cleaned)

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

# View the RMSE table
print(rmse_table)



# Leverage and Cook's Distance and find the indices where Cook's Distance exceeds the threshold
cooks_dist_interaction_discount <- cooks.distance(model_interaction_discount)
cooks_dist_interaction_discount_1 <- cooks.distance(model_interaction_discount_1)

n <- nrow(superstore_df)

# Calculate the threshold (4 / n)
threshold <- 4 / n

influential_points_interaction_discount <- which(cooks_dist_interaction_discount > threshold)
influential_points_interaction_discount_1 <- which(cooks_dist_interaction_discount_1 > threshold)

influential_points_interaction_discount
influential_points_interaction_discount_1

plot(model_interaction_discount, which = 4)

plot(model_interaction_discount_1, which = 4)

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


# Install if not already installed
install.packages("effects")

# Load the package
library(effects)

# Plot all effects from the model
plot(allEffects(model_interaction_discount_1), multiline = TRUE, ci.style = "bands")


# Extract fitted values and residuals from the model
fitted_values <- model_interaction_discount$fitted.values
residuals <- model_interaction_discount$residuals

plot(fitted_values, residuals,
     xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at zero for reference

# Plot: Histogram of Residuals
hist(residuals, main = "Histogram of Residuals",
     xlab = "Residuals", breaks = 50, col = "lightblue", border = "black")

# Q-Q Plot of Residuals
qqnorm(residuals)
qqline(residuals, col = "red")

# Boxplot for Residuals
boxplot(residuals(model_interaction_discount), 
        main = "Boxplot of Residuals", 
        ylab = "Residuals", 
        col = "lightblue")

# Boxplot for Fitted Values
boxplot(fitted(model_interaction_discount), 
        main = "Boxplot of Fitted Values", 
        ylab = "Fitted Values", 
        col = "lightgreen")

# Combined Boxplot for Residuals and Fitted Values
boxplot(residuals(model_interaction_discount), 
        fitted(model_interaction_discount), 
        names = c("Residuals", "Fitted Values"), 
        main = "Boxplot of Residuals vs Fitted Values", 
        col = c("lightblue", "lightgreen"))



