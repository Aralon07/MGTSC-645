### Load libraries ###
library(dplyr)
library(ggplot2)
library(car)

#### Remove datasets created ####
rm()

# Load the dataset
superstore_df <- read.csv(file.choose())

superstore_df <- superstore_df %>%
  select(-Row.ID, -Order.ID, -Customer.ID, -Customer.Name, -Product.ID, -Product.Name, -Country)

# Load required package
library(ggplot2)

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

# Create a new variable for fast deliveries
superstore_df$FastDelivery <- ifelse(superstore_df$Shipping.Delay <= 3, 1, 0)

# View basic summary of the entire dataset
summary(superstore_df)

# Correlation/VIF for numeric values
cor(superstore_df %>% select(Sales, Quantity, Discount, Shipping.Delay))

vif_model <- lm(Sales ~ Quantity + Discount + Shipping.Delay +
                  Segment + Ship.Mode + Region + Category,
                data = superstore_df)
vif(vif_model)

# Linear Regression - Profit; All other applicable variables
lm_profit_model <- lm(Profit ~ Sales + Quantity + Discount + Ship.Mode + Segment + Region + Category + Sub.Category  + Shipping.Delay, 
                data = superstore_df)
summary(lm_profit_model)

# Model with interaction terms
model_interaction <- lm(Profit ~ Sales + Quantity + Discount + Region + Category +
                          Sales:Region + Discount:Category + Quantity:Category + Segment:Category, data = superstore_df)
summary(model_interaction)


# Model with only significant interaction terms
model_refined <- lm(Profit ~ Sales + Quantity + Discount + Region + Category +
                      Sales:Region + Discount:Category +
                      Quantity:Category, data = superstore_df)

summary(model_refined)

# Model with interaction terms (Discount)

model_interaction_discount <- lm(Profit ~ Sales + Quantity + Discount + Region + Category +
                                   Sales:Region + Discount:Category + Quantity:Category  + Discount:Quantity
                                 + Discount:Sales + Discount:Region, data = superstore_df)
summary(model_interaction_discount)

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



