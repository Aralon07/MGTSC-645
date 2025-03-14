

# Load the dataset
library(dplyr)
superstore_df <- read.csv(file.choose())

superstore_df <- superstore_df %>%
  select(-Row.ID, -Order.ID, -Customer.ID, -Customer.Name, -Product.ID, -Product.Name, -Country)

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

# Create new columns for Order/Shipping month and year
# Move new columns to front of dataset
# Then, remove order.date/ship.date columns
library(lubridate)
superstore_df$Order.Month <- month(superstore_df$Order.Date, label = TRUE, abbr = TRUE)
superstore_df$Order.Year  <- year(superstore_df$Order.Date)
superstore_df$Shipping.Month <- month(superstore_df$Ship.Date, label = TRUE, abbr = TRUE)
superstore_df$Shipping.Year  <- year(superstore_df$Ship.Date)
superstore_df$Order.Weekday   <- wday(superstore_df$Order.Date, label = TRUE, abbr = TRUE)
superstore_df$Shipping.Weekday <- wday(superstore_df$Ship.Date, label = TRUE, abbr = TRUE)

superstore_df <- superstore_df[, c("Order.Month", "Order.Year", "Order.Weekday", 
                                   "Shipping.Month", "Shipping.Year", "Shipping.Weekday", "Shipping.Delay",
                                   setdiff(names(superstore_df), c("Order.Month", "Order.Year", "Order.Weekday", 
                                                                   "Shipping.Month", "Shipping.Year", "Shipping.Weekday", "Shipping.Delay")))]

superstore_df <- superstore_df %>%
  select(-Order.Date, -Ship.Date)

# Create discount bin, Replace NA values with "No Discount", Convert back to factor with desired order
# Check for missing values and distribution
superstore_df$Discount.Binned <- cut(superstore_df$Discount,
                                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 1),
                                     labels = c("0-10%", "11-20%", "21-30%", "31-40%", "41-100%"),
                                     include.lowest = FALSE, right = TRUE)

superstore_df$Discount.Binned <- as.character(superstore_df$Discount.Binned)
superstore_df$Discount.Binned[is.na(superstore_df$Discount.Binned)] <- "No Discount"

superstore_df$Discount.Binned <- factor(superstore_df$Discount.Binned,
                                        levels = c("No Discount", "0-10%", "11-20%", "21-30%", "31-40%", "41-100%"))

superstore_df <- superstore_df %>%
  select(Order.Month, Order.Year, Order.Weekday, 
         Shipping.Month, Shipping.Year, Shipping.Weekday, Shipping.Delay,
         Ship.Mode, Segment, City, State, Postal.Code, Region, 
         Category, Sub.Category, Sales, Quantity, Discount, Discount.Binned, 
         Profit)

sum(is.na(superstore_df$Discount.Binned))
table(superstore_df$Discount.Binned)

# Check structure of reformatted Dataset and column names
str(superstore_df)
colnames(superstore_df)


# Fit a full model using all variables
lm_full_model <- lm(Profit ~ Order.Month + Order.Year + Order.Weekday + Shipping.Month + 
                   Shipping.Year + Shipping.Weekday + Shipping.Delay + Ship.Mode + Segment + 
                   City + State + Postal.Code + Region + Category + Sub.Category + 
                   Sales + Quantity + Discount + Discount.Binned, data = superstore_df)

# Summarize the model to check the coefficients
summary(lm_full_model)

# Backward Stepwise Selection
step_model_backward <- step(lm_full_model, direction = "backward")
summary(step_model_backward)

# Remove target variable (Profit) temporarily
superstore_pca_df <- superstore_df[, c("Order.Month", "Order.Year", "Order.Weekday", "Shipping.Month", 
                                       "Shipping.Year", "Shipping.Weekday", "Shipping.Delay", "Ship.Mode", 
                                       "Segment", "City", "State", "Postal.Code", "Region", "Category", 
                                       "Sub.Category", "Sales", "Quantity", "Discount", "Discount.Binned")]

# Create dummy variables using model.matrix()
# This will automatically handle factors and convert them to dummy variables
superstore_pca_numeric <- model.matrix(~ . -1, data = superstore_pca_df)

# Now run PCA
pca_result <- prcomp(superstore_pca_numeric, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

plot(pca_result, type = "l", main = "Scree Plot")


# Summarize the model to check the coefficients
library(car)
summary(lm_full_model)
vif(lm_full_model)





# Linear regression model for Profit with interaction term between Discount and Category
profit_model <- lm(Profit ~ Discount * Category, data = superstore_df)
summary(profit_model)

# Linear regression model for Sales with interaction term between Discount and Category
sales_model <- lm(Sales ~ Discount * Category, data = superstore_df)
summary(sales_model)

# Create a new data frame with different Discount values and all Categories
discount_levels <- seq(0, 1, by = 0.05)  # Example: Discount levels from 0 to 50%
categories <- unique(superstore_df$Category)

new_data <- expand.grid(Discount = discount_levels, Category = categories)

# Predict Sales and Profit for each combination
new_data$Predicted_Profit <- predict(profit_model, newdata = new_data)
new_data$Predicted_Sales <- predict(sales_model, newdata = new_data)

# View the results
print(new_data)
summary(new_data)

# Plot Predicted Sales vs Discount for each Category
library(ggplot2)

ggplot(new_data, aes(x = Discount, y = Predicted_Sales, color = Category)) +
  geom_line() +
  labs(title = "Predicted Sales vs Discount by Category", 
       x = "Discount", y = "Predicted Sales")

# Plot Predicted Profit vs Discount for each Category
ggplot(new_data, aes(x = Discount, y = Predicted_Profit, color = Category)) +
  geom_line() +
  labs(title = "Predicted Profit vs Discount by Category", 
       x = "Discount", y = "Predicted Profit")

# Actual Sales and Profit with Quantity
comparison_data$Total_Actual_Sales <- comparison_data$Sales * comparison_data$Quantity
comparison_data$Total_Actual_Profit <- comparison_data$Profit * comparison_data$Quantity
comparison_data <- cbind(superstore_df, new_data[match(superstore_df$Discount, new_data$Discount), 
                                                 c("Predicted_Profit", "Predicted_Sales")])

comparison_data$Total_Actual_Sales <- comparison_data$Sales * comparison_data$Quantity
comparison_data$Total_Actual_Profit <- comparison_data$Profit * comparison_data$Quantity

# View the first few rows of the comparison data
head(comparison_data)

