
# Load the dataset
superstore_df <- read.csv(file.choose())

# Column names
colnames(superstore_df)

# Check for NA values
sum(duplicated(superstore_df))

# Check structure of dataframe
str(superstore_df) 

# Convert ship.date and order.date to date format
superstore_df$Order.Date <- as.Date(superstore_df$Order.Date, format="%m/%d/%Y")
superstore_df$Ship.Date <- as.Date(superstore_df$Ship.Date, format="%m/%d/%Y")

# Check ship.date and order.date format
head(superstore_df$Order.Date)
head(superstore_df$Ship.Date)

# Create new variable 'Shipping.Delay' to show number of days between shipping/ordering
superstore_df$Shipping.Delay <- as.numeric(difftime(superstore_df$Ship.Date, superstore_df$Order.Date, units = "days"))


# Convert Categorical variables to factors and check the levels
superstore_df$Category <- as.factor(superstore_df$Category)
levels(superstore_df$Category)

superstore_df$Sub.Category <- as.factor(superstore_df$Sub.Category)
levels(superstore_df$Sub.Category)

superstore_df$Segment <- as.factor(superstore_df$Segment)
levels(superstore_df$Segment)

# Check the unique categories
unique(superstore_df$Category)



# Aggregate profit by subcategory, discount bin, and segment
library(dplyr)

subcategory_profit <- superstore_df %>%
  group_by(Category, Sub.Category, Discount.Binned, Segment) %>%
  summarize(Average_Profit = mean(Profit, na.rm = TRUE), .groups = 'drop')

# Visualizing subcategories within categories by profit

# Grouped Bar Chart
library(ggplot2)

ggplot(subcategory_profit, aes(x = Discount.Binned, y = Average_Profit, fill = Segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Category + Sub.Category, scales = "free_y") +  
  labs(title = "Profit by Subcategory, Segment, and Discount Level",
       x = "Discount Binned",
       y = "Average Profit",
       fill = "Segment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap
ggplot(subcategory_profit, aes(x = Discount.Binned, y = Sub.Category, fill = Average_Profit)) +
  geom_tile() +
  facet_grid(Category ~ .) +  
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  labs(title = "Profitability by Subcategory and Discount Level",
       x = "Discount Binned",
       y = "Subcategory",
       fill = "Avg Profit") +
  theme_minimal()


# Check to ensure discounts are within expected range
summary(superstore_df$Discount)  # Check for extreme values


# Create discount bins
superstore_df$Discount.Binned <- cut(superstore_df$Discount, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 1), 
                                     labels = c("0-10%", "11-20%", "21-30%", "31-40%", "41-100%"))

sum(is.na(superstore_df$Discount.Binned))  # Count missing values
mean(is.na(superstore_df$Discount.Binned))  # Percentage of missing values
superstore_df$Discount.Binned <- as.character(superstore_df$Discount.Binned)  # Convert to character
superstore_df$Discount.Binned[is.na(superstore_df$Discount.Binned)] <- "No Discount" 
superstore_df$Discount.Binned <- as.factor(superstore_df$Discount.Binned)  # Convert back to factor

sum(is.na(superstore_df$Discount.Binned)) 
table(superstore_df$Discount.Binned) #View distribution of discount bins

# Visual of discount frequency
ggplot(superstore_df, aes(x = Discount.Binned)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequency of Discounts Applied",
       x = "Discount Bins",
       y = "Number of Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize sales volume by discount level
ggplot(superstore_df, aes(x = Discount.Binned, y = Sales, fill = Discount.Binned)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Discount Level",
       x = "Discount Binned",
       y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate Discounted Revenue
superstore_df$Discounted.Revenue <- superstore_df$Sales * (1 - superstore_df$Discount)

# Profit Margin Calculation
superstore_df$Profit.Margin <- superstore_df$Profit / superstore_df$Sales

# Customer Segmentation Analysis
aggregate(Profit ~ Segment + Discount.Binned, data = superstore_df, FUN = mean)

# Heatmap and barchart to show average profit across segment and discount bin
library(ggplot2)

profit_data <- aggregate(Profit ~ Segment + Discount.Binned, data = superstore_df, FUN = mean)

# Heatmap
ggplot(profit_data, aes(x = Discount.Binned, y = Segment, fill = Profit)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +  # Red = Low Profit, Green = High Profit
  labs(title = "Customer Segment Profitability by Discount Level",
       x = "Discount Bins", 
       y = "Customer Segment",
       fill = "Avg Profit") +
  theme_minimal()

# Barchart
ggplot(profit_data, aes(x = Discount.Binned, y = Profit, fill = Segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Profit by Customer Segment and Discount Level",
       x = "Discount Bins", 
       y = "Average Profit",
       fill = "Customer Segment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Product - level analysis
aggregate(Sales ~ Sub.Category + Discount.Binned, data = superstore_df, FUN = sum)

# Sales and profit outliers
boxplot(superstore_df$Sales, main = "Sales Outliers")
boxplot(superstore_df$Profit, main = "Profit Outliers")

# Discount outliers
boxplot(superstore_df$Discount, main = "Discount Outliers")

# Discount vs Sales Growth
plot(superstore_df$Discount, superstore_df$Sales, main = "Discount vs. Sales", xlab = "Discount", ylab = "Sales")

# Discount vs profitability
plot(superstore_df$Discount, superstore_df$Profit.Margin, main = "Discount vs. Profit Margin", xlab = "Discount", ylab = "Profit Margin")

# Scatter plot of profit vs sales by discount bin
ggplot(superstore_df, aes(x = Sales, y = Profit, color = Discount.Binned)) +
  geom_point(alpha = 0.5) +
  labs(title = "Sales vs. Profit by Discount Level",
       x = "Sales",
       y = "Profit",
       color = "Discount Level") +
  theme_minimal()

# Fit a linear model for profit as a function of sales and discount bin
lm_model <- lm(Profit ~ Sales + Discount.Binned, data = superstore_df)
summary(lm_model)

# Check the average profit per discount bin

library(dplyr)
library(ggplot2)

discount_analysis <- superstore_df %>%
  group_by(Discount.Binned) %>%
  summarise(Average_Profit = mean(Profit, na.rm = TRUE),
            Total_Sales = sum(Sales, na.rm = TRUE),
            Count = n()) %>%
  arrange(Discount.Binned)

ggplot(discount_analysis, aes(x = Discount.Binned, y = Average_Profit)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(Average_Profit, 2)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Average Profit by Discount Bin", x = "Discount Bin", y = "Average Profit")


# Check profit trends by discount and category
# Aggregate profit per category and discount level
category_discount <- superstore_df %>%
  group_by(Category, Discount.Binned) %>%
  summarise(Average_Profit = mean(Profit, na.rm = TRUE))

# Visualize
ggplot(category_discount, aes(x = Discount.Binned, y = Average_Profit, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Profit by Discount and Product Category", x = "Discount Bin", y = "Average Profit")


# Compute Price Elasticity
superstore_pe <- superstore_df %>%
  mutate(Price = Sales / Quantity)

elasticity_model <- lm(log(Quantity) ~ log(Price) + Discount, data = superstore_pe)
summary(elasticity_model)

# Test tiered discount strategies based on quantity purchased would be more profitable
superstore_tier <- superstore_df %>%
  mutate(Tiered_Discount = case_when(
    Quantity >= 10 ~ "High Volume Discount",
    Quantity >= 5  ~ "Medium Volume Discount",
    TRUE          ~ "No Extra Discount"
  ))

tiered_analysis <- superstore_tier %>%
  group_by(Tiered_Discount) %>%
  summarise(Average_Profit = mean(Profit, na.rm = TRUE))

ggplot(tiered_analysis, aes(x = Tiered_Discount, y = Average_Profit)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Profitability of Tiered Discounts", x = "Discount Strategy", y = "Average Profit")


# Compare Profit for Bundled vs. Discounted Sales
superstore_bundling <- superstore_df %>%
  mutate(Bundle_Offer = ifelse(Sub.Category %in% c("Paper", "Binders"), "Bundle", "No Bundle"))

bundle_analysis <- superstore_bundling %>%
  group_by(Bundle_Offer, Discount.Binned) %>%
  summarise(Average_Profit = mean(Profit, na.rm = TRUE))

ggplot(bundle_analysis, aes(x = Discount.Binned, y = Average_Profit, fill = Bundle_Offer)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Profit: Bundled vs. Discounted Sales", x = "Discount Bin", y = "Average Profit")



# Predict the profit for each discount bin
superstore_df$predicted_profit <- predict(lm_model, newdata = superstore_df)

# Plot the results
ggplot(superstore_df, aes(x = Sales, y = predicted_profit, color = Discount.Binned)) +
  geom_point(alpha = 0.5) +
  labs(title = "Predicted Profit vs. Sales by Discount Level",
       x = "Sales",
       y = "Predicted Profit",
       color = "Discount Level") +
  theme_minimal()

# COrrelation between sales, profit, and discount
cor(superstore_df[c("Sales", "Profit", "Discount")], use = "complete.obs")


# Linear regression model for Profit
profit_model <- lm(Profit ~ Discount + Quantity + Profit.Margin, data = superstore_df)
summary(profit_model)

# Linear regression model for Sales
sales_model <- lm(Sales ~ Discount + Quantity + Profit.Margin, data = superstore_df)
summary(sales_model)

library(rpart)
# Model for Profit
profit_tree <- rpart(Profit ~ Discount + Quantity + Profit.Margin + Category + Sub.Category, data = superstore_df, method = "anova")
plot(profit_tree)
text(profit_tree)

# Model for Sales
sales_tree <- rpart(Sales ~ Discount + Quantity + Profit.Margin + Category + Sub.Category, data = superstore_df, method = "anova")
plot(sales_tree)
text(sales_tree)

# Simulate different discount levels and measure the effect
test_discounts <- c(0, 0.1, 0.2, 0.3, 0.4)  # Discount levels

test_results <- data.frame(Discount = test_discounts, Sales = NA, Profit = NA)

for (i in 1:length(test_discounts)) {
  discounted_data <- superstore_df
  discounted_data$Discounted.Revenue <- discounted_data$Sales * (1 - test_discounts[i])
  discounted_data$Discounted.Profit <- discounted_data$Profit * (1 - test_discounts[i])
  
  # Calculate mean sales and profit for each test discount level
  test_results$Sales[i] <- mean(discounted_data$Discounted.Revenue, na.rm = TRUE)
  test_results$Profit[i] <- mean(discounted_data$Discounted.Profit, na.rm = TRUE)
}

ggplot(test_results, aes(x = Discount, y = Profit)) +
  geom_line() + 
  geom_point() + 
  labs(title = "Profit vs Discount in Simulation")


# Segment-Based Discount Efficiency
library(ggplot2)
ggplot(superstore_df, aes(x = Discount.Binned, y = Profit, fill = Segment)) +
  geom_boxplot() + 
  labs(title = "Profit by Discount and Segment", x = "Discount Bin", y = "Profit")


# Affect of Quantity on sales, profit and discount

ggplot(superstore_df, aes(x = Quantity, y = Sales, color = Segment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sales vs Quantity by Segment")


ggplot(superstore_df, aes(x = Quantity, y = Profit, color = Segment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Profit vs Quantity by Segment")


ggplot(superstore_df, aes(x = Quantity, y = Discount, color = Segment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Discount vs Quantity by Segment")


# Get summary statistics on sales, discount, and profit
summary(superstore_df[, c("Sales", "Discount", "Profit")])

# Correlation of discount on sales and profit
cor(superstore_df[, c("Sales", "Discount", "Profit")], use = "complete.obs")

# Linear Regression model of sales and discount
superstore_lm <- lm(Sales ~ Discount, data = superstore_df)
summary(superstore_lm)


# Check linearity of sales and discount
library(ggplot2)

ggplot(superstore_df, aes(x = Discount, y = Sales)) +
  geom_point(alpha = 0.5) +  # Scatter plot for data points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line
  labs(title = "Sales vs. Discount", x = "Discount", y = "Sales") +
  theme_minimal()


# Boxplot on effects of discount on profit
ggplot(superstore_df, aes(x = factor(Discount), y = Profit)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-50, 200)) +  # Zoom in on the y-axis (Profit)
  theme_minimal()




# Load Packages
library(dplyr)
library(lubridate)
library(stringr)

# Check structure and data types
str(superstore_df)  

# Convert Date Columns to Date Format
superstore_df <- superstore_df %>%
  mutate(Order.Date = as.Date(Order.Date, format = "%m/%d/%Y"),
         Ship.Date = as.Date(Ship.Date, format = "%m/%d/%Y"))

# Calculate shipping duration for number of days between order and shipping

superstore_df <- superstore_df %>%
  mutate(Shipping.Duration = as.numeric(Ship.Date - Order.Date))


# Correlation Matrix
library(corrplot)
cor_matrix <- cor(superstore_df %>% select(-Postal.Code, -Shipping.Duration, -Row.ID) %>% select_if(is.numeric), use="complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method="color", type="lower", tl.cex=0.8, number.cex=0.7, addCoef.col="black")


# Print correlation values
print(cor_matrix)

# Create a correlation heatmap
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", tl.srt = 45)
