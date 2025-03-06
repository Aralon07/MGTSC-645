# this comment is to test github
# Load the dataset
superstore_df <- read.csv(file.choose())

# Column names
colnames(superstore_df)

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

