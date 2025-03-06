# Load the dataset
superstore_df <- read.csv(file.choose())

# Get summary statistics
summary(superstore_df)

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


