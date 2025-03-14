# Load the dataset
superstore_df <- read.csv(file.choose())

# Column names
colnames(superstore_df)

# Convert categorical variables into factors
superstore_df$Segment <- as.factor(superstore_df$Segment)
superstore_df$Category <- as.factor(superstore_df$Category)
superstore_df$Sub.Category <- as.factor(superstore_df$Sub.Category)
superstore_df$Region <- as.factor(superstore_df$Region)
superstore_df$Order.Date <- as.Date(superstore_df$Order.Date, format = "%m/%d/%Y")
superstore_df$Ship.Date <- as.Date(superstore_df$Ship.Date, format = "%m/%d/%Y")

# Check data structure
str(superstore_df)

# To view unique vs total orders of customers
library(dplyr)

table_order <- table(superstore_df$Order.ID) # Count frequency of each Order.ID

duplicates <- table_order[table_order > 1] # View only duplicates (Order.IDs that appear more than once)
duplicates

superstore_df[duplicated(superstore_df$Order.ID) | duplicated(superstore_df$Order.ID, fromLast = TRUE), ] # Rows with duplicated order.ID

customer_orders_summary <- superstore_df %>%
  group_by(Customer.ID) %>%
  summarise(Unique.Orders = n_distinct(Order.ID)) # See how many unique orders per customer

# Create a summary for each customer with both total orders and unique orders
customer_orders_comparison <- superstore_df %>%
  group_by(Customer.ID) %>%
  summarise(
    Total.Orders = n(),                     # Total number of orders (including duplicates)
    Unique.Orders = n_distinct(Order.ID)    # Number of unique orders
  )

# View the comparison
print(customer_orders_comparison)

# Create a summary for each customer with both total orders and unique orders, including extra details
customer_orders_comparison <- superstore_df %>%
  group_by(Customer.ID) %>%
  summarise(
    Total.Orders = n(),                                   # Total number of orders (including duplicates)
    Unique.Orders = n_distinct(Order.ID),                  # Number of unique orders
    Total.Sales = sum(Sales),                              # Total sales across all orders
    Total.Profit = sum(Profit),                            # Total profit across all orders
    Avg.Sales.Per.Order = mean(Sales),                      # Average sales per order
    Avg.Profit.Per.Order = mean(Profit),                    # Average profit per order
    Unique.Order.IDs = paste(unique(Order.ID), collapse = ", "), # List of unique Order IDs
    Duplicate.Order.IDs = paste(Order.ID[duplicated(Order.ID)], collapse = ", ") # List of duplicated Order IDs
  )

# View the comparison with more details
print(customer_orders_comparison)

# Find customers with negative profit and summarize the total negative profit per customer
negative_profit_customers <- superstore_df %>%
  filter(Profit < 0) %>%
  group_by(Customer.ID) %>%
  summarise(Total.Negative.Profit = sum(Profit)) %>%
  filter(Total.Negative.Profit < 0)  # Filter customers with negative profit

# View the negative profit customers
print(negative_profit_customers)


# Create a new column to classify profit as positive or negative
library(ggplot2)
superstore_df$Profit.Status <- ifelse(superstore_df$Profit < 0, "Negative", "Positive")

# Create the bar chart
ggplot(superstore_df, aes(x = Profit.Status, fill = Profit.Status)) +
  geom_bar() +
  labs(title = "Comparison of Positive and Negative Profit",
       x = "Profit Status",
       y = "Count of Orders") +
  scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) +
  theme_minimal() +
  theme(legend.position = "none")

# Calculate the average profit
average_profit <- mean(superstore_df$Profit, na.rm = TRUE)

# Print the result
average_profit

# Create the visualization for average profit
ggplot(data.frame(AverageProfit = average_profit), aes(x = "", y = AverageProfit)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste("Avg Profit: $", round(AverageProfit, 2), sep = "")), 
            position = position_stack(vjust = 0.5), color = "white", size = 6) +
  labs(title = "Average Profit of All Orders", x = "", y = "Average Profit") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


# Segmenting by customer category and analyzing discount impact
superstore_df %>%
  group_by(Segment, Discount) %>%
  summarise(Avg_Profit = mean(Profit, na.rm = TRUE), 
            Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  ggplot(aes(x = Discount, y = Avg_Profit, color = Segment)) +
  geom_line() +
  labs(title = "Average Profit by Discount for Different Customer Segments", x = "Discount", y = "Average Profit") +
  theme_minimal()

# Building a Profit Curve
superstore_df %>%
  group_by(Discount) %>%
  summarise(Profit = sum(Profit, na.rm = TRUE), 
            Sales = sum(Sales, na.rm = TRUE)) %>%
  ggplot(aes(x = Discount, y = Profit)) +
  geom_line() +
  labs(title = "Profit Curve: Discount vs Profit", x = "Discount", y = "Profit") +
  theme_minimal()


# Comparison of sales volume and negative profit

profit_vs_sales <- superstore_df %>%
  mutate(Profit.Type = ifelse(Profit < 0, "Negative Profit", "Positive Profit")) %>%
  group_by(Profit.Type) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Quantity = sum(Quantity),
    Avg.Sales.Per.Order = mean(Sales),
    Avg.Quantity.Per.Order = mean(Quantity),
    Total.Orders = n_distinct(Order.ID)
  )

print(profit_vs_sales)


ggplot(superstore_df, aes(x = Sales, y = Profit, color = Profit < 0)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                     labels = c("Positive Profit", "Negative Profit")) +
  labs(
    title = "Sales vs Profit (Individual Orders)",
    x = "Sales ($)",
    y = "Profit ($)",
    color = "Profit Type"
  ) +
  theme_minimal()

# Bar plot for Profit/sales by Category

library(dplyr)
library(ggplot2)
library(tidyr)

# Prepare summary data
category_summary <- superstore_df %>%
  group_by(Category) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit)
  ) %>%
  pivot_longer(cols = c(Total.Sales, Total.Profit), names_to = "Metric", values_to = "Value")

# Plot
ggplot(category_summary, aes(x = Category, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "Sales vs Profit by Category",
    x = "Category",
    y = "Amount ($)",
    fill = "Metric"
  ) +
  scale_fill_manual(values = c("Total.Sales" = "skyblue", "Total.Profit" = "darkgreen")) +
  theme_minimal()


# Bar Chart of sub category and profit/sales
subcategory_summary <- superstore_df %>%
  group_by(Sub.Category) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit)
  ) %>%
  pivot_longer(cols = c(Total.Sales, Total.Profit), names_to = "Metric", values_to = "Value")

ggplot(subcategory_summary, aes(x = reorder(Sub.Category, -Value), y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "Sales vs Profit by Sub-Category",
    x = "Sub-Category",
    y = "Amount ($)",
    fill = "Metric"
  ) +
  scale_fill_manual(values = c("Total.Sales" = "skyblue", "Total.Profit" = "darkgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatterplot sales vs profit
library(ggplot2)

ggplot(superstore_df, aes(x = Sales, y = Profit)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Sales vs Profit (Per Order Line)",
    x = "Sales ($)",
    y = "Profit ($)"
  ) +
  theme_minimal()

cor(superstore_df$Sales, superstore_df$Profit, use = "complete.obs")


# Group sales volume in bins and caompare average profit margin per bin

superstore_df %>%
  mutate(Sales.Bin = cut(Sales, breaks = c(0, 50, 100, 250, 500, 1000, 5000, Inf),
                         labels = c("0-50", "51-100", "101-250", "251-500", "501-1000", "1001-5000", "5000+"))) %>%
  group_by(Sales.Bin) %>%
  summarise(Average.Profit = mean(Profit), Total.Orders = n()) %>%
  arrange(Sales.Bin)

sales_bin_summary <- superstore_df %>%
  mutate(Sales.Bin = cut(Sales, breaks = c(0, 50, 100, 250, 500, 1000, 5000, Inf),
                         labels = c("0-50", "51-100", "101-250", "251-500", "501-1000", "1001-5000", "5000+"))) %>%
  group_by(Sales.Bin) %>%
  summarise(Average.Profit = mean(Profit))

ggplot(sales_bin_summary, aes(x = Sales.Bin, y = Average.Profit)) +
  geom_col(fill = "lightgreen") +
  labs(
    title = "Average Profit by Sales Volume Bin",
    x = "Sales Volume Bin",
    y = "Average Profit ($)"
  ) +
  theme_minimal()

 # Summary table: Sales vs profit by Category

category_summary <- superstore_df %>%
  group_by(Category) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit),
    Avg.Profit = mean(Profit),
    Avg.Sales = mean(Sales),
    Profit.Margin = sum(Profit) / sum(Sales)
  ) %>%
  arrange(desc(Total.Profit))

category_summary

ggplot(category_summary, aes(x = reorder(Category, Total.Sales), y = Total.Profit, fill = Category)) +
  geom_col() +
  labs(title = "Total Profit by Category", x = "Category", y = "Total Profit ($)") +
  theme_minimal()

# Summary table: Sales vs profit by Sub Category
subcategory_summary <- superstore_df %>%
  group_by(Sub.Category) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit),
    Avg.Profit = mean(Profit),
    Avg.Sales = mean(Sales),
    Profit.Margin = sum(Profit) / sum(Sales)
  ) %>%
  arrange(desc(Total.Profit))

subcategory_summary

ggplot(subcategory_summary, aes(x = reorder(Sub.Category, Profit.Margin), y = Profit.Margin, fill = Sub.Category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Profit Margin by Sub-Category", x = "Sub-Category", y = "Profit Margin (%)") +
  theme_minimal()

# Subcategories that result in loss
subcategory_summary %>% filter(Total.Profit < 0)


# Discount Loss analysis of sub categories

discount_loss_analysis <- superstore_df %>%
  group_by(Sub.Category, Discount.Binned) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit),
    Avg.Profit = mean(Profit),
    Profit.Margin = sum(Profit) / sum(Sales),
    Order.Count = n()
  ) %>%
  arrange(Sub.Category, desc(Profit.Margin))

discount_loss_analysis

loss_driving_discounts <- discount_loss_analysis %>%
  filter(Total.Profit < 0)

loss_driving_discounts

# Reorder Discount.Binned so "No Discount" appears first
discount_loss_analysis$Discount.Binned <- factor(
  discount_loss_analysis$Discount.Binned,
  levels = c("No Discount", "0-10%", "11-20%", "21-40%", "41-100%")
)

ggplot(discount_loss_analysis, aes(x = Discount.Binned, y = Profit.Margin, fill = Discount.Binned)) +
  geom_col() +
  facet_wrap(~ Sub.Category, scales = "free_y") +
  labs(title = "Profit Margin by Discount Range per Sub-Category",
       x = "Discount Range", y = "Profit Margin") +
  theme_minimal()

# Determining sales and profit by time period
library(lubridate)


superstore_df <- superstore_df %>%
  mutate(
    Order.Month = month(Order.Date, label = TRUE),
    Order.Quarter = quarter(Order.Date),
    Order.Season = case_when(
      month(Order.Date) %in% c(12, 1, 2) ~ "Winter",
      month(Order.Date) %in% c(3, 4, 5) ~ "Spring",
      month(Order.Date) %in% c(6, 7, 8) ~ "Summer",
      month(Order.Date) %in% c(9, 10, 11) ~ "Fall"
    )
  )

# By Season
season_summary <- superstore_df %>%
  group_by(Order.Season) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit),
    Avg.Profit.Margin = mean(Profit.Margin)
  ) %>%
  arrange(desc(Total.Sales))

ggplot(season_summary, aes(x = Order.Season)) +
  geom_col(aes(y = Total.Sales, fill = "Sales"), position = "dodge", width = 0.5) +
  geom_col(aes(y = Total.Profit, fill = "Profit"), position = "dodge", width = 0.5) +
  labs(title = "Sales and Profit by Season",
       x = "Season", y = "Amount") +
  scale_fill_manual(values = c("Sales" = "skyblue", "Profit" = "seagreen")) +
  theme_minimal()

# By Month
month_summary <- superstore_df %>%
  group_by(Order.Month) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit),
    Avg.Profit.Margin = mean(Profit.Margin)
  ) %>%
  arrange(desc(Total.Sales))

ggplot(month_summary, aes(x = Order.Month)) +
  geom_col(aes(y = Total.Sales, fill = "Sales"), position = "dodge", width = 0.5) +
  geom_col(aes(y = Total.Profit, fill = "Profit"), position = "dodge", width = 0.5) +
  labs(title = "Sales and Profit by Month",
       x = "Month", y = "Amount") +
  scale_fill_manual(values = c("Sales" = "lightblue", "Profit" = "darkgreen")) +
  theme_minimal()

# Determining if discount is impacts by season or month

superstore_df <- superstore_df %>%
  mutate(
    Order.Month = month(Order.Date, label = TRUE),
    Order.Season = case_when(
      month(Order.Date) %in% c(12, 1, 2) ~ "Winter",
      month(Order.Date) %in% c(3, 4, 5) ~ "Spring",
      month(Order.Date) %in% c(6, 7, 8) ~ "Summer",
      month(Order.Date) %in% c(9, 10, 11) ~ "Fall"
    )
  )

# By Season
season_discount_analysis <- superstore_df %>%
  group_by(Order.Season) %>%
  summarise(
    Avg.Discount = mean(Discount),
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit),
    Avg.Profit.Margin = mean(Profit.Margin)
  ) %>%
  arrange(desc(Total.Sales))

ggplot(season_discount_analysis, aes(x = Order.Season)) +
  geom_bar(aes(y = Avg.Discount, fill = "Avg. Discount"), stat = "identity", width = 0.4, position = "dodge") +
  geom_bar(aes(y = Total.Sales / 1000, fill = "Total Sales (in 1000s)"), stat = "identity", width = 0.4, position = "dodge") +
  geom_bar(aes(y = Total.Profit / 1000, fill = "Total Profit (in 1000s)"), stat = "identity", width = 0.4, position = "dodge") +
  labs(title = "Discount Impact by Season",
       x = "Season", y = "Amount (Sales & Profit) / Discount") +
  scale_fill_manual(values = c("Avg. Discount" = "skyblue", "Total Sales (in 1000s)" = "lightgreen", "Total Profit (in 1000s)" = "orange")) +
  theme_minimal()

# By Month
month_discount_analysis <- superstore_df %>%
  group_by(Order.Month) %>%
  summarise(
    Avg.Discount = mean(Discount),
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit),
    Avg.Profit.Margin = mean(Profit.Margin)
  ) %>%
  arrange(desc(Total.Sales))

ggplot(month_discount_analysis, aes(x = Order.Month)) +
  geom_bar(aes(y = Avg.Discount, fill = "Avg. Discount"), stat = "identity", width = 0.4, position = "dodge") +
  geom_bar(aes(y = Total.Sales / 1000, fill = "Total Sales (in 1000s)"), stat = "identity", width = 0.4, position = "dodge") +
  geom_bar(aes(y = Total.Profit / 1000, fill = "Total Profit (in 1000s)"), stat = "identity", width = 0.4, position = "dodge") +
  labs(title = "Discount Impact by Month",
       x = "Month", y = "Amount (Sales & Profit) / Discount") +
  scale_fill_manual(values = c("Avg. Discount" = "skyblue", "Total Sales (in 1000s)" = "lightgreen", "Total Profit (in 1000s)" = "orange")) +
  theme_minimal()

# Comparison of discount, profit/sales during holiday seasons
library(lubridate)

generate_us_holidays <- function(years) {
  holidays <- c(
    as.Date(paste(years, "-01-01", sep = "")),  # New Year's Day
    as.Date(paste(years, "-07-04", sep = "")),  # Independence Day
    as.Date(paste(years, "-09-01", sep = "")),  # Labor Day (Assume it's the 1st Monday)
    as.Date(paste(years, "-11-27", sep = "")),  # Thanksgiving (Assume the 4th Thursday)
    as.Date(paste(years, "-12-25", sep = ""))   # Christmas
  )
  return(holidays)
}

range(year(superstore_df$Order.Date))

us_holidays <- generate_us_holidays(2014:2017)

superstore_df$Holiday_Flag <- ifelse(superstore_df$Order.Date %in% us_holidays, "Holiday", "Non-Holiday")

superstore_df$Year <- year(superstore_df$Order.Date) # Add a Year column 

holiday_comparison <- superstore_df %>%
  group_by(Year, Holiday_Flag, Discount.Binned) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE),
            Total_Profit = sum(Profit, na.rm = TRUE),
            Avg_Sales = mean(Sales, na.rm = TRUE),
            Avg_Profit = mean(Profit, na.rm = TRUE)) %>%
  ungroup() # Summarize data by Year, Holiday Status, and Discount


holiday_comparison # View the summarized data

# Reorder the Discount.Binned factor levels
holiday_comparison$Discount.Binned <- factor(holiday_comparison$Discount.Binned, 
                                             levels = c("No Discount", 
                                                        "0-10%", 
                                                        "11-20%", 
                                                        "21-30%", 
                                                        "31-40%", 
                                                        "41-100%"))

# Plot total sales by year, holiday status, and discount range
ggplot(holiday_comparison, aes(x = Year, y = Total_Sales, fill = Discount.Binned)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Holiday_Flag) +
  labs(title = "Total Sales Comparison by Year and Holiday Status",
       x = "Year", y = "Total Sales") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# Plot total profit by year, holiday status, and discount range
ggplot(holiday_comparison, aes(x = Year, y = Total_Profit, fill = Discount.Binned)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Holiday_Flag) +
  labs(title = "Total Profit Comparison by Year and Holiday Status",
       x = "Year", y = "Total Profit") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


# Comparison of profit and sales with and without discount

# Summarize by season for orders with and without discount
season_discount_comparison <- superstore_df %>%
  mutate(Discount_Status = ifelse(Discount == 0, "No Discount", "With Discount")) %>%
  group_by(Order.Season, Discount_Status) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit),
    Avg.Discount = mean(Discount)
  ) %>%
  arrange(Order.Season)

# Sales/profit comparison with and without discount by season
ggplot(season_discount_comparison, aes(x = Order.Season, y = Total.Sales, fill = Discount_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales Comparison (With vs Without Discount) by Season",
       x = "Season", y = "Total Sales") +
  scale_fill_manual(values = c("With Discount" = "lightblue", "No Discount" = "lightcoral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Discount_Status)

ggplot(season_discount_comparison, aes(x = Order.Season, y = Total.Profit, fill = Discount_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Profit Comparison (With vs Without Discount) by Season",
       x = "Season", y = "Total Profit") +
  scale_fill_manual(values = c("With Discount" = "lightblue", "No Discount" = "lightcoral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Discount_Status)


# Summarize by month for orders with and without discount
month_discount_comparison <- superstore_df %>%
  mutate(Discount_Status = ifelse(Discount == 0, "No Discount", "With Discount")) %>%
  group_by(Order.Month, Discount_Status) %>%
  summarise(
    Total.Sales = sum(Sales),
    Total.Profit = sum(Profit),
    Avg.Discount = mean(Discount)
  ) %>%
  arrange(Order.Month)

# Sales/profit Comparison with and without discount by month
ggplot(month_discount_comparison, aes(x = factor(Order.Month), y = Total.Sales, fill = Discount_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales Comparison (With vs Without Discount) by Month",
       x = "Month", y = "Total Sales") +
  scale_fill_manual(values = c("With Discount" = "lightblue", "No Discount" = "lightcoral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Discount_Status)

ggplot(month_discount_comparison, aes(x = factor(Order.Month), y = Total.Profit, fill = Discount_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Profit Comparison (With vs Without Discount) by Month",
       x = "Month", y = "Total Profit") +
  scale_fill_manual(values = c("With Discount" = "lightblue", "No Discount" = "lightcoral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Discount_Status)

# Impacts of profitability when offering discounts

ggplot(superstore_df, aes(x = Discount, y = Profit)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Sales Profit vs Discount",
       x = "Discount Percentage", y = "Profit")

# Break even analysis for each sub category
loss_subcategories <- superstore_df %>%
  filter(Profit < 0) %>%
  group_by(Sub.Category) %>%
  summarise(Total_Loss = sum(Profit), Avg_Profit_Margin = mean(Profit.Margin), Avg_Sales = mean(Sales))
loss_subcategories <- loss_subcategories %>%
  mutate(Profit_Per_Unit = Avg_Profit_Margin * Avg_Sales)  # Estimated Profit per unit

loss_subcategories <- loss_subcategories %>%
  mutate(BreakEven_Sales_Volume = abs(Total_Loss) / Profit_Per_Unit)  # Break-even sales volume

ggplot(loss_subcategories, aes(x = Sub.Category, y = BreakEven_Sales_Volume, fill = Sub.Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Break-even Sales Volume for Sub-Categories with Losses",
       x = "Sub-Category", y = "Break-even Sales Volume") +
  theme_minimal() +
  coord_flip()  # Flip to make it easier to view

# Comparison of break even volume to actual sales
# Calculate actual sales volume (Total Quantity Sold per Sub-Category)
actual_sales_volume <- superstore_df %>%
  group_by(Sub.Category) %>%
  summarise(Actual_Sales_Volume = sum(Quantity))  # or use sum(Sales) if you prefer sales value


# Merge break-even and actual sales volume data
comparison_data <- loss_subcategories %>%
  left_join(actual_sales_volume, by = "Sub.Category")

ggplot(comparison_data, aes(x = Sub.Category)) +
  geom_bar(aes(y = BreakEven_Sales_Volume, fill = "Break-even Sales Volume"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Actual_Sales_Volume, fill = "Actual Sales Volume"), stat = "identity", position = "dodge") +
  labs(title = "Comparison of Break-even and Actual Sales Volume per Sub-Category",
       x = "Sub-Category", y = "Sales Volume") +
  scale_fill_manual(values = c("Break-even Sales Volume" = "red", "Actual Sales Volume" = "blue")) +
  theme_minimal() +
  coord_flip()  # Flip the plot for better visibility
