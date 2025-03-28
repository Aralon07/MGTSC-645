### Load libraries ###
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(MASS)
install.packages("tseries") #ADF test for stationarity
library(tseries)

#### Data Clean-up ####

# Load the dataset
superstore_df <- read.csv(file.choose())

# Remove irrelevant variables (7 Variables)
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

# Check if profit is stationary (p>0.05 = non-stationary)
adf_test_result <- adf.test(superstore_df$Profit)
print(adf_test_result) # pvalue = 0.01 (stationary)

plot(superstore_df$Profit, type = "l", main = "Profit Over Time")

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

#### Data Clean-up ENDED ####


### Linear Regression ###

# For Sales
lm_profit_sales <- lm(Profit ~ Sales, data = superstore_df)
plot(lm_profit_sales, which = 1, main = "Residuals vs Fitted - Sales Model")

summary(lm_profit_sales)

# For Quantity
lm_profit_quantity <- lm(Profit ~ Quantity, data = superstore_df)
plot(lm_profit_quantity, which = 1, main = "Residuals vs Fitted - Quantity Model")

summary(lm_profit_quantity)

# For Discount
lm_profit_discount <- lm(Profit ~ Discount, data = superstore_df)
plot(lm_profit_discount, which = 1, main = "Residuals vs Fitted - Discount Model")

summary(lm_profit_discount)

# For Ship.Mode
lm_profit_ship_mode <- lm(Profit ~ Ship.Mode, data = superstore_df)
plot(lm_profit_ship_mode, which = 1, main = "Residuals vs Fitted - Ship.Mode Model")

summary(lm_profit_ship_mode)

# For Segment
lm_profit_segment <- lm(Profit ~ Segment, data = superstore_df)
plot(lm_profit_segment, which = 1, main = "Residuals vs Fitted - Segment Model")

summary(lm_profit_segment)

# For Region
lm_profit_region <- lm(Profit ~ Region, data = superstore_df)
plot(lm_profit_region, which = 1, main = "Residuals vs Fitted - Region Model")

summary(lm_profit_region)

# For Category
lm_profit_category <- lm(Profit ~ Category, data = superstore_df)
plot(lm_profit_category, which = 1, main = "Residuals vs Fitted - Category Model")

summary(lm_profit_category)

# For Sub.Category
lm_profit_sub_category <- lm(Profit ~ Sub.Category, data = superstore_df)
summary(lm_profit_sub_category)

# For Shipping.Delay(# Does longer shipping delay affect profitability)
lm_profit_shipping_delay <- lm(Profit ~ Shipping.Delay, data = superstore_df)
plot(lm_profit_shipping_delay, which = 1, main = "Residuals vs Fitted - Shipping Delay Model")

summary(lm_profit_shipping_delay)

# Linear Regression - Profit; All other applicable variables
lm_profit_model <- lm(Profit ~ Sales + Quantity + Discount + Ship.Mode + Segment + Region + Category + Sub.Category  + Shipping.Delay, 
                data = superstore_df)
plot(lm_profit_model, which = 1, main = "Residuals vs Fitted - Full Model")

summary(lm_profit_model)

stepwise_model <- step(lm_profit_model, direction = "both", trace = 1)
summary(stepwise_model)
vif(stepwise_model)
# Q-Q Plot for Residuals
qqnorm(residuals(stepwise_model))  # Create Q-Q plot
qqline(residuals(stepwise_model), col = "red", lwd = 2)


# Three-way interaction between Sales, Region, and Discount
lm_model_three_way <- lm(Profit ~ Sales * Region * Discount* Quantity,
                         data = superstore_df)

# Summary of the four-way interaction model
summary(lm_model_three_way)

# Stepwise model selection (including interaction terms)
stepwise_model <- step(lm(Profit ~ Sales * Region * Discount* Quantity,
                          data = superstore_df), direction = "both", trace = 1)

summary(stepwise_model)

# Plot residuals for four way interaction
plot(lm_model_three_way, which = 1, main = "Residuals vs Fitted - Stepwise Model")
plot(lm_model_three_way, which = 2, main = "Normal Q-Q Plot - Stepwise Model")


### Linear Regression ENDED ###


### Linear Regression with Interaction Terms ###

# Three-way interaction between Sales, Region, and Discount
lm_model_three_way <- lm(Profit ~ Sales * Region * Discount* Quantity,
                         data = superstore_df)

summary(lm_model_three_way)

# Extract residuals and fitted values
residuals_df <- data.frame(
  Fitted = fitted(lm_model_three_way),
  Residuals = residuals(lm_model_three_way)
)

# 1. Residuals vs. Fitted Plot
ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# 2. Histogram of Residuals
ggplot(residuals_df, aes(x = Residuals)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

# 3. Q-Q Plot (Normality Check)
qqnorm(residuals_df$Residuals, main = "Q-Q Plot of Residuals")
qqline(residuals_df$Residuals, col = "red")

plot(lm_model_three_way$fitted.values, sqrt(abs(residuals(lm_model_three_way))), 
     main = "Scale-Location Plot",
     xlab = "Fitted Values", 
     ylab = "√|Residuals|")
abline(h = 0, col = "red")

library(lmtest)
dwtest(lm_model_three_way)

library(car)
vif(lm_model_three_way)





# Model with decided interaction terms
model_interaction <- lm(Profit ~ Sales + Quantity + Discount + Region + Category +
                          Sales:Region + Discount:Category + Quantity:Category + Segment:Category, data = superstore_df)
summary(model_interaction)


# Model with only significant interaction terms
model_refined <- lm(Profit ~ Sales + Quantity + Discount + Region + Category +
                      Sales:Region + Discount:Category +
                      Quantity:Category, data = superstore_df)

summary(model_refined)

# Model with interaction terms (Discount interactions included on refined) ***highest r2

model_interaction_discount <- lm(Profit ~ Sales + Quantity + Discount + Region + Category +
                                   Sales:Region + Discount:Category + Quantity:Category  + Discount:Quantity
                                 + Discount:Sales + Discount:Region, data = superstore_df)
summary(model_interaction_discount)
vif(model_interaction_discount)


# Model with interaction terms (Discount interaction focused)

model_interaction_discount_1 <- lm(Profit ~ Sales + Quantity + Discount  + Category + Region +
                                  Discount:Category  + Discount:Quantity
                                 + Discount:Sales + Discount:Region, data = superstore_df)

summary(model_interaction_discount_1)
vif(model_interaction_discount_1)

# Plot residuals vs fitted values
plot(model_interaction_discount, which = 1)

# Normal Q-Q plot to check for normality of residuals
plot(model_interaction_discount, which = 2)
# Shapiro-Wilk test for normality
shapiro.test(residuals(model_interaction_discount))

# Alternatively, use a Q-Q plot
qqnorm(residuals(model_interaction_discount))
qqline(residuals(model_interaction_discount), col = "red")

# Scale-Location plot to check for homoscedasticity
plot(model_interaction_discount, which = 3)
bptest(model_interaction_discount)

# Residuals vs leverage to detect influential data points
plot(model_interaction_discount, which = 5)

### Linear Regression with Interaction Terms ENDED ###


### Plotting interaction terms with profit ### 

# Predict values of Profit using the model
superstore_df$Predicted_Profit <- predict(model_interaction_final_2, newdata = superstore_df)

# Plot interaction between Quantity and Category
ggplot(superstore_df, aes(x = Quantity, y = Predicted_Profit, color = Category)) +
  geom_line(size = 1) +  # Draw lines for different categories
  labs(title = "Interaction Effect of Quantity and Category on Profit",
       x = "Quantity",
       y = "Predicted Profit",
       color = "Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot interaction between Sales and Discount
ggplot(superstore_df, aes(x = Sales, y = Predicted_Profit, color = Discount)) +
  geom_line(size = 1) +  # Draw lines for different discount levels
  labs(title = "Interaction Effect of Sales and Discount on Profit",
       x = "Sales",
       y = "Predicted Profit",
       color = "Discount") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Plot interaction between Discount and Region
ggplot(superstore_df, aes(x = Discount, y = Predicted_Profit, color = Region)) +
  geom_line(size = 1) +  # Draw lines for different regions
  labs(title = "Interaction Effect of Discount and Region on Profit",
       x = "Discount",
       y = "Predicted Profit",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot interaction between Quantity and Category
ggplot(superstore_df, aes(x = Quantity, y = Predicted_Profit, color = Category)) +
  geom_line(size = 1) +  # Draw lines for different categories
  labs(title = "Interaction Effect of Quantity and Category on Profit",
       x = "Quantity",
       y = "Predicted Profit",
       color = "Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

### Plotting interaction terms with profit ENDED ### 


### Linear Regression with Interaction Terms FINAL ###
library(lubridate)

# Assuming df has an "Order Date" column in Date format or character format
superstore_df$Order.DayOfWeek <- weekdays(as.Date(superstore_df$`Order.Date`))

# Convert to factor to ensure proper handling in modeling
superstore_df$Order.DayOfWeek <- factor(superstore_df$Order.DayOfWeek, 
                               levels = c("Monday", "Tuesday", "Wednesday", 
                                          "Thursday", "Friday", "Saturday", "Sunday"))

# Model with only significant interaction terms
model_interaction_final <- lm(Profit ~ Sales + Quantity + Discount + Shipping.Delay + 
                          Region + Category + Order.DayOfWeek +
                          Sales:Region + Discount:Category + 
                          Quantity:Category + Sales:Discount + 
                          Discount:Region, data = superstore_df)

  # Summary of the model
  summary(model_interaction_final) 
  vif(model_interaction_final)
  
  # Perform stepwise selection (backward elimination)
  model_interaction_backward <- stepAIC(model_interaction_final, direction = "backward")
  summary(model_interaction_backward)
  
  # Perform stepwise selection (both)
  model_interaction_stepwise <- stepAIC(model_interaction_final, direction = "both")
  summary(model_interaction_stepwise)
  vif(model_interaction_stepwise)
  
  
# Removed insignificant variables
model_interaction_final_2 <- lm(Profit ~ Sales + Quantity + Discount  + 
                                  Region +
                                  Sales:Region + Discount:Category + 
                                  Quantity:Category + Sales:Discount + 
                                  Discount:Region, data = superstore_df)

  model_interaction_stepwise_2 <- stepAIC(model_interaction_final_2, direction = "both", trace = TRUE)
  summary(model_interaction_stepwise_2)
  
  
  # Summary of the model
  summary(model_interaction_final_2) 
  vif(model_interaction_final_2)


# QQ plot to check normality of residuals
qqnorm(residuals(model_interaction_stepwise))

# Histogram to check normality visually
hist(residuals(model_interaction_stepwise), 
     breaks = 30, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "skyblue", 
     border = "black")

plot(fitted(model_interaction_stepwise), residuals(model_interaction_stepwise),
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20, col = "blue")

# Residuals vs Fitted plot
plot(fitted(model_interaction_stepwise), residuals(model_interaction_stepwise),
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)

# Durbin-Watson test to check for autocorrelation
library(lmtest)
dwtest(model_interaction_stepwise)



# Plot residuals over time (e.g., Order.Date)
plot(superstore_df$Order.Date, residuals(model_interaction_stepwise), 
     xlab = "Order Date", ylab = "Residuals", 
     main = "Residuals Over Time")
abline(h = 0, col = "red", lty = 2)

# Convert Order.Date to Date format (if not already)
superstore_df$Order.Date <- as.Date(superstore_df$Order.Date)

# Plot residuals over time (Order.Date)
plot(superstore_df$Order.Date, residuals(model_interaction_stepwise), 
     xlab = "Order Date", ylab = "Residuals", 
     main = "Residuals Over Time")
abline(h = 0, col = "red", lty = 2)

# Add a QQ plot to check normality of residuals (you can do this separately)
# QQ
qqnorm(residuals(model_interaction_stepwise))
qqline(residuals(model_interaction_stepwise), col = "red", lty = 2)

### Linear Regression with Interaction Terms FINAL ENDED ###

### Robust Regression ###

library(MASS)
model_robust <- rlm(Profit ~ Sales + Quantity + Discount + Shipping.Delay + 
                      Region + Category + Order.DayOfWeek, data = superstore_df)

summary(model_robust)

# Calculate Z-scores
z_scores <- scale(superstore_df$Profit)
outliers <- which(abs(z_scores) > 3)  # Identifying outliers with a Z-score greater than 3

# Remove outliers
superstore_df_clean <- superstore_df[-outliers, ]

### NON PARAMETRIC TESTS ###

# For categorical predictors with more than two levels. Testing if profit differs across groups without normality assumption
kruskal.test(Profit ~ Region, data = superstore_df)
kruskal.test(Profit ~ Category, data = superstore_df)
kruskal.test(Profit ~ Quantity, data = superstore_df)

# Dunn's Test for pairwise comparisons
install.packages("dunn.test")
library(dunn.test)
dunn.test(superstore_df$Profit, superstore_df$Region, method = "bonferroni")
dunn.test(superstore_df$Profit, superstore_df$Category, method = "bonferroni")
dunn.test(superstore_df$Profit, superstore_df$Quantity, method = "bonferroni")


# Spearman's Rank Correlation between Sales and Profit
cor.test(superstore_df$Sales, superstore_df$Profit, method = "spearman")

# Spearman's Rank Correlation between Discount and Profit
cor.test(superstore_df$Discount, superstore_df$Profit, method = "spearman")


### NON PARAMETRIC TESTS ENDED###


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


 


