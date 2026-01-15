# LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

# Section 5: Loading, transforming and visualising data in R

# Import libraries and packages
# install.packages("dplyr")
# install.packages("ggplot2", dependencies=TRUE)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(moments)
library(corrplot) #install.packages("corrplot")

#library(mgcv)

# Set working directory (update path as needed)
setwd("/Users/adyat/OneDrive/Desktop/Adrian/LSE Data Analytics Career Accelerator/Course 3/R Scripts")

# Import Data
reviews <- read.csv("/Users/adyat/OneDrive/Desktop/Adrian/LSE Data Analytics Career Accelerator/Course 3/R Scripts/turtle_reviews.csv")

# Sense-check the DataFrame
head(reviews)
str(reviews)

# Check for any missing values
colSums(is.na(reviews))

# Create a summary of the new DataFrame
summary(reviews) 

# Drop unnecessary columns
reviews <- reviews %>%
  select(-language, -platform)

# Rename the data
reviews <- reviews %>%
  rename(`remuneration` = `remuneration..k..`,
         `spending_score` = `spending_score..1.100.`)

# View the new column names
names(reviews)

###############################################################################

# Perform exploratory data analysis:

# Distribution of loyalty points
plot_1 <- ggplot(reviews, aes(x = loyalty_points)) +
  geom_histogram(binwidth = 150, color = "blue", alpha = 0.7) +
  labs(title = "Loyalty Points: Overall Distribution", x = "Loyalty Points", y = "Frequency")
plot_1

# Scatterplot of Loyalty Points vs Income
plot_2 <- ggplot(reviews, aes(x = remuneration, y = loyalty_points)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(title = "Loyalty Points vs Income", x = "Annual Income (£000)", y = "Loyalty Points")
plot_2

# Scatterplot of Loyalty Points vs Spending Score
plot_3 <- ggplot(reviews, aes(x = spending_score, y = loyalty_points)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(title = "Loyalty Points vs Spending Score", x = "Spending Score", y = "Loyalty Points")
plot_3

# Boxplot of Loyalty Points Distribution
plot_4 <- ggplot(reviews, aes(x="", y = loyalty_points)) +
  geom_boxplot(color = "blue") +
  labs(title = "Loyalty Points: Range and Outliers",
       x = "",
       y = "Loyalty Points"
       )
plot_4

# Save all plots using a loop
plots <- list(
  plot_1 = plot_1,
  plot_2 = plot_2,
  plot_3 = plot_3,
  plot_4 = plot_4
)

# Save to an exportable format
for (name in names(plots)) {
  message("Saving: ", name)
  ggsave(
    filename = paste0(name, ".png"),
    plot = plots[[name]],
    width = 6,
    height = 4,
    dpi = 300
  )
}

# Observations/ Recommendations:
# Visual exploration of loyalty points indicates a right-skewed distribution, 
# with most customers accumulating a moderate number of points and
# a small proportion of customers exhibiting an exceptionally high number of loyalty points. 
# Scatterplots show a positive relationship between both remuneration
# and spending score with loyalty points; 
# however, the association is stronger for spending score. 
# This suggests that behavioural engagement, represented by spending score, 
# plays a more direct role in loyalty point accumulation than income alone. 
# The few customers with extremely high loyalty points may represent a unique group, 
# and further investigation could help explain their behaviour.

###############################################################################

# Section 6: Statistical analysis and modelling in R

# Calculate mean, median, and mode
mean_score <- mean(reviews$loyalty_points)
median_score <- median(reviews$loyalty_points)
mode_score <- as.numeric(names(sort(table(reviews$loyalty_points), decreasing = TRUE)[1]))

# Calculate Range
range_loyalty_points <- range(reviews$loyalty_points)

# Calculate Difference between highest and lowest values
difference_high_low <- diff(range_loyalty_points)

# Calculate Interquartile Range (IQR)
iqr_loyalty_points <- IQR(reviews$loyalty_points)

# Calculate Variance
variance_loyalty_points <- var(reviews$loyalty_points)

# Calculate Standard Deviation
std_deviation_loyalty_points <- sd(reviews$loyalty_points)

# Display results
list(
  Range = range_loyalty_points,
  Difference = difference_high_low,
  IQR = iqr_loyalty_points,
  Variance = variance_loyalty_points,
  Standard_Deviation = std_deviation_loyalty_points
  )

# Check the distribution shape:

# Skewness and Kurtosis
skewness(reviews$loyalty_points)
kurtosis(reviews$loyalty_points)

# Shapiro-Wilk test for normality
shapiro.test(reviews$loyalty_points)

# Observations:
# The loyalty_points variable shows substantial variability, 
# ranging from 25 to 6,847, with a fairly large spread (SD = 1283). 
# The distribution is strongly right-skewed (skewness = 1.46), 
# meaning most customers earn a fairly modest number of loyalty points, 
# while a smaller group earn much higher amounts, consistent with earlier visual analysis.
# The kurtosis value (4.71) suggests heavier tails and a sharper peak, 
# which confirms that there are some extreme values in the data. 
# This matches what we saw earlier in the visuals: 
# the histogram showed a long right tail, 
# and the boxplot highlighted several high-value cases. 
# The Shapiro–Wilk test (W = 0.84, p < 0.001) also confirms
# that the data is not normally distributed.

###############################################################################

# Check for correlations among features:

# Drop unnecessary columns for correlation analysis
reviews_numeric_data <- reviews %>%
  select_if(is.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(reviews_numeric_data, use = "complete.obs") 

# Create the correlation plot
corrplot(correlation_matrix, method = "circle")

# View the correlation matrix
correlation_matrix

# Round the output
round(correlation_matrix, 2)

# Observations:
# The correlation matrix shows that loyalty_points has a moderate to strong relationship 
# with both remuneration (r = 0.62) and spending_score (r = 0.67). 
# This suggests that both variables are likely to be useful 
# in explaining how customers accumulate loyalty points. 
# However, remuneration and spending_score are not strongly correlated with each other (r ≈ 0), 
# meaning there is no evidence of multicollinearity risk. 
# In other words, each variable seems to add its own unique information 
# and both are suitable to include in the regression model.

###############################################################################

# Perform Multiple Linear Regression:

# Create the multiple linear regression model
model <- lm(loyalty_points ~ remuneration + spending_score, data = reviews)

# Summarize the model
summary(model)

# Observations:

# Residuals:
# Most residuals fall between roughly –364 and +281, 
# meaning the majority of predictions are reasonably close to the actual loyalty point values.
# The range extends from –1646 to +1999, suggesting there are some customers 
# where the model under- or over-predicts loyalty points more substantially. 
# These could represent unusual or extreme behaviours, likely linked to specific
# customer types or segments identified earlier in the clustering analysis.

# Coefficients:
# Both remuneration (p < 2e-16) and spending_score (p < 2e-16) are highly statistically 
# significant predictors of loyalty_points.
# The coefficients indicate that, holding other factors constant:
# for every 1-unit increase in remuneration, loyalty_points increase by about 34 points, and
# for every 1-unit increase in spending_score, loyalty_points increase by about 33 points.

# Fit:
# The R-squared = 0.8269 and Adjusted R-squared = 0.8267 
# indicate that the model explains about 83% of the variation in loyalty_points.
# This is a strong fit, meaning the predictors do a good job 
# of explaining customer loyalty point accumulation.

# F-statistic:
# The F-statistic is very large (4770 with p < 2e-16), meaning the model as a whole is statistically significant.
# This confirms that at least one of the predictors is meaningfully related to loyalty_points, 
# and in this case both clearly are.

# Overall comments:
# The model explains a high proportion of variance in loyalty_points 
# and both predictors are highly significant. 
# This suggests it is a very effective tool for predicting loyalty accumulation at an overall population level.

# Turtle Games could use this model to estimate expected loyalty points for different customer profiles,
# identify customers likely to generate higher loyalty engagement, and 
# support loyalty programme strategy and targeting.

# These results align well with the earlier analysis. 
# Week 1 identified remuneration and spending as meaningful drivers, 
# and the regression here clearly quantifies their importance, 
# explaining most of the variation in loyalty points. 
# At the same time, the presence of unusually high or unexpected cases 
# fits with the Week 2 and Week 3 findings, 
# where behaviour was shown to be complex and customers formed distinct segments.


# Visualise the model

# Plot actual vs. predicted values:
plot_5 <- ggplot(reviews, aes(x = loyalty_points, y = predict(model, reviews))) +
  geom_point(color = "blue", alpha = 0.6) +
  stat_smooth(method = "loess") +  
  labs(x = 'Actual Loyalty Points', y = 'Predicted Loyalty Points') +
  ggtitle('Actual vs. Predicted Loyalty Points')

# Save to an exportable format.
ggsave(
  filename = "plot_5.png",
  plot = plot_5,
  width = 6,
  height = 4,
  dpi = 300
)

#Observations:
# The Actual vs Predicted plot supports the earlier statistical results, 
# showing that the model fits the data very well overall. 
# The strong upward pattern matches the high R² value, 
# meaning the model explains most of the variation in loyalty points. 
# There is some slight curvature at very low loyalty values 
# and a bit more spread around mid-range values, 
# which echoes the residual range in the summary statistics. 
# However, these issues are fairly limited, and the model still performs strongly 
# as a practical tool for predicting loyalty point accumulation.
