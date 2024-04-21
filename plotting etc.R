# Read the data
data <- read.csv("D:/Uni Resources/Data 101/Project Component 1/EarningsTrain.csv")

# Summary statistics
summary(data)

# Select only numeric columns
numeric_data <- data[, sapply(data, is.numeric)]

# Summary statistics
summary(numeric_data)

# Correlation analysis
correlation_matrix <- cor(numeric_data)
print(correlation_matrix)

# Visualization (scatter plot)
plot(numeric_data$GPA, numeric_data$Earnings, xlab = "GPA", ylab = "Earnings", main = "Scatter Plot")

# Visualization (histogram)
hist(numeric_data$Earnings, main = "Histogram of Earnings", xlab = "Earnings")

# Visualization (box plot)
boxplot(numeric_data$Earnings ~ data$Major, main = "Boxplot of Earnings by Major", xlab = "Major", ylab = "Earnings")

# Outlier detection and removal (using Z-score)
z <- abs(scale(numeric_data$Earnings))
threshold <- 3
cleaned_data <- data[z < threshold, ]

# Check the new summary statistics after removing outliers
summary(cleaned_data)