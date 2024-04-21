library(rpart)
library(rpart.plot)

# Read the data
train <- read.csv("D:/Uni Resources/Data 101/Project Component 1/EarningsTrain.csv")

set.seed(123)

# Split the data into training and test sets
TrainIndex <- sample(nrow(train), 0.7 * nrow(train))
TrainData <- train[TrainIndex, ]
TestData <- train[-TrainIndex, ]


num_vars <- c("Number_Of_Professional_Connections", "GPA", "Earnings", 
              "Graduation_Year", "Height", "Number_Of_Credits", "Number_Of_Parking_Tickets")


get_out <- function(data, num_vars, threshold = 2.9) {
  for (var in num_vars) {
    z_scores <- abs(scale(data[[var]]))
    data <- data[z_scores < threshold, ]
  }
  return(data)
}

TrainData <- get_out(TrainData, num_vars)

TrainData$pred1 <- (TrainData$GPA * TrainData$Number_Of_Professional_Connections^2)
TrainData$pred3 <- (TrainData$Number_Of_Professional_Connections * TrainData$Graduation_Year)



minBuck <-16
minS <- 15
cup <- 0.0001
dep <- 4

# Build regression tree models all majors
tree1 <- rpart(formula = Earnings ~ (Number_Of_Professional_Connections^2) + pred3, 
               data = TrainData[TrainData$Major == "Other", ], 
               method = "anova",
               control = rpart.control(minsplit = minS, minbucket = minBuck, cp = cup, maxdepth = dep))


tree2 <- rpart(formula = Earnings ~ ., 
               data = TrainData[TrainData$Major == "STEM", ], 
               method = "anova",
               control = rpart.control(minsplit = minS, minbucket = minBuck, cp = cup, maxdepth = dep))

tree3 <- rpart(formula = Earnings ~ ., 
               data = TrainData[TrainData$Major == "Vocational", ], 
               method = "anova",
               control = rpart.control(minsplit = minS, minbucket = minBuck, cp = cup, maxdepth = dep))

tree4 <- rpart(formula = Earnings ~ ., 
               data = TrainData[TrainData$Major == "Buisness", ], 
               method = "anova",
               control = rpart.control(minsplit = minS, minbucket = minBuck, cp = cup, maxdepth = dep))

tree5 <- rpart(formula = Earnings ~ ., 
               data = TrainData[TrainData$Major == "Humanities", ], 
               method = "anova",
               control = rpart.control(minsplit = minS, minbucket = minBuck, cp = cup, maxdepth = dep))

tree6 <- rpart(formula = Earnings ~ ., 
               data = TrainData[TrainData$Major == "Professional", ], 
               method = "anova",
               control = rpart.control(minsplit = minS, minbucket = minBuck, cp = cup, maxdepth = dep))

# Make predictions for all majors separately
TrainData$pred.Earnings[TrainData$Major == "Other"] <- predict(tree1, newdata = TrainData[TrainData$Major == "Other", ])
TrainData$pred.Earnings[TrainData$Major == "STEM"] <- predict(tree2, newdata = TrainData[TrainData$Major == "STEM", ])
TrainData$pred.Earnings[TrainData$Major == "Vocational"] <- predict(tree3, newdata = TrainData[TrainData$Major == "Vocational",])
TrainData$pred.Earnings[TrainData$Major == "Buisness"] <- predict(tree4, newdata = TrainData[TrainData$Major == "Buisness", ])
TrainData$pred.Earnings[TrainData$Major == "Humanities"] <- predict(tree5, newdata = TrainData[TrainData$Major == "Humanities", ])
TrainData$pred.Earnings[TrainData$Major == "Professional"] <- predict(tree6, newdata = TrainData[TrainData$Major == "Professional", ])


# Calculate MSE for combined predictions
mse <- mean((TrainData$Earnings - TrainData$pred.Earnings)^2)

print(mse)
