library(rpart)
library(rpart.plot)

# Read the data
TrainData <- read.csv("D:/Uni Resources/Data 101/Project Component 1/EarningsTrain.csv")

test <- read.csv("D:/Uni Resources/Data 101/Project Component 1/EarningsTest.csv")

set.seed(123)

# Split the data into training and test sets


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

test$pred1 <- (test$GPA * test$Number_Of_Professional_Connections^2)
test$pred3 <- (test$Number_Of_Professional_Connections * test$Graduation_Year)


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
test$pred.Earnings[test$Major == "Other"] <- predict(tree1, newdata = test[test$Major == "Other", ])
test$pred.Earnings[test$Major == "STEM"] <- predict(tree2, newdata = test[test$Major == "STEM", ])
test$pred.Earnings[test$Major == "Vocational"] <- predict(tree3, newdata = test[test$Major == "Vocational", ])
test$pred.Earnings[test$Major == "Buisness"] <- predict(tree4, newdata = test[test$Major == "Buisness", ])
test$pred.Earnings[test$Major == "Humanities"] <- predict(tree5, newdata = test[test$Major == "Humanities", ])
test$pred.Earnings[test$Major == "Professional"] <- predict(tree6, newdata = test[test$Major == "Professional", ])


write.csv(test, file = "D:/Uni Resources/Data 101/Project Component 1/EarningsTest_Predictions.csv", row.names = FALSE)


