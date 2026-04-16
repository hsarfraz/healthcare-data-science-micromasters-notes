# program summary
# unsupervised learning -> 
# 

## Load necessary libraries
library(caret)
library(dplyr)
library(pROC)

################################################
# STEP 1 PRE-PROCESSING AND DATA CURATION 
################################################

# Simulate healthcare data (100 patients)
# We are setting the base for the sequence of numbers in the dataset
# The seed allocates a certain sequence of fixed numbers to the dataset

set.seed(123) 

# setting the dataset

data <- data.frame(
  Age = sample(40:80, 100, replace = TRUE), #ages between 40-80
  
  Cholesterol = rnorm(100, mean = 200, sd = 30),
  
  BloodPressure = rnorm(100, mean = 120, sd = 15),
  
  Outcome = sample(0:1, 100, replace = TRUE)
)


# Introduce some missing values randomly

data$Cholesterol[sample(1:100, 10)] <- NA #10 values from 1-100 index will be given NA


# Handle missing values by imputing median 
# mutate_all() goes through all columns
# the period represents the specific column in the iteration
# it is good practice to go through every column when replacing NAs

data <- data %>% mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) #mutate all applies rule to all column and period is used to specify column iteration 


# normalise numeric features
# we are basically setting all the numeric values to a common scale
# all features have a mean of 0 and standard deviation of 1
# it is important to normalise all columns

preProcess_data <- preProcess(data[, c("Age", "Cholesterol", "BloodPressure")],
                              method = c("center", "scale")) #center + scale makes mean: 0 + SD: 1

normalised_data <- predict(preProcess_data, data)


# Append Outcome column back to the normalised dataset

normalised_data$Outcome <- data$Outcome

################################################
# STEP 2 DEEP LEARNING MODEL
################################################

# Load library

library(randomForest)

# Split data into training and testing sets

set.seed(123)

trainIndex <- createDataPartition(normalised_data$Outcome, p = 0.8, list = FALSE)

trainData <- normalised_data[trainIndex, ]

testData <- normalised_data[-trainIndex, ]

trainData$Outcome <- as.factor(trainData$Outcome)
testData$Outcome  <- as.factor(testData$Outcome)


# Train a random forest model
#random forest is supervised learning

rf_model <- randomForest(
  
  Outcome ~ Age + Cholesterol + BloodPressure, 
  
  data = trainData, 
  
  ntree = 100  # Number of trees
  
)


# Predict on test data

rf_predictions <- predict(rf_model, testData, type = "response")


# Evaluate model performance

conf_matrix <- table(testData$Outcome, rf_predictions)

print(conf_matrix)


# you can also visualise the confusion matrix on a heatmap
# converting from wide format (matrix) to long format (tidy data)


conf_df <- as.data.frame(as.table(conf_matrix))

colnames(conf_df) <- c("Actual", "Predicted", "Count")


# Plot confusion matrix as a heatmap

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
  
  geom_tile() +
  
  geom_text(aes(label = Count), color = "white", size = 6) +
  
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  
  labs(title = "Confusion Matrix Heatmap", x = "Actual Label", y = "Predicted Label") +
  
  theme_minimal()