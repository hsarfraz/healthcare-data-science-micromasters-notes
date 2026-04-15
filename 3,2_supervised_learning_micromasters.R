# program summary
# supervised learning -> 
# classification (logistic regression 0/1) -> 
# confusion matrix -> 
# ROC curve

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
# STEP 2 SUPERVISED LEARNING MODEL
################################################

# supervised learning is when the ML is given the data with labels/answers
# Split data into training and testing sets

set.seed(123) # want to ensure that the numbers in this section are the same

# taking 80% of rows indexes of the entire dataset
# the proportion of the outcome class is also preserved
# 80% of 1's are included in the training dataset
# 80% of 0's are included in the training dataset 

trainIndex <- createDataPartition(normalised_data$Outcome, p = 0.8, list = FALSE)

trainData <- normalised_data[trainIndex, ] # includes data with the indexes specified

testData <- normalised_data[-trainIndex, ] # minus sign includes indexes not in dataset


# Train logistic regression model
# the outcome column is the dependent variable/y axis/outcome
# the other columns are the independent cariables/x axis/inputs
# binomial means that a logistic regression is used (classification of 0 & 1)

model <- glm(Outcome ~ Age + Cholesterol + BloodPressure, data = trainData, family = binomial)


# Predict on test data
# gives a probability score from 0-1

predictions <- predict(model, testData, type = "response") # can stop here if you want

# after training
# Convert probabilities to binary outcomes (0 or 1)
# if probability is greater than 0.5 then it has disease
# if probability is lower than 0.5 then you don't have disease
# setting threshold

predicted_classes <- ifelse(predictions > 0.5, 1, 0)


# Evaluate model performance using a confusion matrix
# top left: true negative
# top right: false positive (patient doesn't have disease but model said they do)
# bottom left: false negative (patient has disease but model said they don't)
# bottom right: true positive 

conf_matrix <- table(testData$Outcome, predicted_classes) # rows, columns

print(conf_matrix)

# Compute ROC curve
# ROC curve gives us an idea of the ML models performance in predicting disease presence
# sensitivity: true positive rate (true positive/(true positive + False negative))
# specificity: true negative / (true negative + false positive)
# false positive rate: false positive/(false positive + true negative) OR (1 - specificity)

roc_curve <- roc(testData$Outcome, predictions)

# y-axis: sensitivity or true positive rate
# x-axis: 1-specificity or false positive rate

plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression", legacy.axes = TRUE)

# the code below obtains the exact threshold rate and coordinates

coords(roc_curve, x = "all", ret = c("threshold", "sensitivity", "specificity"))