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
# STEP 2 UNSUPERVISED LEARNING MODEL
################################################

# k means clustering

# Select only numeric variables for clustering
# the scale function standardizes the values around zero
# all values in the scale function are comparable by variation 

clustering_data <- scale(normalised_data[, c("Age", "Cholesterol", "BloodPressure")])


# Apply K-means clustering (assuming 2 clusters for simplicity)

set.seed(123)

kmeans_model <- kmeans(clustering_data, centers = 2)


# Add cluster labels to dataset
# you compare clusters manually and observe which groups they were paired with

normalised_data$Cluster <- kmeans_model$cluster


# Print cluster assignments

head(normalised_data)

# table to see cluster categories 
# rows - columns

table(normalised_data$Cluster, normalised_data$Outcome)