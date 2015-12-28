# GCD-Project
Repository for the Getting and Cleaning Data course project
## You should create one R script called run_analysis.R that does the following. 
## 1. Merges the training and the test sets to create one data set.

#load necessary libraries
library(dplyr, rapportools)

# Read data sets into R
  features <- read.table("features.txt")
  activity <- read.table("activity_labels.txt")
  y_train <- read.table("y_train.txt")
  y_test <- read.table("y_test.txt")
  X_test <- read.table("X_test.txt")
  X_train <- read.table("X_train.txt")
  subj_test <- read.table("subject_test.txt")
  subj_train <- read.table("subject_train.txt")

# Merge training and test data sets
  bind_tt <- bind_rows(X_train, X_test)

# Add variable names from features dataset
  colnames(bind_tt) <- features$V2

## 2. Extracts only the measurements on the mean and standard deviation for each 
##    measurement. 

  extractvars <- features[grep("(mean|std)\\(", features[,2]),]
  mean_std <- bind_tt[,extractvars[,1]]
  
## 3. Uses descriptive activity names to name the activities in the data set

#combine training and test label numbers
  bind_names <- bind_rows(y_train, y_test)
  
#give the numbers corresponding activity labels
  for (i in 1:nrow(activity)) {
    num <- as.numeric(activity[i, 1])
    label <- as.character(activity[i, 2])
    bind_names[bind_names$V1 == num, ] <- label
  }

#combine labels with mean_std data set
  labeled <- cbind(bind_names, mean_std)

## 4. Appropriately labels the data set with descriptive variable names.
  
#Make names more descriptive
  VariableNames <- tocamel(names(labeled), delim = "\\.|\\_", upper = TRUE,
                           sep = "")
  VariableNames1 <- gsub("Acc", "Acceleration", VariableNames)
  VariableNames2 <- gsub("Mag", "Magnitude", VariableNames1)
  VariableNames3 <- gsub("Gyro", "Gyroscope", VariableNames2)
  VariableNames4 <- gsub("BodyBody", "Body", VariableNames3)
  VariableNames5 <- gsub("Std", "StandardDeviation", VariableNames4)
  VariableNames6 <- gsub("Freq", "Frequency", VariableNames5)

#Apply names to data set
  names(labeled) <- VariableNames6
  
## 5. From the data set in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject
  
  bind_subj <- bind_rows(subj_train, subj_test)
  subj_activity_avg <- aggregate(labeled, by = list(activity = labeled[,1], 
                                     subject = bind_subj[,1]), mean)
  
  write.csv(subj_activity_avg, file='GCDproject.txt', row.names=FALSE)
