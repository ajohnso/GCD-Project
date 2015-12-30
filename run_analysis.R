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
  
#Add variable name to activity dataset
  colnames(activity) <- c("activitynumber", "activityname")


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
    label <- as.factor(activity[i, 2])
    bind_names[bind_names$activitynumber == num, ] <- label
  }

#combine labels with mean_std data set
  labeled <- cbind(bind_names, mean_std)

## 4. Appropriately labels the data set with descriptive variable names.
  
#Make names more descriptive
  
  VariableNames <- tolower(names(labeled))
  VariableNames1 <- gsub("^t", "time", VariableNames)
  VariableNames2 <- gsub("^f", "frequency", VariableNames1)
  VariableNames3 <- gsub("acc", "acceleration", VariableNames2)
  VariableNames4 <- gsub("mag", "magnitude", VariableNames3)
  VariableNames5 <- gsub("gyro", "gyroscope", VariableNames4)
  VariableNames6 <- gsub("bodybody", "body", VariableNames5)
  VariableNames7 <- gsub("std", "standarddeviation", VariableNames6)
  VariableNames8 <- gsub("freq", "frequency", VariableNames7)
  VariableNames9 <- gsub("v1", "activity", VariableNames8)
  VariableNames10 <- gsub("-", "", VariableNames9)

#Apply names to data set
  names(labeled) <- VariableNames10
  
## 5. From the data set in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject
  
  bind_subj <- bind_rows(subj_train, subj_test)
  
#Give descriptive variable name to merged subject dataset
  colnames(bind_subj) <- "subject"
  
# Merge subject dataset with labeled activty and measurements dataset
  subj_activity_avg <- bind_cols(bind_subj, labeled)
  
#Create tidy data set with averaged for each variable for each activity and subject
  tidy_data <- subj_activity_avg %>%
    group_by(subject, activity) %>%
    summarize_each(funs(mean))

#Output tidy_data to an independent file
  write.table(tidy_data, file='getdataproject_tidydata.txt', row.name=FALSE)
  
