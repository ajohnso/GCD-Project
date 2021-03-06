This data project consists of data from wearable activity trackers.

Code Procedures:
===============

1. Download UCI HAR Data into working directory

2. Read in datasets from UCI HAR Data folder

3. Merge X_train and X_test

4. Apply appropriate variable names to merged dataset from features.txt

4. Extract only mean and std measurements

5. Merge y_train and y_test to combine activity labels

6. Apply names to the activity numbers from activity_labels.txt

7. Create an independent tidy data set with the average of each variable for each activity and each subject

Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'


Variable names were updated to more descriptive terms as follows:

Symbols were removed and variable descriptions were improved-see below:

"Acc" changed to "Acceleration"

"Mag" changed to "Magnitude"

"Gyro" changed to "Gyroscope"

"BodyBody" changed to "Body"

"Std" changed to "StandardDeviation"

"Freq" changed to "Frequency"


Datasets created in this tidy data project include the following with descriptions:

  features : from features.txt in UCI HAR Dataset folder
  
  activity : from activity_labels.txt in UCI HAR Dataset folder
  
  y_train : from y_train.txt from train folder
  
  y_test : from y_test.txt from test folder
  
  X_test : from X_test.txt from test folder
  
  X_train : from X_train.txt from train folder
  
  subj_test : from subject_test.txt from test folder
  
  subj_train : from subject_train.txt from train folder
  
  bind_tt : Merge training and test data sets
  
  mean_std : test and training data sets with only the measurements on the mean and standard deviation for each measurement extracted
  
  bind_names : merged training and test label numbers from  y_train and y_test
  
  labeled : dataset with mean and standard deviation measurements combined with activity labels
  
  bind_subj : merged dataset with subject numbers and activity numbers
  
  subj_activity_avg : dataset with the average of each variable for each activity and each subject
  
  
  
  
