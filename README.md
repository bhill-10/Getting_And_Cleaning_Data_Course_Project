# Getting And Cleaning Data Course Project
Contains files for the Course Project for Getting And Cleaning Data course on Coursera

##To ASSIST THE GRADER: I have added comments starting with "QUESTION" and a number (1-5) to highlight the requirements for the project


1) A description of the data set that included information from the original data readme along with descriptions of changes made to the data set as part of the course project
2) The raw R Code with commentary (executable code can be found in the separate file run_analysis.R which is part of this repo); raw code and comments begin below the line of "%"
3) A code book is included as a separate file in this repo

==================================================================
##Section 1: 

###Original Readme from the dataset is included between the two lines of "*"
******************************************************************
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
==========================================================

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

******************************************************************
##Changes made to original dataset
1) The test and training datasets were combined into one dataset
2) The subject IDs were added from the subject_test and subject_train files 
3) Activity labels from the activity_labels.txt file replace the numbered value records in the dataset
4) Variable names from the features.txt file were used to label the data
5) A subset of the combined data was selected based on the subject_ID, activity_ID, and the variables whose names included either "mean" or "std"
6) The subset of data was altered by calculating the mean of each variable by subject and by activity
7) A tidy dataset was contstructed and written to a table in a text file called "tidy.txt"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##HERE IS THE RAW R CODE

###Make calls to required libraries
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)


###READ DATA
#Download data and change working directory to unzipped data folder
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
setInternet2(TRUE)
download.file(fileUrl, destfile ="./getdata_course_project_data.zip", 
              method = "auto", mode = "wb")
unzip("getdata_course_project_data.zip")
setwd("UCI HAR Dataset")

###read features, variables, activities and assign descriptive names to R-objects
features <- read.table("features_info.txt", header = FALSE, skip=12, nrows = 17)
names(features) <- "Label"

### by separating on "(", the name is shortened by leaving off the "() : 'description'"
###Please reference Code Book, Variable Descriptions for more information
hci_variables <- read.table("features_info.txt", header = FALSE, sep = "(",skip = 32,nrows = 17)[1]
names(hci_variables) <- "Label"

angle_vars <- read.table("features_info.txt", header = FALSE,skip = 52,nrows = 5)
names(angle_vars) <- "Label"

complete_variables <-read.table("features.txt", header = FALSE)
names(complete_variables) <- c("ID", "Variable")

activity <- read.table("activity_labels.txt", header = FALSE, sep = " ")
names(activity) <- c("ID","Label")

###Read training data
train_data <- read.table("./train/X_train.txt", header=FALSE)
train_labels <-read.table("./train/Y_train.txt", header = FALSE)
train_subjects <- read.table("./train/subject_train.txt", header = FALSE)

###add descriptive names to training data objects created above
names(train_data) <- as.character(complete_variables$Variable)
names(train_labels) <- "activity_ID"
names(train_subjects) <- "subject_ID"

###combine training data, labels, and subjects into one data frame
train_data_combined <- cbind(train_subjects, train_labels, train_data)


#QUESTION 3: Uses descriptive activity names to name the activities in 
#the data set (for training data)

###replace activity ID with activity label/description
train_data_combined$activity_ID <- sapply(train_data_combined$activity_ID , 
               function(x){
                       x <- activity$Label[activity$ID == x]
                       return(x)
                       })

### read test data
test_data <- read.table("./test/X_test.txt", header=FALSE)
test_labels <-read.table("./test/Y_test.txt", header = FALSE)
test_subjects <- read.table("./test/subject_test.txt", header = FALSE)

#QUESTION 4: add variable names to the columns add descriptive names to test data objects created above
names(test_data) <- as.character(complete_variables$Variable)
names(test_labels) <- "activity_ID"
names(test_subjects) <- "subject_ID"

###combine training data, lables, and subjects into one data frame
test_data_combined <- cbind(test_subjects, test_labels, test_data)


#QUESTION 3: Uses descriptive activity names to name the activities in 
#the data set (for test  data)

###replace activity ID with activity label/description

test_data_combined$activity_ID <- sapply(test_data_combined$activity_ID , 
        function(x){
                x <- activity$Label[activity$ID == x]
                return(x)
        })

#QUESTION 1: Merge training and test data into one data set "all_data_combined" combine test and training data sets; arrange by subject ID
all_data_combined <- rbind(test_data_combined, train_data_combined)
all_data_combined <- arrange(all_data_combined, subject_ID)

#QUESTION 2: Extracts only the measurements on the mean and standard deviation 
###for each measurement.  Select column if column name/variable contains "mean" or "std"
sel_mean <- grep("mean",names(all_data_combined))
sel_std <- grep("std", names(all_data_combined))
sel <- c(1,2,sort(c(sel_mean,sel_std)))
subset_data <- all_data_combined[,sel]

# QUESTION 5: Create tidy dataset with average of each variable for each activity and each subject

###creates new variable combining subject ID and activity description
subset_data <- subset_data %>% mutate(subact_ID = paste(subject_ID, activity_ID))

###calculate the mean of variable for each combination of subject and activity
subset_data <- subset_data %>% group_by(subact_ID) %>% 
        summarise_each(funs(mean))

###repoplutate the subject and activity ID columns and remove combined ID column
c <- strsplit(subset_data$subact_ID, " ")
subset_data$subject_ID <- sapply(c, function(x) { x[1]})
subset_data$activity_ID <- sapply(c, function(x) { x[2]})
subset_data <- subset_data [,-1]
subset_data.vars <- names(subset_data)[-c(1,2)]
subset_data <- arrange(melt(subset_data, id = c("subject_ID", "activity_ID"), measure.vars = subset_data.vars), subject_ID, activity_ID)

#write tidy data set to .txt file
write.table(subset_data, file = 
        "C:/Users/Kelly/Desktop/Blake/dstwork/Getting_And_Cleaning_Data/Getting_And_Cleaning_Data_Course_Project/tidy.txt", 
        row.names = FALSE)
