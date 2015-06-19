# Getting And Cleaning Data Course Project
Contains files for the Course Project for Getting And Cleaning Data course on Coursera

##To ASSIST THE GRADER: I have added comments starting with "QUESTION" and a number (1-5) 
##to highlight the requirments for the project


##HERE IS THE RAW R CODE

#Make calls to required libraries
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)


#READ DATA
#Download data and change working directory to unzipped data folder
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
setInternet2(TRUE)
download.file(fileUrl, destfile ="./getdata_course_project_data.zip", 
              method = "auto", mode = "wb")
unzip("getdata_course_project_data.zip")
setwd("UCI HAR Dataset")

#read features, variables, activities and assign descriptive names to R-objects
features <- read.table("features_info.txt", header = FALSE, skip=12, nrows = 17)
names(features) <- "Label"

# by separating on "(", the name is shortened by leaving off the "() : 'description'"
#Please reference Code Book, Variable Descriptions for more information
hci_variables <- read.table("features_info.txt", header = FALSE, sep = "(",skip = 32,nrows = 17)[1]
names(hci_variables) <- "Label"

angle_vars <- read.table("features_info.txt", header = FALSE,skip = 52,nrows = 5)
names(angle_vars) <- "Label"

complete_variables <-read.table("features.txt", header = FALSE)
names(complete_variables) <- c("ID", "Variable")

activity <- read.table("activity_labels.txt", header = FALSE, sep = " ")
names(activity) <- c("ID","Label")

#Read training data
train_data <- read.table("./train/X_train.txt", header=FALSE)
train_labels <-read.table("./train/Y_train.txt", header = FALSE)
train_subjects <- read.table("./train/subject_train.txt", header = FALSE)

#add descriptive names to training data objects created above
names(train_data) <- as.character(complete_variables$Variable)
names(train_labels) <- "activity_ID"
names(train_subjects) <- "subject_ID"

#combine training data, lables, and subjects into one data frame
train_data_combined <- cbind(train_subjects, train_labels, train_data)


#QUESTION 3: Uses descriptive activity names to name the activities in 
#the data set (for training data)

#replace activity ID with activity label/description
train_data_combined$activity_ID <- sapply(train_data_combined$activity_ID , 
               function(x){
                       x <- activity$Label[activity$ID == x]
                       return(x)
                       })

# read test data
test_data <- read.table("./test/X_test.txt", header=FALSE)
test_labels <-read.table("./test/Y_test.txt", header = FALSE)
test_subjects <- read.table("./test/subject_test.txt", header = FALSE)

#QUESTION 4: add variable names to the columns
#add descriptive names to test data objects created above
names(test_data) <- as.character(complete_variables$Variable)
names(test_labels) <- "activity_ID"
names(test_subjects) <- "subject_ID"

#combine training data, lables, and subjects into one data frame
test_data_combined <- cbind(test_subjects, test_labels, test_data)


#####QUESTION 3: Uses descriptive activity names to name the activities in 
#the data set (for test  data)

#replace activity ID with activity label/description

test_data_combined$activity_ID <- sapply(test_data_combined$activity_ID , 
        function(x){
                x <- activity$Label[activity$ID == x]
                return(x)
        })

#QUESTION 1: Merge training and test data into one data set "all_data_combined"
#combine test and training data sets; arrange by subject ID
all_data_combined <- rbind(test_data_combined, train_data_combined)
all_data_combined <- arrange(all_data_combined, subject_ID)

#QUESTION 2: Extracts only the measurements on the mean and standard deviation 
##for each measurement.  Select column if column name/variable contains "mean" 
#or "std"
sel_mean <- grep("mean",names(all_data_combined))
sel_std <- grep("std", names(all_data_combined))
sel <- c(1,2,sort(c(sel_mean,sel_std)))
subset_data <- all_data_combined[,sel]

# QUESTION 5: Create tidy dataset with average of each variable for each 
#activity and each subject

#creates new variable combining subject ID and activity description
subset_data <- subset_data %>% mutate(subact_ID = paste(subject_ID, activity_ID))

#calculate the mean of variable for each combination of subject and activity
subset_data <- subset_data %>% group_by(subact_ID) %>% 
        summarise_each(funs(mean))

#repoplutate the subject and activity ID columns and 
#remove combined ID column
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
