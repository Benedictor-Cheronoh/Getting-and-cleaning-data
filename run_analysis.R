zip_file <- "https://unitednations-my.sharepoint.com/:u:/r/personal/benedictor_cheronoh_un_org/Documents/BCM/Learning/Data%20Science/data/getdata_projectfiles_UCI%20HAR%20Dataset%20(1).zip?csf=1&web=1&e=Uj0bXI"
# Unzip the file to a specified directory
unzip(zip_file, exdir ="https://unitednations-my.sharepoint.com/:u:/r/personal/benedictor_cheronoh_un_org/Documents/BCM/Learning/Data%20Science/data/getdata_projectfiles_UCI%20HAR%20Dataset%20(1).zip?csf=1&web=1&e=Uj0bXI"
      # Load data from the unzipped files
      data1 <- read.csv("https://unitednations-my.sharepoint.com/:u:/r/personal/benedictor_cheronoh_un_org/Documents/BCM/Learning/Data%20Science/data/getdata_projectfiles_UCI%20HAR%20Dataset%20(1).zip?csf=1&web=1&e=Uj0bXI")
      data2 <- read.csv("https://unitednations-my.sharepoint.com/:u:/r/personal/benedictor_cheronoh_un_org/Documents/BCM/Learning/Data%20Science/data/getdata_projectfiles_UCI%20HAR%20Dataset%20(1).zip?csf=1&web=1&e=Uj0bXI")
      read.csv()
      # Checking if folder exists
      if (!file.exists("https://unitednations-my.sharepoint.com/:u:/r/personal/benedictor_cheronoh_un_org/Documents/BCM/Learning/Data%20Science/data/getdata_projectfiles_UCI%20HAR%20Dataset%20(1).zip?csf=1&web=1&e=Uj0bXI")) { 
        unzip(filename) 
      }      
#UCI HAR Dataset      
      # Checking if folder exists
      if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
      }
      featureNames <- read.table("UCI HAR Dataset/features.txt")
      activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)      
      subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
      activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
      featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
      #UCI HAR Dataset      
      # Checking if folder exists
      if (!file.exists("UCI HAR Dataset")) { 
        unzip(UCI HAR Dataset)   
        subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
        activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
        featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


# Load required libraries
library(data.table)
library(reshape2)
# Grab Data and Unzip to project directory

# File URL to download
fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
##Local directory path and unzipping files
zip_file_path <-"Downloads"
temp_dir <- tempdir()
unzip("Downloads", exdir = temp_dir)
#measurements on the mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

## [1] 10299   563

## [1] 10299    88



#3. properties of the varibles
str(dataActivityTest)
# Get the data on mean and standard deviation
WantedColumns <- grep(".*Mean.*|.*Std.*", features[,2])

# Reduce the features table to the desired columns
features <- features[WantedColumns,]

# Add the lcolumns "subject" and "activity"
WantedColumns <- c(WantedColumns, 562, 563)

# Remove the unwanted columns from allData
allData <- allData[,WantedColumns]

# Add the column names (features) to allData
colnames(allData) <- c(features$V2, "Activity", "Subject")
colnames(allData) <- tolower(colnames(allData))

currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
  currentActivity <- currentActivity + 1
}

str(dataFeaturesTrain)


#labels the data set with descriptive variable names
names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))


# Grab Data and Unzip to project directory

# File URL to download
fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
##Local directory path and unzipping files
zip_file_path <-"Downloads"
temp_dir <- tempdir()
unzip("Downloads", exdir = temp_dir)

# Read training and testing datasets
training = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
training[,562] = read.csv("UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE)
training[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)


## The files that will be used to load data are;

test/subject_test.txt
test/X_test.txt
test/y_test.txt
train/subject_train.txt
train/X_train.txt
train/y_train.txt

### 1.  Merges the training and the test sets to create one data set.
##

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)

y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)


# Combines data table by (x, y, subject)_train vs (X,y, subject)_test by rows
x <- rbind(x_train, X_test)
y <- rbind(y_train, y_test)
s <- rbind(subject_train, subject_test)


WantedColumns <- c(WantedColumns, 562, 563)

## 2.  Extracts only the measurements on the mean and standard deviation for each measurement.
##

# Read features labels into R
features <- read.table("./UCI HAR Dataset/features.txt")
# Features names to features column
names(features) <- c('feat_id', 'feat_name')
# Searches each element of char vector for matches to the mean or std dev
index_features <- grep("-mean\\(\\)|-std\\(\\)", features$feat_name) 
x <- x[, index_features] 
# Replaces the matches from string
names(x) <- gsub("\\(|\\)", "", (features[index_features, 2]))


#measurements on the mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

## [1] 10299   563

## [1] 10299    88



#3. properties of the varibles
str(dataActivityTest)
# Get the data on mean and standard deviation
WantedColumns <- grep(".*Mean.*|.*Std.*", features[,2])

# Reduce the features table to the desired columns
features <- features[WantedColumns,]

# Add the lcolumns "subject" and "activity"
WantedColumns <- c(WantedColumns, 562, 563)

# Remove the unwanted columns from allData
allData <- allData[,WantedColumns]

# Add the column names (features) to allData
colnames(allData) <- c(features$V2, "Activity", "Subject")
colnames(allData) <- tolower(colnames(allData))

currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
  currentActivity <- currentActivity + 1
}

str(dataFeaturesTrain)


#labels the data set with descriptive variable names
names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))
    git push < Getting and Cleaning Data Course Project by Benedictor Cheronoh



