## Load appropriate packages.
require("reshape2")
require("data.table")


## Download file (NB! path is set to current working directory - so remember to set this before you run this script!)
# setwd("...Set path!...")
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
download.file(url, file.path(path, f))


## Unzip the file 
zipfile <- paste0(path, "/", f)
outputDir <- path
unzip(zipfile,exdir=outputDir)


## The unzipped archive put the files in the folder 'UCI HAR Dataset'. A new path is set to this folder.
pathFiles <- file.path(path, "UCI HAR Dataset")


## Load the subject, activity and data files.
TrainSubject <- fread(file.path(pathFiles, "train", "subject_train.txt"))
TestSubject  <- fread(file.path(pathFiles, "test" , "subject_test.txt" ))
TrainActivity <- fread(file.path(pathFiles, "train", "Y_train.txt"))
TestActivity  <- fread(file.path(pathFiles, "test" , "Y_test.txt" ))
TrainData <- fread(file.path(pathFiles, "train", "X_train.txt"))
TestData  <- fread(file.path(pathFiles, "test" , "X_test.txt" ))


## Merge train and test data tables.
Subject <- rbind(TrainSubject, TestSubject)
Activity <- rbind(TrainActivity, TestActivity)
Data <- rbind(TrainData, TestData)


## Rename column names in tables Subject and Activity tables and merge columns with the columns in table Data
setnames(Subject, "V1", "SubjectID")
setnames(Activity, "V1", "activityNumber")
Data <- cbind(cbind(Subject, Activity), Data)

## Set key in table Data
setkey(Data, SubjectID, activityNumber)


## Load the features.txt file in order to identify the variables/measures in table Data. Then select only measures for the mean and standard deviation
Features <- fread(file.path(pathFiles, "features.txt"))
setnames(Features, names(Features), c("featureNumber", "featureName"))
Features <- Features[grepl("mean\\(\\)|std\\(\\)", featureName)]


## Add a column to Features with the feature numbers converted to a vector of variable names matching columns in table Data.
Features$DataColName <- Features[, paste0("V", featureNumber)]


## Subselect the columns in table Data that math the variable names in DataColName in table Features
Data <- Data[, c(key(Data), Features$DataColName), with=FALSE]


## Load the activity_labels.txt file in order to be able to add descriptive names to the activities.
ActivityDesc <- fread(file.path(pathFiles, "activity_labels.txt"))
setnames(ActivityDesc, names(ActivityDesc), c("activityNumber", "Activity"))


## Merge activity labels.
Data <- merge(Data, ActivityDesc, by="activityNumber", all.x=TRUE)

## Add Activity as a key.
setkey(Data, SubjectID, activityNumber, Activity)

## Melt the data table to reshape it from a short and wide format to a tall and narrow format.
Data <- data.table(melt(Data, key(Data), variable.name="DataColName"))

## Merge activity name.
Data <- merge(Data, Features[, list(featureNumber, DataColName, featureName)], by="DataColName", all.x=TRUE)


## Create a new variable, activity that is equivalent to Activity as a factor class. Create a new variable, feature that is equivalent to featureName as a factor class.
Data$activity <- factor(Data$Activity)
Data$feature <- factor(Data$featureName)


## Create function to find features from featureName using the helper function FindFeature.
FindFeature <- function (inp) {
  grepl(inp, Data$featureName)
}


## Features with 2 categories
cat <- matrix(seq(1, 2), nrow=2)


## Add column Domain with the factors 'Time' and 'Frequency' 
fac <- matrix(c(FindFeature("^t"), FindFeature("^f")), ncol=nrow(cat))
Data$Domain <- factor(fac %*% cat, labels=c("Time", "Frequency"))

## Add column Acceleration with the factors 'Body' and 'Gravity' (NB! NA's occur!)
fac <- matrix(c(FindFeature("BodyAcc"), FindFeature("GravityAcc")), ncol=nrow(cat))
Data$Acceleration <- factor(fac %*% cat, labels=c(NA, "Body", "Gravity"))

## Add column Instrument with the factors 'Accelerometer' and 'Gyroscope'
fac <- matrix(c(FindFeature("Acc"), FindFeature("Gyro")), ncol=nrow(cat))
Data$Instrument <- factor(fac %*% cat, labels=c("Accelerometer", "Gyroscope"))

## Add column MeasureType with the factors 'Mean' and 'Standard Deviation'
fac <- matrix(c(FindFeature("mean()"), FindFeature("std()")), ncol=nrow(cat))
Data$MeasureType <- factor(fac %*% cat, labels=c("Mean", "Standard Deviation"))

## Add column JerkFlag with the factors 'Yes' and 'No'
Data$JerkFlag <- factor(FindFeature("Jerk"), labels=c("No", "Yes"))

## Add column MagnitudeFlag with the factors 'Yes' and 'No'
Data$MagnitudeFlag <- factor(FindFeature("Mag"), labels=c("No", "Yes"))

## Add column xialDirection with the factors 'X', 'Y' and 'Z' (NB! NA's occur!)
cat <- matrix(seq(1, 3), nrow=3)
fac <- matrix(c(FindFeature("-X"), FindFeature("-Y"), FindFeature("-Z")), ncol=nrow(cat))
Data$AxialDirection <- factor(fac %*% cat, labels=c(NA, "X", "Y", "Z"))


## Finally create a data set with the average of each variable for each activity and each subject 
setkey(Data, SubjectID, Activity, Domain, Acceleration, Instrument, JerkFlag, MagnitudeFlag, MeasureType, AxialDirection)
TidyData <- Data[, list(AverageValue = mean(value), NumberObservations = .N ), by=key(Data)]

write.table(TidyData, "TidyData.txt", quote=FALSE, sep="\t", row.names=FALSE)


