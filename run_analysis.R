library(dplyr) 
library(data.table) 
library(tidyr)

projectPath <- "/projects/datascience/cleaningdata/courseproject"
dataPath <- paste0(projectPath, "/data" )
filePath <- paste0(dataPath, "/UCI HAR Dataset")
setwd(projectPath)
 
print("Starting Analysis")

## If we don't have the project data yet - go ahead and download it
if (!file.exists(dataPath))
{
  print("Downloading data files")
  dir.create(dataPath)
  fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file( fileUrl, destfile = paste0(dataPath,"/UCI_HAR_Dataset.zip"))
   
  print("Unzipping data files")
  unzip( zipfile = paste0( dataPath, "/UCI_HAR_Dataset.zip"), exdir= dataPath)
} 
  
print( sprintf("Processing data files available in %s", dataPath) )

## Read in the subject files
subject.train <- read.table(file.path( filePath, "train", "subject_train.txt"))
subject.test <- read.table(file.path( filePath, "test", "subject_test.txt"))
subject <- rbind( subject.train, subject.test)

## Read in the Y files
y.train <- read.table(file.path( filePath, "train", "Y_train.txt"))
y.test <- read.table(file.path( filePath, "test", "Y_test.txt"))
y <- rbind( y.train, y.test )

## Read in the X files
x.train <- read.table(file.path( filePath, "train", "X_train.txt"))
x.test <- read.table(file.path( filePath, "test", "X_test.txt"))
x <- rbind( x.train, x.test ) 

# Extract only the measurements on the mean and standard deviation for each measurement. 
features <- read.table('./UCI HAR Dataset/features.txt')
mean.sd <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x.mean.sd <- x[, mean.sd]

# Uses descriptive activity names to name the activities in the data set
names(x.mean.sd) <- features[mean.sd, 2]
names(x.mean.sd) <- tolower(names(x.mean.sd)) 
names(x.mean.sd) <- gsub("\\(|\\)", "", names(x.mean.sd))

activities <- read.table(file.path( filePath,'', 'activity_labels.txt'))
activities[, 2] <- tolower(as.character(activities[, 2]))
activities[, 2] <- gsub("_", "", activities[, 2])

y[, 1] = activities[y[, 1], 2]
colnames(y) <- 'activity'
colnames(subject) <- 'subject'

# Appropriately labels the data set with descriptive activity names.
data <- cbind(subject, x.mean.sd, y)
str(data)
write.table(data, paste0(projectPath, '/merged.txt'), row.names = F)

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
average.df <- aggregate(x=data, by=list(activities=data$activity, subject=data$subject), FUN=mean)
average.df <- average.df[, !(colnames(average.df) %in% c("subj", "activity"))]
str(average.df)
write.table(average.df, paste0(projectPath,'/tidy.txt'), row.names = F)