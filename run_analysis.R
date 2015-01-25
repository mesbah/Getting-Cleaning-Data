library("plyr")

# create Data folder
if(!file.exists("./data")){dir.create("./data")}
# download the data file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/UCI.HAR.Dataset.zip")
# unzip the data file
unzip(zipfile="./data/UCI.HAR.Dataset.zip",exdir="./data")
dataPath <- file.path("./data" , "UCI HAR Dataset")

# refere to Readme.txt in the data set

#- 'train/X_train.txt': Training set.

#- 'train/y_train.txt': Training labels.

#- 'test/X_test.txt': Test set.

#- 'test/y_test.txt': Test labels.

#- 'features.txt': List of all features.

#- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

#load files into data frames

#load data sets files for training and test
testSet  <- read.table(file.path(dataPath, "test" , "X_test.txt" ),header = FALSE)
trainSet <- read.table(file.path(dataPath, "train", "X_train.txt"),header = FALSE)

#load activities files for training and test
testActivities  <- read.table(file.path(dataPath, "test" , "Y_test.txt" ),header = FALSE)
trainActivities <- read.table(file.path(dataPath, "train", "Y_train.txt"),header = FALSE)

#load subject files for training and test
testSubject  <- read.table(file.path(dataPath, "test" , "subject_test.txt"),header = FALSE)
trainSubject <- read.table(file.path(dataPath, "train", "subject_train.txt"),header = FALSE)

# 1- Merges the training and the test sets to create one data set.

# Merge activities, subjects and data sets

dataFeatures<- rbind(trainSet, testSet)
dataSubject <- rbind(trainSubject, testSubject)
dataActivity<- rbind(trainActivities, testActivities)

# set header names for activities and subject
names(dataSubject)<-c("subject") #contains subject who performed activity
names(dataActivity)<- c("activity") #contains activity labels 

# add activity and subject columns to the main data set
dataCombine <- cbind(dataSubject, dataActivity)
df <- cbind(dataFeatures, dataCombine)

# 2- Extracts only the measurements on the mean and standard deviation for each measurement. 

# load features.txt 
# Readme.txt 'features.txt': List of all features.
dataFeaturesNames <- read.table(file.path(dataPath, "features.txt"),head=FALSE)
# str(dataFeaturesNames)
# 'data.frame':    561 obs. of  2 variables:
#    $ V1: int  1 2 3 4 5 6 7 8 9 10 ...
# $ V2: Factor w/ 477 levels "angle(tBodyAccJerkMean),gravityMean)",..: 243 244 245 250 25


#extract features names
featuresNamesubSet<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
df <- df[,c(match(featuresNamesubSet, dataFeaturesNames$V2),match(c('activity', 'subject'), names(df)))]

# 3- Uses descriptive activity names to name the activities in the data set

#load activity_labels.txt
activityLabels <- read.table(file.path(dataPath , "activity_labels.txt" ),header = FALSE)
#'data.frame':    6 obs. of  2 variables:
#$ V1: int  1 2 3 4 5 6
#$ V2: Factor w/ 6 levels "LAYING","SITTING",..: 4 6 5 2 3 1

# merge the data set with activities labels
# it should match activity to 'v1'
df <- merge(df,activityLabels, by.x='activity', by.y='V1')


# 4- Appropriately labels the data set with descriptive variable names.
# Using the labels we loaded from features.txt
# please refer to features_info.txt for more info
names(df)<- c('activity', as.character(featuresNamesubSet), 'subject', 'activity name') 

# 5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata<-aggregate(. ~subject + activity, df, mean)
tidydata<-tidydata[order(tidydata$subject,tidydata$activity),]
write.table(tidydata, file = "./Data/tidydata.txt",row.name=FALSE)
