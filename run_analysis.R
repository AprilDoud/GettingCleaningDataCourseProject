## You should create one R script called run_analysis.R that does the following.

##  1. Merges the training and the test sets to create one data set.

## set wd to R folder
setwd("~/R")

## define variables
data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data_desc <- "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"
outDir <- "~\\Documents\\R"
data_zip <- tempfile()

## download data then unzip
download.file(data_url,dest="dataset.zip",mode="wb")
unzip ("dataset.zip",exdir=outDir)

## Read data from files
features     = read.table('./UCI HAR Dataset/features.txt',header=FALSE); 
activityType = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE); 
subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE); 
xTrain       = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE); 
yTrain       = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE); 

## Assigin column names
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

## Merge yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

## Read in the test data
subjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE); 
xTest       = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE); 
yTest       = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE); 

## Assign column names
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

## Merge xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);

## Combine training and test 
finalData = rbind(trainingData,testData);

## Create a vector for the column names from the finalData
colNames  = colnames(finalData); 

## 2. Extract only the measurements on the mean and standard deviation for each measurement. 

## Create logicalVector
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) 
                 & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) 
                 & !grepl("-std()..-",colNames));

## Subset finalData table to keep only desired columns
finalData = finalData[logicalVector==TRUE];

## 3. Use descriptive activity names to name the activities in the data set

## Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

## Updating colNames vector
colNames  = colnames(finalData); 

## 4. Appropriately label the data set with descriptive activity names. 

## Clean up variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("^(f)","Freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

## Reassigning the new colNames
colnames(finalData) = colNames;

##  5. From the data set in step 4, creates a second, independent tidy data set with the
##     average of each variable for each activity and each subject.

## Create File
finalDataNoAct  = finalData[,names(finalData) != 'activityType'];

## Summarize
tidyData    = aggregate(finalDataNoAct[,names(finalDataNoAct) != c('activityId','subjectId')],by=list(activityId=finalDataNoAct$activityId,subjectId = finalDataNoAct$subjectId),mean);

##Merge
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

## Export
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')


