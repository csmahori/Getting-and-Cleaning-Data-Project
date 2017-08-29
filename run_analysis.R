setwd("./g&cData")
#downloading the Dataset
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","data.zip")
#unzipping the downloaded file
unzip("./data.zip")
#set working directory to the location where the UCI HAR Dataset was unzipped
setwd("./g&cData/UCI HAR Dataset")
# Read in the data from files
features<-read.table("./features.txt",header = FALSE)
activityTypes<-read.table("./activity_labels.txt",header = FALSE)
##test Data
subjectTest<-read.table("./test/subject_test.txt",header=FALSE)
xTest<-read.table("./test/X_test.txt",header = FALSE)
yTest<-read.table("./test/y_test.txt",header = FALSE)
##train Data
subjectTrain<-read.table("./train/subject_train.txt",header=FALSE)
xTrain<-read.table("./train/X_train.txt",header = FALSE)
yTrain<-read.table("./train/y_train.txt",header = FALSE)
# Assigin column names to the data imported
colnames(activityTypes)=c('activityId','activityName')
##test Data
colnames(subjectTest)='subjectId'
colnames(xTest)=features[,2]
colnames(yTest)='activityId'
##train Data
colnames(subjectTrain)='subjectId'
colnames(xTrain)=features[,2]
colnames(yTrain)='activityId'
##Merge test Data
testData<-cbind(subjectTest,yTest,xTest)
##merge train Data
trainData<-cbind(subjectTrain,yTrain,xTrain)
#1.Merge the training and the test sets to create one data set
finalData<-rbind(testData,trainData)
#2.Extracts only the measurements on the mean and standard deviation for each measurement
logicalVector<-(grepl('[Mm]ean',colnames(finalData))|grepl('[sS]td',colnames(finalData))|grepl('activity',colnames(finalData))|grepl('subjectId',colnames(finalData)))
finalData[logicalVector]
#3.Uses descriptive activity names to name the activities in the data set
finalData=merge(finalData,activityTypes,by='activityID',all.x = TRUE)
unique(finalData[,"activityType"])
#4.Appropriately labels the data set with descriptive variable names
colNames<-colnames(finalData)
colNames = gsub("\\()","",colNames)
colNames = gsub("\\-s","S",colNames)
colNames = gsub("\\-m","M",colNames)
colNames = gsub("\\-e","E",colNames)
colNames = gsub("\\-i","I",colNames)
colNames = gsub("\\-a","A",colNames)
colNames = gsub("\\-k","K",colNames)
colNames = gsub("\\-b","B",colNames)
colNames = gsub("\\-c","C",colNames)
colNames = gsub("^(t)","time",colNames)
colNames = gsub("^(f)","freq",colNames)
colNames = gsub("([Gg]ravity)","Gravity",colNames)
colNames = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames)
colNames = gsub("[Gg]yro","Gyro",colNames)
colNames = gsub("AccMag","AccMagnitude",colNames)
colNames = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames)
colNames = gsub("JerkMag","JerkMagnitude",colNames)
colNames = gsub("GyroMag","GyroMagnitude",colNames)
colNames = gsub("\\-","",colNames)
##Reassigning the new descriptive column names to the finalData set
colnames(finalData)<-colNames
finalData$activityId<-NULL
#Remove dduplicated columns
finalData<-finalData[,!duplicated(t(finalData))]
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
# Group the data by subject and activity
tidygroup <-group_by(tidy, subjectId, activityType)
# Calculate the mean for all features using a Dplyr function
tidymean <- summarise_all(tidygroup, funs(mean))
