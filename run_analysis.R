#Course Project: Getting and Cleaning Data
#PCHAOPRANGHERRERA

#1.Merge the training and the test sets to create one data set.

#set working directory to the UCI HAR Dataset
setwd('/personal/DesktopFiles/UCI HAR Dataset/');

#Training Sets#
# 1.1 Read in the data
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# 1.2 Assigin column names
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# 1.3 Create the final training set
trainingData = cbind(yTrain,subjectTrain,xTrain);

#test Sets#
# 1.4 Read in the data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# 1.5 Assign column names to the test data imported above
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# 1.6 Create the final test set
testData = cbind(yTest,subjectTest,xTest);

#Final combined training and test data set#
finalData = rbind(trainingData,testData);

###############################################################################################

#2.Extracts only the measurements on the mean and standard deviation for each measurement.

#2.1 Create a vector for the column names to select the desired mean() & stddev() columns
colNames  = colnames(finalData); 

# 2.2 Create a logicalVector that contains the ID, mean() & stddev() columns are TRUE, otherwise FALSE
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# 2.3 Subset final Data
finalData = finalData[logicalVector==TRUE];

################################################################################################

#3.Uses descriptive activity names to name the activities in the data set

# 3.1 Merge the finalData set with the acitivityType table so that it contains descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Uses descriptive activity names to name the activities in the data set after the merging
colNames  = colnames(finalData); 


###############################################################################################
#4.Appropriately labels the data set with descriptive variable names.

# 4.1 Tidy variable names
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# 4.2 Appropriately labels the new descriptive column names
colnames(finalData) = colNames;

###############################################################################################
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#5.1 Create a second, independent tidy data set, finalDataNoActType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

#5.2 Table includes just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');














