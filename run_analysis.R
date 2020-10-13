library(dplyr)
# Download the dataset
dataurl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zippedFile<- "UCI HAR Dataset.zip"
if(!file.exists(zippedFile)){download.file(dataurl, zippedFile, mode ='wb')}

#unzip the data
data<-"UCI HAR Dataset"
if(!file.exists(data)){unzip(zippedFile)}

#read training data
Subjects<- read.table(file.path(data, "train", "subject_train.txt"))
values <- read.table(file.path(data, "train", "X_train.txt"))
Activity<- read.table(file.path(data, "train", "y_train.txt"))

 #read test data
subjects1<-read.table(file.path(data, "test", "subject_test.txt"))
values1<-read.table(file.path(data, "test", "X_test.txt"))
Activity1<-read.table(file.path(data, "test", "y_test.txt"))

#read features
dataFeature<-read.table(file.path(data, "features.txt"), as.is = TRUE)

#read Activity labels
activities <- read.table(file.path(data, "activity_labels.txt"))
 colnames(activities)<-c("activityId", "activityLabel")
 
#merge training and test data
trainTestData<-rbind(cbind(Subjects, values, Activity), cbind(subjects1, values1, Activity1))

# assign column names
 colnames(trainTestData)<-c("Subject", dataFeature[, 2], "Task")

# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement
keepColumn<-grepl("Subject|Task|mean|std", colnames(trainTestData))
# keep data in these columns only
trainTestData<-trainTestData[, keepColumn]

# Step 3 - Use descriptive activity names to name the activities in the data set
trainTestData$Task<- factor(trainTestData$Task, levels = activities[, 1], labels = activities[,2])

# Step 4 - Appropriately label the data set with descriptive variable names
# get column names
trainTestDatacols<-colnames(trainTestData)

 # expand abbreviations and clean up names
trainTestDatacols<gsub("^f", "frequencyDomain", trainTestDatacols)
trainTestDatacols<gsub("^t", "timeDomain", trainTestDatacols)
trainTestDatacols<gsub("Acc", "Accelerometer", trainTestDatacols)
trainTestDatacols<gsub("Gyro", "Gyroacope", trainTestDatacols)
trainTestDatacols<gsub("Mag", "Magnitude", trainTestDatacols)
trainTestDatacols<gsub("Freq", "Frequency", trainTestDatacols)
trainTestDatacols<gsub("mean", "Mean", trainTestDatacols)
 trainTestDatacols<gsub("std", "StandardDeviation", trainTestDatacols)
  
  #remove special characters
trainTestDatacols<-gsub("[\\(\\)-]", "", trainTestDatacols)

#correct typo
trainTestDatacols<- gsub('Body Body', 'Body', trainTestDatacols)

# use new labels as column names
colnames(trainTestData)<-trainTestDatacols

# Step 5 - Create a second, independent tidy set with the average of each variable for each activity and each subject
 # group by Subject and Task and summarise using mean
 tidydata<- trainTestData %>% group_by( Subject, Task) %>% summarise_each(funs(mean))
 
 # output to file "tidy_data.txt"
write.table(tidydata, 'tidy_data.txt', row.names = FALSE, quote = FALSE)
