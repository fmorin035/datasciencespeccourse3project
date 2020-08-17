#Loading dataset from test and train folders

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile = "course3project.zip")

  
trainx<-read.delim("UCI HAR Dataset/train/X_train.txt",header=FALSE,sep="")
features<-read.delim("UCI HAR Dataset/features.txt",header=FALSE,sep="")
featurevector<-features[,2]

trainxfeatures<-read.delim("UCI HAR Dataset/train/X_train.txt",header=FALSE,sep="",col.names = featurevector)


trainy<-read.delim("UCI HAR Dataset/train/y_train.txt",header=FALSE,sep="",col.name=c("trainy"))

activityvector<-trainy[,1]

trainyxfeaturetrainy<-cbind(trainxfeatures,activityvector)

subjectrain<-read.delim("UCI HAR Dataset/train/subject_train.txt",header=FALSE,sep="",col.name=c("subjecttrain"))
idvector<-subjectrain[,1]

trainyxfeaturetrainysubjectrain<-cbind(trainyxfeaturetrainy,idvector)


testx<-read.delim("UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="")
features<-read.delim("UCI HAR Dataset/features.txt",header=FALSE,sep="")
featurevector<-features[,2]

testxfeatures<-read.delim("UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="",col.names = featurevector)


testy<-read.delim("UCI HAR Dataset/test/y_test.txt",header=FALSE,sep="",col.name=c("testy"))

activityvector<-testy[,1]

testyxfeaturetesty<-cbind(testxfeatures,activityvector)

subjectest<-read.delim("UCI HAR Dataset/test/subject_test.txt",header=FALSE,sep="",col.name=c("subjecttest"))
idvector<-subjectest[,1]

testyxfeaturetestysubjectest<-cbind(testyxfeaturetesty,idvector)




#Part 1. Script that merges the training and the test sets to create one data set.

library(dplyr)
test<-as_tibble(testyxfeaturetestysubjectest)
train<-as_tibble(trainyxfeaturetrainysubjectrain)

testtrain<-bind_rows(test,train)

#Part 2. Script that extracts only the measurements on the mean and standard deviation for each measurement.

library(tidyselect)

mstt<-select(testtrain, matches("mean|std|vector"))

#Part 3. Script that uses descriptive activity names to name the activities in the data set


mstt1<-mstt %>% mutate_at(c("activityvector"), replace,mstt$activityvector==1,"WALKING")
mstt2<-mstt1 %>% mutate_at(c("activityvector"), replace,mstt$activityvector==2,"WALKING_UPSTAIRS")
mstt3<-mstt2 %>% mutate_at(c("activityvector"), replace,mstt$activityvector==3,"WALKING_DOWNSTAIRS")
mstt4<-mstt3 %>% mutate_at(c("activityvector"), replace,mstt$activityvector==4,"SITTING")
mstt5<-mstt4 %>% mutate_at(c("activityvector"), replace,mstt$activityvector==5,"STANDING")
mstt6<-mstt5 %>% mutate_at(c("activityvector"), replace,mstt$activityvector==6,"LAYING")


#Part 4.
#Was performed in step 1, loading the datasets with proper column/variable names


#Part 5.Script that, from the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

meanstdvariables<-colnames(mstt6)
meanstdvariables2<-meanstdvariables[1:86]

finaltidydataset <- mstt6 %>%group_by(activityvector,idvector) %>%summarize_at(meanstdvariables2,mean)




