#set working directory
  setwd("C:/Users/Catherine/Desktop")

#Read in features and activity labels files.
  features <-read.table("UCI HAR Dataset/features.txt", header=FALSE)
  activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE)

#Change feature column names from abbreviated terms to longer descriptive variable names. 
#Use underscore to help with readability of unavoidably long variable names  
  features<-tolower(features[,2])
  features<-gsub("mean\\(\\)", "mean", features, ignore.case=TRUE)
  features<-gsub("std\\(\\)", "standard_deviation", features, ignore.case=TRUE) 
  features<-gsub("-", "_", features) #
  features<-gsub("\\(", "_", features) 
  features<-gsub("\\)", "", features)                                                   
  features<-gsub("\\,", "_", features)
  features<-gsub("iqr", "interquartile_range", features) 
  features<-gsub("sma", "signal_magnitude_area", features)
  features<-gsub("mad", "median_absolute_deviation", features)
  features<-gsub("^t", "time", features)
  features<-gsub("^f", "frequency", features)
  features<-gsub("freq", "frequency", features) 
  features<-gsub("acc", "accelerometer", features)
  features<-gsub("gyro", "gyroscope", features)

#read in the training datafiles, rename columns and use descriptive activity labels
  sub_train <-read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE) #sub id from training set
  x_train <-read.table("UCI HAR Dataset/train/x_train.txt", header=FALSE)
  colnames(x_train)<-features # set features as the column name for all the train variables
  y_train <-read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE)# activity
  y_train$V1[which(y_train$V1==1)]<- "Walking" 
  y_train$V1[which(y_train$V1==2)]<- "Walking_upstairs" 
  y_train$V1[which(y_train$V1==3)]<- "Walking_downstairs" 
  y_train$V1[which(y_train$V1==4)]<- "Sitting" 
  y_train$V1[which(y_train$V1==5)]<- "Standing" 
  y_train$V1[which(y_train$V1==6)]<- "Laying"
  colnames(y_train)<-"Activity"

#read in test datafiles, change activity labels to descriptive labels and set column name to "Activity"
  sub_test <-read.table("UCI HAR DATASET/test/subject_test.txt", header=FALSE) #sub id from test set
  x_test<- read.table("UCI HAR DATASET/test/x_test.txt", header=FALSE) 
  colnames(x_test)<-features# set features as the column name for all the test variables
  y_test <-read.table("UCI HAR DATASET/test/y_test.txt", header=FALSE) #activity
  y_test$V1[which(y_test$V1==1)]<- "Walking" 
  y_test$V1[which(y_test$V1==2)]<- "Walking_upstairs" 
  y_test$V1[which(y_test$V1==3)]<- "Walking_downstairs" 
  y_test$V1[which(y_test$V1==4)]<- "Sitting" 
  y_test$V1[which(y_test$V1==5)]<- "Standing" 
  y_test$V1[which(y_test$V1==6)]<- "Laying"
  colnames(y_test)<-"Activity"

#merge training and test sets retain only unique columns
  traindf<-cbind(sub_train, x_train, y_train)
  testdf<-cbind(sub_test, x_test,y_test)
  alldf<-rbind(traindf, testdf)
  alldf<-alldf[,unique(colnames(alldf))]# removes the duplicate column
  
#rename first column.
  colnames(alldf)[1]<-"Subject_id"
   
#Extracts only the measurements on the mean and standard deviation. 
#subset data to return only cols with mean and std.
  alldfstd<-alldf[grepl("standard_deviation", colnames(alldf),)]
  alldfmean<-alldf[grepl("mean", colnames(alldf),)]

  #bind with subject and activity variables
  tidydf<-cbind(alldf$Subject_id, alldf$Activity, alldfstd, alldfmean)
  colnames(tidydf)[1]<-"Subject_id"
  colnames(tidydf)[2]<-"Activity"

#Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
#convert to factor variable to later use in aggregate or DT
  tidydf$Subject_id<-as.factor(tidydf$Subject_id)
  tidydf$Activity<-as.factor(tidydf$Activity)

#load data.table and subset (.SD) DT by mean and ID and Activity 
  library(data.table) 
  DT<- data.table(tidydf) 
  meandf<- DT[, lapply(.SD, mean), by=c("Subject_id", "Activity")]
  meandf<- DT[, lapply(.SD, mean), by=c("Subject_id", "Activity")] 
  meandf<- meandf[order(meandf$Subject_id),] 

#write to file
  write.table(meandf, "tidydata.txt", row.name=FALSE) 
