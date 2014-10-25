#Merges the training and the best sets to create one data set.
setwd("c:/users/duw/documents/datascience/getting data/UCI HAR Dataset")
features <- read.table('./features.txt',header=FALSE) 
activity_type <- read.table('./activity_labels.txt',header=FALSE)  
subject_train <- read.table('./train/subject_train.txt',header=FALSE)  
x_train <- read.table('./train/x_train.txt',header=FALSE)  
y_train <- read.table('./train/y_train.txt',header=FALSE)  
colnames(activity_type) <- c('activity_id','activity_type') 
colnames(subject_train) <- "subject_id"
colnames(x_train) <- features[,2]
colnames(y_train) <- "activity_id" 
training_data <- cbind(y_train,subject_train,x_train) 
subject_test <-read.table('./test/subject_test.txt',header=FALSE)  
x_test <- read.table('./test/x_test.txt',header=FALSE) 
y_test <- read.table('./test/y_test.txt',header=FALSE) 
colnames(subject_test) <- "subject_id"
colnames(x_test) <- features[,2]  
colnames(y_test) <- "activity_id" 
test_data <- cbind(y_test,subject_test,x_test) 
merge_data <- rbind(training_data,test_data) 
col_names <- colnames(merge_data)

#Extracts only the measurements on the mean and standard deviation for each measurement.
extract_data <- (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names)) 
measurements <- merge_data[extract_data == TRUE]

#Uses descriptive activity names to name the activities in the data set.
updated_data <- merge(measurements,activity_type,by='activity_id',all.x=TRUE)
col_names <- colnames(updated_data)

#Appropriately labels the data set with descriptive variable names.
for (i in 1:length(col_names))  
{ 
  col_names[i] <- gsub("\\()","",col_names[i]) 
  col_names[i] <- gsub("-std$","Std_dev",col_names[i]) 
  col_names[i] <- gsub("-mean","Mean",col_names[i]) 
  col_names[i] <- gsub("^(t)","Time",col_names[i]) 
  col_names[i] <- gsub("^(f)","Freq",col_names[i]) 
  col_names[i] <- gsub("([Gg]ravity)","Gravity",col_names[i]) 
  col_names[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i]) 
  col_names[i] <- gsub("[Gg]yro","Gyro",col_names[i]) 
  col_names[i] <- gsub("AccMag","Acc_magnitude",col_names[i]) 
  col_names[i] <- gsub("([Bb]odyaccjerkmag)","Body_acc_jerk_magnitude",col_names[i]) 
  col_names[i] <- gsub("JerkMag","Jerk_magnitude",col_names[i]) 
  col_names[i] <- gsub("GyroMag","Gyro_magnitude",col_names[i]) 
} 
colnames(updated_data) <- col_names

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
pro_data <- updated_data[,names(updated_data) != 'activity_type'] 
combined_data <- aggregate(pro_data[,names(pro_data) != c('activity_id','subject_id')],by=list(activity_id=pro_data$activity_id,subject_id = pro_data$subject_id),mean) 
final_data <- merge(combined_data,activity_type,by='activity_id',all.x=TRUE)
write.table(final_data, './tidyData.txt',row.names=FALSE,sep='\t')

