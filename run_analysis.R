install.packages("plyr")
library(plyr);
#Merges the training and the test sets to create one data set.

data_test <- read.table("X_test.txt")
as.data.frame(data_test)
subject_test <- read.table("subject_test.txt")
as.data.frame(subject_test)
activities_label_test <- read.table("y_test.txt")
as.data.frame(activities_label_test)
data_test <- cbind(activities_label_test,data_test)
data_test <- cbind(subject_test,data_test)

data_train <- read.table("X_train.txt")
as.data.frame(data_train)
subject_train <- read.table("subject_train.txt")
as.data.frame(subject_train)
activities_label_train <- read.table("y_train.txt")
as.data.frame(activities_label_train)
data_train <- cbind(activities_label_train,data_train)
data_train <- cbind(subject_train,data_train)

data_total <- rbind(data_train,data_test)

#Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
as.data.frame(features)
mean_columns <- which(grepl("mean()",features$V2))
std_columns <- which(grepl("std()",features$V2))
columns <- append(mean_columns,std_columns, after = length(mean_columns))
features <- features[columns,]
columns <- columns +2 
colnames(data_total)[1] <- "VolunteerID"
colnames(data_total)[2] <- "Activities"
columns <- paste0("V",columns)
columns <- append(c("VolunteerID","Activities"),columns, after = 2)
data_mean_std <- data_total[,columns]

#Appropriately labels the data set with descriptive variable names. 

colnames(data_mean_std)[3:length(data_mean_std)] <- as.character(features$V2)

#Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
as.data.frame(activities)
data_mean_std$Activities <- factor(as.character(data_mean_std$Activities),
                    levels = as.character(activities$V1),
                    labels = as.character(activities$V2)) 


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

summaryDF <- aggregate(. ~VolunteerID + Activities, data_mean_std, mean)
summaryDF <- summaryDF[order(summaryDF$VolunteerID,summaryDF$Activities),]
write.table(summaryDF, file = "tidydataset.txt",row.name=FALSE)
