library(data.table)
library(dplyr)

#get full name of working directory
getwd()
setwd("/Users/user/Desktop/datasciencecoursera/Reading_and_cleaning_data")

#Download and unzip data
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 

download.file(url1, "run_data.zip", method="curl")

unzip(zipfile = "/Users/user/Desktop/datasciencecoursera/run_data.zip", exdir = "./REading_and_cleaning_data") #changed the name of this folder manually to be "Reading_and_cleaning_data"


#1. Merges the training and the test sets to create one data set.

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#3. Uses descriptive activity names to name the activities in the data set

#4. Appropriately labels the data set with descriptive variable names. 

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##############
#Read in data
##############

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","feature_names"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("act_code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject_code")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature_names)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "act_code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject_code")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature_names)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "act_code")

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


#=======================================================================
#1. Merge the training and test sets to create one data set.
#=======================================================================

#combine both train, both test, both subject tables together. Then combine all.
x_merged <- rbind(x_train, x_test) # features
y_merged <- rbind(y_train, y_test) # activities 
subject_merged <- rbind(subject_train, subject_test) #subject
all_merged <- cbind(x_merged, y_merged, subject_merged)

#=========================================================================================
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#=========================================================================================

final_data <- all_merged %>% 
                    select(subject_code, act_code, contains("mean"), contains("std"))

#=======================================================================
#3. Uses descriptive activity names to name the activities in the data set
#=======================================================================

final_data$act_code <- activities[final_data$act_code, 2]


#=======================================================================
#4. Appropriately labels the data set with descriptive variable names. 
#=======================================================================


colnames(final_data) #see all variables

# change the variable names to be more readible
names(final_data)[1] = "SubjectCode"
names(final_data)[2] = "Activity"
names(final_data) <- gsub("Acc", "Accelerometer", names(final_data))
names(final_data) <- gsub("Gyro", "Gyroscope", names(final_data))
names(final_data) <- gsub("BodyBody", "Body", names(final_data))
names(final_data) <- gsub("Mag", "Magnitude", names(final_data))
names(final_data) <- gsub("^t", "Time", names(final_data))
names(final_data) <- gsub("^f", "Frequency", names(final_data))
names(final_data) <- gsub("tBody", "TimeBody", names(final_data))
names(final_data) <- gsub("-mean()", "Mean", names(final_data), ignore.case = TRUE)
names(final_data) <- gsub("-std()", "STD", names(final_data), ignore.case = TRUE)
names(final_data) <- gsub("-freq()", "Frequency", names(final_data), ignore.case = TRUE)
names(final_data) <- gsub("angle", "Angle", names(final_data))
names(final_data) <- gsub("gravity", "Gravity", names(final_data))

View(final_data)

#==============================================================================================================================================
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#==============================================================================================================================================

summary_data <- final_data %>%
                group_by(SubjectCode, Activity) %>%
                summarise_all(funs(mean))

head(summary_data, 20)

write.table(summary_data, "summary_data.txt", row.name = FALSE)

