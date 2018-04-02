#Call the dplyr package
library(dplyr)

#download UCI HAR Dataset from the web and save in working directory
#url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(url,"data.zip")
#unzip the zip file
#unzip("data.zip",exdir = ".")

#read and combine dataset from the test folder
test_x <- read.table("UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("UCI HAR Dataset/test/y_test.txt")
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt")
test_data <- cbind(test_sub, test_y, test_x)

#read and combine dataset from the train folder
train_x <- read.table("UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("UCI HAR Dataset/train/y_train.txt")
train_sub <- read.table("UCI HAR Dataset/train/subject_train.txt")
train_data <- cbind(train_sub, train_y, train_x)

#Merges the training and the test sets to create one data set.
df <- rbind(train_data,test_data)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("UCI HAR Dataset/features.txt")
names(df) <- c("subject", "activity", as.vector(features[,2]))
features_filter <- grepl("mean|std", features[,2]) & !grepl("meanFreq", features[,2])
header <- c(TRUE,TRUE,as.vector(features_filter))
df <- df[,header]

#Uses descriptive activity names to name the activities in the data set
df$activity <- gsub("1", "WALKING", df$activity)
df$activity <- gsub("2", "WALKING_UPSTAIRS", df$activity)
df$activity <- gsub("3", "WALKING_DOWNSTAIRS", df$activity)
df$activity <- gsub("4", "SITTING", df$activity)
df$activity <- gsub("5", "STANDING", df$activity)
df$activity <- gsub("6", "LAYING", df$activity)

#Appropriately labels the data set with descriptive variable names
descriptive_names <- tolower(names(df))
descriptive_names <- gsub("\\-mean\\(\\)", " mean", descriptive_names)
descriptive_names <- gsub("\\-std\\(\\)", " standard deviation", descriptive_names)
descriptive_names <- gsub("^t", "time-", descriptive_names)
descriptive_names <- gsub("^f", "frequency-", descriptive_names)
descriptive_names <- gsub("bodybody", "body", descriptive_names)
descriptive_names <- gsub("acc", " acceleration ", descriptive_names)
descriptive_names <- gsub("mag", " magnitude ", descriptive_names)
descriptive_names <- gsub("gyro", " gyroscope ", descriptive_names)
descriptive_names <- gsub("\\-x", " x-axis", descriptive_names)
descriptive_names <- gsub("\\-y", " y-axis", descriptive_names)
descriptive_names <- gsub("\\-z", " z-axis", descriptive_names)
descriptive_names <- gsub("  "," ", descriptive_names)
names(df) <- descriptive_names

#From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.
tidy_df <- tbl_df(df) %>% 
        group_by(subject, activity) %>%
        summarise_all(funs(mean))
View(tidy_df)
