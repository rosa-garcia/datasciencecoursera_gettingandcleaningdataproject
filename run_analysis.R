library(tidyr)
library(plyr)
library(dplyr)

# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# download raw data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataset_file_name <- "getdata-projectfiles-UCI HAR Dataset.zip"
if(!file.exists(dataset_file_name)) {
  download.file(url, dest=dataset_file_name, mode="wb") 
}

# build train data
subject_train <- read.table(unz(dataset_file_name, "UCI HAR Dataset/train/subject_train.txt"))
x_train <- read.table(unz(dataset_file_name, "UCI HAR Dataset/train/X_train.txt"))
y_train <- read.table(unz(dataset_file_name, "UCI HAR Dataset/train/y_train.txt"))
train_data <- cbind(subject = subject_train, activity = y_train, x_train)

# build test data
subject_test <- read.table(unz(dataset_file_name, "UCI HAR Dataset/test/subject_test.txt"))
x_test <- read.table(unz(dataset_file_name, "UCI HAR Dataset/test/X_test.txt"))
y_test <- read.table(unz(dataset_file_name, "UCI HAR Dataset/test/y_test.txt"))
test_data <- cbind(subject = subject_test, activity = y_test, x_test)

# join train data and test data
all_data <- rbind(train_data, test_data)

# set variable names
features <- read.table(unz(dataset_file_name, "UCI HAR Dataset/features.txt"))
names(all_data) <- c("subject", "activity", make.names(as.character(features[[2]]), unique=TRUE))

# extract only the measurements on the mean and standard deviation
mean_std_data <- select(all_data, 1:2, contains(".mean."), contains(".std."))

# clean variable names
names(mean_std_data) <- sub("mean..", "mean", names(mean_std_data))
names(mean_std_data) <- sub("std..", "std", names(mean_std_data))
names(mean_std_data) <- sub("BodyBody", "Body", names(mean_std_data))

# set descriptive activity names
activity_labels <- read.table(unz(dataset_file_name, "UCI HAR Dataset/activity_labels.txt"))
mean_std_data$activity <- factor(mean_std_data$activity, levels=activity_labels[[1]], labels=activity_labels[[2]])

# calculate averages by subject and activity (as required)
messy_data <- aggregate(
  select(mean_std_data, -subject, -activity), 
  by=list(
    subject=mean_std_data$subject, 
    activity=mean_std_data$activity), 
  mean)

# tidy data
tidy_data<- messy_data %>%
  gather(domain_signal_meanstd_axis, value, -subject, -activity) %>%
  separate(domain_signal_meanstd_axis, into=c("domain", "signal_meanstd_axis"), 1)  %>%
  separate(signal_meanstd_axis, into=c("signal", "meanstd", "axis"), "\\.", fill = "right")  %>%
  spread(meanstd, value) %>%
  select(subject, activity, domain, signal, axis, mean, std) %>%
  arrange(subject, activity, domain, signal, axis)

# rename text values to be more clear
names(tidy_data) <- sub("std", "standardDeviation", names(tidy_data))
tidy_data$domain <- sub("f", "frequency", tidy_data$domain)
tidy_data$domain <- sub("t", "time", tidy_data$domain)

# create factors
tidy_data$domain <- factor(tidy_data$domain)
tidy_data$signal <- factor(tidy_data$signal)
tidy_data$axis <- factor(tidy_data$axis)

tidy_data
