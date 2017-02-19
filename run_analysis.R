
# 1. Merges the training and the test sets to create one data set. DONE
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. DONE
# 3. Uses descriptive activity names to name the activities in the data set DONE
# 4. Appropriately labels the data set with descriptive variable names. DONE
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. DONE

library(dplyr)

get_features <- function() {
  features <- read.table("data/UCI HAR Dataset/features.txt",header = FALSE)
  features[,2]
}

get_required_feature_names <- function() {
  all_features <- get_features()
  
  grep(pattern = "mean\\(\\)|std\\(\\)", x = all_features, value = TRUE)
}

get_measurements_with_names <- function() {
  xtest <- read.table("data/UCI HAR Dataset/test/X_test.txt", header = FALSE)
  xtrain <- read.table("data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
  
  measurements <- rbind(xtest, xtrain)
  colnames(measurements) <- get_features()
  
  measurements
}

get_activity_labels <- function() {
  ytest <- read.table("data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
  ytrain <- read.table("data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
  
  activity_listing <- rbind(ytest,ytrain) %>%
    rename(activity_id = V1)
  
  activity_labels <- read.table("data/UCI HAR Dataset/activity_labels.txt", header = FALSE) %>%
    rename(activity_id = V1,  activity_label = V2)
  
  activity_listing_with_labels <- merge(activity_listing, activity_labels)
  select(activity_listing_with_labels,activity_label)
}

get_subjects <- function() {
  subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
  subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
  
  rbind(subject_test,subject_train) %>%
    rename(subject_id = V1)
}

get_required_measurements <- function() {
  all_measurements <- get_measurements_with_names()
  required_features <- get_required_feature_names()
  
  all_measurements[,required_features]
}

get_required_data <- function() {
  required_measurements <- get_required_measurements()
  activity_labels <- get_activity_labels()
  subjects <- get_subjects()
  
  cbind(subjects, activity_labels, required_measurements)
}

create_averaged_data <- function(input_data) {
  grouped_data <- group_by(input_data,subject_id,activity_label)
  summarise_each(grouped_data,funs(mean))
}

save_data_to_output <- function(data, filename) {
  dir.create("output",showWarnings = FALSE)
  write.table(averaged_data,file = paste("output/", filename), row.names = FALSE)
}

required_data <- get_required_data()
averaged_data <- create_averaged_data(required_data)
save_data_to_output(averaged_data,"averaged_data.txt")
