fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file( fileUrl, "dataset.zip")

unzip("dataset.zip")

XTrain <<- read.table("UCI HAR Dataset/train/X_train.txt")
XTest <<- read.table("UCI HAR Dataset/test/X_test.txt")

# Merge the training and the test sets to create one data set.

merged <- rbind(XTrain, XTest)

featureNames <- read.table("UCI HAR Dataset/features.txt")[, 2]
names(merged) <- featureNames

# Extract only the measurements on the mean and standard deviation for each measurement.
# Limit to columns with feature names matching mean() or std():

matches <- grep("(mean|std)\\(\\)", names(merged))
merged_limited <- merged[, matches]

# Get the activity data and map to nicer names:

yTrain <- read.table("UCI HAR Dataset/train/y_train.txt")
yTest  <- read.table("UCI HAR Dataset/test/y_test.txt")
yMerged <- rbind(yTrain, yTest)[, 1]

activityNames <-
    c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
activities <- activityNames[yMerged]

  
# Appropriately label the data set with descriptive variable names.
# Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
# Remove extra dashes and BodyBody naming error from original feature names

  names(merged_limited) <- gsub("^t", "Time", names(merged_limited))
  names(merged_limited) <- gsub("^f", "Frequency", names(merged_limited))
  names(merged_limited) <- gsub("-mean\\(\\)", "Mean", names(merged_limited))
  names(merged_limited) <- gsub("-std\\(\\)", "StdDev", names(merged_limited))
  names(merged_limited) <- gsub("-", "", names(merged_limited))
  names(merged_limited) <- gsub("BodyBody", "Body", names(merged_limited))

# Add activities and subject with nice names
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
subjectTest  <- read.table("UCI HAR Dataset/test/subject_test.txt")
subjects <- rbind(subjectTrain, subjectTest)[, 1]

tidy <- cbind(Subject = subjects, Activity = activities, merged_limited)

 # Create a second, independent tidy data set with the average of each variable for each activity and each subject.
 library(plyr)

 # Column means for all but the subject and activity columns
 limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
 tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
 names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])

  # Write file
  write.table(tidyMeans, "tidyMeans.csv", row.names = FALSE)

  # Also return data
  head(tidyMeans)


