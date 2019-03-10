#################################################################################################
# Preliminary Steps:
#################################################################################################


# Ensure that the prerequisite packages are loaded before proceeding with the script
packages <- c("data.table", "dplyr")
sapply(packages, library, character.only=TRUE, quietly=TRUE)
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# get current Working Directory path and set it to 'curr_path'
curr_path <- getwd()

# set Project's URL to a variable
project_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# name the directory where to store data for our project
data_path <- "UCI HAR Dataset"

# use a variable for the file name of a zip file we are about to download
data_file <- "data_set.zip" 

# if a compressed file containing the Data Set for this project had NOT been downloaded previously
# THEN download it
if (!file.exists(data_file)) {
         download.file(project_url, data_file, mode = "wb")
} 

# decompress the (downloaded) data_file which contains the Data Set for our project
unzip(data_file)

# Set the Working Directory to the allocated sub-directory for the project's the Data Set  
setwd(file.path(curr_path, data_path))



# input 'training' subsets of data into 3 'training' subsets
trainingSubjects <- read.table(file.path(getwd(), "train", "subject_train.txt"))
trainingValues <- read.table(file.path(getwd(), "train", "X_train.txt"))
trainingActivity <- read.table(file.path(getwd(), "train", "y_train.txt"))

# input 'test' subsets of data into 3 'test' subsets
testSubjects <- read.table(file.path(getwd(), "test", "subject_test.txt"))
testValues <- read.table(file.path(getwd(), "test", "X_test.txt"))
testActivity <- read.table(file.path(getwd(), "test", "y_test.txt"))

# input 'features' without converting text labels 'factors'
features <- read.table(file.path(getwd(), "features.txt"), as.is = TRUE)

activities <- read.table(file.path(getwd(), "activity_labels.txt"))

#rename column names of 'activities' table to meaningful column names
colnames(activities) <- c("activityId", "activityLabel")

#################################################################################################
# Part 1 - Merge the training and the test data sets to create a single Data Set
#################################################################################################

# combine indidvidual data tables into a single data set
complete_data_set <- rbind(
        cbind(trainingSubjects, trainingValues, trainingActivity),
        cbind(testSubjects, testValues, testActivity)
)

# assign column names
colnames(complete_data_set) <- c("subject", features[, 2], "activity")

# clear memory from redundant sub-sets of data
rm(trainingSubjects, trainingValues, trainingActivity,
   testSubjects, testValues, testActivity)

#################################################################################################
# Part 2 - Extract only the measurements on the mean and standard deviation for each measurement
#################################################################################################

# determine which column names to keep based on an 'OR' pattern by assigning 'TRUE'
# to positive matches
keep_columns <- grepl("subject|activity|mean|std", colnames(complete_data_set))

# reduce a total data set to the columns one resolved to keep above
complete_data_set <- complete_data_set[, keep_columns]



#################################################################################################
# Step 3 -  Use descriptive activity names to name the activities in the data set
#################################################################################################

# categorising 'activities' within the complete Data Set into 6 'levels'
# and giving them meaningful 'labels'
complete_data_set$activity <- factor(complete_data_set$activity,levels = activities[, 1], labels = activities[, 2])


#################################################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
#################################################################################################

# assign Column names of the entire data Set to 'complete_data_set_cols'
complete_data_set_cols <- colnames(complete_data_set)

# remove characters such as ']' and '-' from column names
complete_data_set_cols <- gsub("[\\(\\)-]", "", complete_data_set_cols)

# correct 'BodyBody' sub string within a column name...
complete_data_set_cols <- gsub("BodyBody", "Body", complete_data_set_cols)

# ...and give meaningful names to columns by substituting abbreviations, such as: change 'std'
# to 'Standard Deviation'
complete_data_set_cols <- gsub("Acc", "Accelerometer", complete_data_set_cols)
complete_data_set_cols <- gsub("Freq", "Frequency", complete_data_set_cols)
complete_data_set_cols <- gsub("Gyro", "Gyroscope", complete_data_set_cols)
complete_data_set_cols <- gsub("Mag", "Magnitude", complete_data_set_cols)
complete_data_set_cols <- gsub("mean", "Mean", complete_data_set_cols)
complete_data_set_cols <- gsub("std", "StandardDeviation", complete_data_set_cols)
complete_data_set_cols <- gsub("^f", "frequencyDomain", complete_data_set_cols)
complete_data_set_cols <- gsub("^t", "timeDomain", complete_data_set_cols)

# subsitute previous column names with newly-created meaningful column names
colnames(complete_data_set) <- complete_data_set_cols

#################################################################################################
# Step 5 - Create a Tidy Data Set with the average of each variable for each activity 
#			and each subject
#################################################################################################

# group by 'subject' and 'activity' column in the resulting Data Set;
# and apply 'mean' function to data in all columns
complete_data_set_Means <- complete_data_set %>%
        group_by(subject, activity) %>% summarise_all(mean)

# Output the resulting Tidy Dataset which contains the 'mean' of all 'activity' and 'subject' variables
write.table(complete_data_set_Means, "tidy_data.txt", row.names = FALSE, quote = FALSE)

#################################################################################################
# At the end of the script reset the Working Directory to its original setting or the sake of 
# preserving uniformity if in the future iterations of this script are going to be required

setwd('./..')