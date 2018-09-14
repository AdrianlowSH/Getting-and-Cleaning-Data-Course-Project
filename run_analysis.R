## Load Library

library(magrittr)
library(data.table)

## Step 1: Read files from directory

# Read all test files
Subject.Test <- read.table(file.path("test", "subject_test.txt"), header = FALSE)
Features.Test <- read.table(file.path("test", "X_test.txt"), header = FALSE)
Activity.Test <- read.table(file.path("test", "Y_test.txt"), header = FALSE)
Subject.Train <- read.table(file.path("train", "subject_train.txt"), header = FALSE)
Features.Train <- read.table(file.path("train", "X_train.txt"), header = FALSE)
Activity.Train <- read.table(file.path("train", "Y_train.txt"), header = FALSE)

## Step 2: Merge the test and train sets to one data set

# Merge each test and train file based on topic
Data.Subject <- rbind(Subject.Test, Subject.Train)
Data.Features <- rbind(Features.Test, Features.Train)
Data.Activity <- rbind(Activity.Test, Activity.Train)

# set name to variables
names(Data.Subject) <- c("Subject")
names(Data.Activity) <- c("Activity")
Data.Features.Names <- read.table(file.path("features.txt"), header = FALSE)
names(Data.Features) <- Data.Features.Names$V2

# Merge columns together
Data.combined <- cbind(Data.Subject, Data.Activity) %>% cbind(., Data.Features)

# Extract mean and Std columns with subject and activities to one set
col.withMeanSTD <- grep(".*Mean.*|.*Std.*", names(Data.combined), ignore.case = TRUE)
col.required <- c(1, 2, col.withMeanSTD)
Data.Extracted <- Data.combined[, col.required]

## Step 3: Replace activity names with descriptive activity names
Activity.Label <- read.table("activity_labels.txt")
Data.Extracted$Activity <- as.character(Data.Extracted$Activity)
for(i in 1:6){
  Data.Extracted$Activity[Data.Extracted$Activity == i] <- as.character(Activity.Label[i,2])
}

## Step 4: Appropriately labels the data set with descriptive variable names
names(Data.Extracted) <- gsub("^t", "Time", names(Data.Extracted))
names(Data.Extracted) <- gsub("Acc", "Accelerometer", names(Data.Extracted))
names(Data.Extracted) <- gsub("-mean()", "Mean", names(Data.Extracted))
names(Data.Extracted) <- gsub("-std()", "Std", names(Data.Extracted))
names(Data.Extracted) <- gsub("Gyro", "Gyroscope", names(Data.Extracted))
names(Data.Extracted) <- gsub("Mag", "Magnitude", names(Data.Extracted))
names(Data.Extracted) <- gsub("^f", "Frequency", names(Data.Extracted))
names(Data.Extracted) <- gsub("BodyBody", "Body", names(Data.Extracted))
names(Data.Extracted) <- gsub("angle", "Angle", names(Data.Extracted))
names(Data.Extracted) <- gsub("tBody", "Body", names(Data.Extracted))
names(Data.Extracted) <- gsub("gravity", "Gravity", names(Data.Extracted))

## Step 5: Create a tidy data set based on activity and subject
Data.Extracted$Subject <- as.factor(Data.Extracted$Subject)
Data.Extracted <- data.table(Data.Extracted)
Data.Tidied <- aggregate(. ~Subject + Activity, Data.Extracted, mean)
write.table(Data.Tidied, "TidiedData.txt", row.names = false)