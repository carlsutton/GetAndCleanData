#  course project Get and clean data
#  the follwing  lines of code "1. Merges the training and the test sets 
#  to create one data set."
#  read in subject, train and test data, labeling columns as subject_id for merge
#  purposes
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",
                           col.names=c("subject_id"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",
                           col.names = c("subject_id"))
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt",
                      col.names = c("activity_id"))
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI har dataset/test/y_test.txt",
                     col.names = c("activity_id"))
# xtest <- dim(x_test);# xtest;# ytest <- dim(y_test);# ytest
# xtrain <-dim(x_train);# xtrain;# ytrain <- dim(y_train);# ytrain
#  check that the structure of the X_train and X_test data sets are identical
identical(str(x_train),str(x_test))
#  check that the structure of y_train and y_test are identical
identical(str(y_test),str(y_train))
#  the structures of the data frames are compatible
#  now I need to insert columns to merge by
subject_train$ID <- as.numeric(rownames(subject_train))
subject_test$ID <- as.numeric(rownames(subject_test))
x_train$ID <- as.numeric(rownames(x_train))
y_train$ID <- as.numeric(rownames(y_train))
x_test$ID <- as.numeric(rownames(x_test))
y_test$ID <- as.numeric(rownames(y_test))
dim(subject_train);dim(x_train);dim(y_train);dim(subject_test);dim(x_test)
dim(y_test)
#  the test data frames have the same number of rows
#  the train data frames have the same number of rows
#  the x and y training data frames have a common column labelled ID
#  the x and y test data frames have a common column labeled ID
train <- merge(subject_train, y_train, all = TRUE)
train <- merge(train, x_train, all = TRUE)
test <- merge(subject_test, y_test, all = TRUE)
#  merge test and x_test, ie, the x and y data for test
test <- merge(test, x_test, all = TRUE)
#  are columns ready for the final merge?
test.name <- colnames(test)
train.name <- colnames(train)
identical(test.name, train.name)
dim(test); dim(train)
#  combine test and train using rowbind
total.data <- rbind(test, train)
dim(total.data)

#  The following code: "2.  Extracts only the measurements
#  on the mean and standard deviation for each measurement".
#  Read table for data labels. In keeping with given file names, I will label these 
#  "features" to keep the names straight in my mind.
features <- read.table("UCI HAR Dataset/features.txt", 
                            col.names = c("feature_id", "feature_label"),)
#  now to narrow it down to means and standard deviations columns
#  going to spend some hours on character string functions
#  grepl returns a logical vector which I can use to subset with
#  grepl(pattern, x, ignore.case = FALSE, perl = FALSE,
#  fixed = FALSE, useBytes = FALSE)
selected_features <- features[grepl("mean\\(\\)", features$feature_label) |
                                      grepl("std\\(\\)", features$feature_label), ]
head(selected_features)
head(total.data)
#  to extract columns from total.data, cols 1-3 must be preserved, then "mean"
#  and "std" colums get extracted and placed at col 4 position and beyond
#  i.e. c(1,2,3) preserves cols 1-3, and features_labels + 3 starts placing them after
data.selected <- total.data[, c(c(1,2,3), selected_features$feature_id + 3) ]
head(data.selected)
#  The following code: "3. Uses descriptive activity names to name the activities
#  in the data set"
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt",
                              col.names = c("activity_id", "activity_label"))
head(activity_labels); dim(activity_labels)
labeled.data.selected <- merge(data.selected, activity_labels)

#  the following code "4. Appropriately labels the data set with descriptive
#  variable names."
#  I could learn to dislike dealing with character searches
selected_features$feature_label = gsub("\\(\\)","",selected_features$feature_label)
selected_features$feature_label = gsub("-",".",selected_features$feature_label)
for (i in 1:length(selected_features$feature_label)) {
        colnames(labeled.data.selected)[i + 3] <- selected_features$feature_label[i]
}
data.col.named <- labeled.data.selected
                 
#  the following code: "5. From the data set in step 4, creates a second,
#  independent tidy data set with the average of each variable for each 
#  activity and each subject.
#  Eliminate unneeded columns first with elim
elim <- c("ID","activity_label")
data.col.named <- data.col.named[, !(names(data.col.named) %in% elim)]
ave.data <- aggregate(data.col.named, by = list(subject = data.col.named$subject_id,
                activity = data.col.named$activity_id), FUN = mean, na.rm=TRUE)
dim(ave.data)
elim <- c("subject", "activity")
ave.data <- ave.data[, !(names(ave.data) %in% elim)]
dim(ave.data)
ave.data <- merge(ave.data,activity_labels)
dim(ave.data)
write.table(file="submit.txt", x=ave.data, row.name = FALSE)



                


