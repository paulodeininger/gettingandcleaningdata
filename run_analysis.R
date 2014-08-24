## Tools for performing the analysis on UCI HAR Dataset

## Reading data
## Appropriately labels the data set with descriptive variable names. 

activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
colnames(activityLabels) <- c("ActivityId", "Activity")
featureLabel <- read.table("./UCI HAR Dataset/features.txt")
colnames(featureLabel) <- c("FeatureId", "Feature")

trainingLabels <- read.table("./UCI HAR Dataset/train/y_train.txt")
colnames(trainingLabels) <- "ActivityId"
trainingActivity <- data.frame(activityLabels[trainingLabels[, 1], "Activity"])
colnames(trainingActivity) <- "Activity"
trainingSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
colnames(trainingSubjects) <- "Subject"
trainingSet <- read.table("./UCI HAR Dataset/train/X_train.txt")
colnames(trainingSet) <- featureLabel$Feature

training <- cbind(trainingLabels, trainingActivity, trainingSubjects, trainingSet)

testLabels <- read.table("./UCI HAR Dataset/test/y_test.txt")
colnames(testLabels) <- "ActivityId"
testActivity <- data.frame(activityLabels[testLabels[, 1], "Activity"])
colnames(testActivity) <- "Activity"
testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
colnames(testSubjects) <- "Subject"
testSet <- read.table("./UCI HAR Dataset/test/X_test.txt")
## Uses descriptive activity names to name the activities in the data set
colnames(testSet) <- featureLabel$Feature

test <- cbind(testLabels, testActivity, testSubjects, testSet)

## Merges the training and the test sets to create one data set.
mergedDataSet <- rbind(training, test)

## Extracts only the measurements on the mean and standard deviation for each measurement.
measurementsLogical <- grepl("mean|std", featureLabel$Feature)
mergedDsOnlyMeanStd <- mergedDataSet[c(TRUE, TRUE, TRUE, measurementsLogical)]

## Creates a second, independent tidy data set with the average of each variable 
## for each activity and each subject.
tidy <- aggregate(mergedDsOnlyMeanStd[,4] ~ 
                      mergedDsOnlyMeanStd$ActivityId + mergedDsOnlyMeanStd$Subject
                  , data = mergedDsOnlyMeanStd
                  , FUN = mean)

for (x in 5:length(mergedDsOnlyMeanStd)) {
    n <- featureLabel$Feature[x]
    tmp <- aggregate(mergedDsOnlyMeanStd[,x] ~ 
                         mergedDsOnlyMeanStd$ActivityId + mergedDsOnlyMeanStd$Subject
                     , data = mergedDsOnlyMeanStd
                     , FUN = mean)[, 3]
    tidy = cbind(tidy, tmp)
}

names(tidy) <- names(mergedDsOnlyMeanStd)[c(1,3:length(mergedDsOnlyMeanStd))]

## Generate the tidy.txt
write.table(tidy, file = "tidy.txt", row.name = FALSE)

tidy