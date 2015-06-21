# Performs an analysis on the data and saves the results to `tiny-data.txt`
runAnalysis <- function() {
	merged <- getMergedDataSet()

	# Filter it for only subject, activity, mean or std
	columns = colnames(merged)
	data <- merged[ , grepl( "mean", columns) | grepl("std", columns) | columns %in% c("subject", "activity_label") ]

	# Order by the subject and activity
	data <- data[order(data$subject, data$activity_label),]

	# Rename the activity_label column to just activity
	data <- rename(data, activity = activity_label)

	data.melted <- melt(data, id.var = c("activity", "subject"))
	data.final <- dcast(data.melted, subject + activity ~ variable, mean)

	write.table(data.final, file="tidy-data.txt", row.names=FALSE)

	data
}

# Merge the training and the test sets to create one data set
getMergedDataSet <- function() {
	testSet <- getDataSet("test")
	trainingSet <- getDataSet("train")
	rbind(testSet, trainingSet)
}

# Get the data for a specific data set
# The `type` argument should be `test` or `train`
getDataSet <- function(type) {
	root <- "UCI HAR Dataset/"
	dataDirectory <- paste(root, type, "/", sep="")
	featurePath <- paste(dataDirectory, "X_", type, ".txt", sep="")
	subjectsPath <- paste(dataDirectory, "subject_", type, ".txt", sep="")
	activityIdsPath <- paste(dataDirectory, "y_", type, ".txt", sep="")
	activityLabelsPath <- paste(root, "activity_labels.txt", sep="")

	features <- read.table(featurePath)
	subjects <- read.table(subjectsPath)

	activityIds <- read.table(activityIdsPath)
	colnames(activityIds) <- c("activity")
	activityLabels <- read.table(activityLabelsPath)
	colnames(activityLabels) <- c("activity", "activity_label")
	activityData <- merge(activityIds, activityLabels, by="activity")

	data <- cbind(subjects, activityData, features)
	columns <- c("subject", colnames(activityData), getFeatureColumnNames())
	colnames(data) <- columns

	data
}

# Returns a vector of all of the features in the dataset
getFeatureColumnNames <- function() {
	as.vector(read.table("UCI HAR Dataset/features.txt")$V2)
}
