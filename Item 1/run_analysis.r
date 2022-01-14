library(dplyr)

importDataset <- function() {
    activity_labels <<- read.table("specdata/activity_labels.txt", col.names = c("activity_id", "activity_name"))
    feature_labels <<- read.table("specdata/features.txt", col.names = c("feature_id", "feature_name"))

    train_set <<- read.table("specdata/train/X_train.txt", col.names = feature_labels$feature_name)
    train_labels <<- read.table("specdata/train/y_train.txt", col.names = c("activity_id"))
    train_subject <<- read.table("specdata/train/subject_train.txt", col.names = c("subject_id"))

    test_set <<- read.table("specdata/test/X_test.txt", col.names = feature_labels$feature_name)
    test_labels <<- read.table("specdata/test/y_test.txt", col.names = c("activity_id"))
    test_subject <<- read.table("specdata/test/subject_test.txt", col.names = c("subject_id"))

    print("Datasets imported successfully!")
}

mergeDataset <- function() {
    if (!(exists("activity_labels") &&
        exists("feature_labels") &&
        exists("train_set") &&
        exists("train_labels") &&
        exists("train_subject") &&
        exists("test_set") &&
        exists("test_labels") &&
        exists("test_subject"))) {
        importDataset()
    }
    
    merged_subjects <- rbind(train_subject, test_subject)
    merged_sets <- rbind(train_set, test_set)
    merged_labels <- rbind(train_labels, test_labels)
    merged_labels$activity_name <- activity_labels$activity_name[match(merged_labels$activity_id, activity_labels$activity_id)]

    merged_dataset <<- cbind(merged_subjects, merged_labels, merged_sets)

    display(head(merged_dataset))
    print("Train and test datasets merged successfully!")
}

extractMeansStd <- function() {
    if (!exists("merged_dataset")) {
        mergeDataset()
    }
    
    merged_dataset %>%
        select("subject_id", "activity_name", contains("mean") | contains("std"))
}

generateTidyMeans <- function() {    
    tidy_means <<- extractMeansStd() %>% 
        group_by(subject_id, activity_name) %>%
        summarize_at(base_data, -c(1:2), mean)
    
    write.table(tidy_means, file = "tidy_means.txt", row.names = FALSE)
}