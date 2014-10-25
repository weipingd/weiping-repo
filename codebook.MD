---
title: "codebook"
author: "weiping"
date: "Saturday, October 25, 2014"
output: html_document
---

Task1: Merge the training and test data sets to create one data set.

Read below source files into tables:
features.txt, activity_labels.txt, subject_test.txt, x_test.txt, y_test.txt, subject_train.txt, x_train.txt, y_train.txt

assign column names to each data set and merge to create one data set.

Task2: Extract only the measurements on the mean and standard deviation for each measurement.

Exract data of mean and stddev columns.

Task3: Use descriptive activity names to name the activities in the data set.

Merge data sets with activity type table.

Task4: Appropriately labels the data set with descriptive variable names.

Use gsub to replace unmeaningful labels.

Task5: Creates a second independent data set with the average of each variable for each activity and each subject.


