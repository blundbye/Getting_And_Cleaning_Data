# Getting and Cleaning Data - Course Project (week 4)

This is the project for the Getting and Cleaning Data course.
The repository consists of the following files:

* README.md - This file
* Codebook.md - Description of the output produced by the R script `run_analysis.R`
* run_analysis.R - The R-script (see below) 
* TidyData.txt - The tidy dataset (tab-delimited text file)

The R-script, `run_analysis.R`, does the following:

1. Downloads and unzips the dataset  
2. Loads both the training and test datasets, keeping only those columns which
   reflect a mean or standard deviation
3. Loads the activity and subject data for each dataset, and merges those
   columns with the dataset
4. Merges training and test datasets
5. Loads activity and feature info
6. Converts all relevant features into factor columns
7. Creates a tidy dataset, `TidyData.txt` that consists of the average (mean) value 
   of each variable for each subject, activity and type of feature
