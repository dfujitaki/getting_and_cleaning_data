# Codebook

# Filenames
From the raw data readme file (only including relevant tables):
- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

Inertial Signals files were not included as they had no description file and would have been removed at the mean/standard deviation filter anyway.

# Description of Code
The only package needed is dplyr.  The first section reads in all the required text files.  The next sections are split into two pieces:
For both the trainng and test sets:
* Create join index, attach observation names, rename subject and activitiy columns for joins later.
* Join the tables together to produce a tidy dataset filtered only on our columns of interest.
Once both the training and test sets are created, I used rbind to append the two sets together and then used dplyr to summarize the columns by subject and activity.

There are 30 subjects and 6 activities (walking, walking upstairs, walking downstairs, sitting, standing, and laying) for a total of 180 observations in the tidy average dataset.

See run_analysis.R for more detail.

# Finalized Variable List:
XYZ refers to the direction of the movement.
Per Hadley's paper on tidy data for an individual dataset, each measurment should have an individual column and each observation should be in a different row.  Each observation (row) contains one measurement of the following variables:
tBodyAccmeanX

tBodyAccmeanY

tBodyAccmeanZ

tBodyAccstdX

tBodyAccstdY

tBodyAccstdZ

tGravityAccmeanX

tGravityAccmeanY

tGravityAccmeanZ

tGravityAccstdX

tGravityAccstdY

tGravityAccstdZ

tBodyAccJerkmeanX

tBodyAccJerkmeanY

tBodyAccJerkmeanZ

tBodyAccJerkstdX

tBodyAccJerkstdY

tBodyAccJerkstdZ

tBodyGyromeanX

tBodyGyromeanY

tBodyGyromeanZ

tBodyGyrostdX

tBodyGyrostdY

tBodyGyrostdZ

tBodyGyroJerkmeanX

tBodyGyroJerkmeanY

tBodyGyroJerkmeanZ

tBodyGyroJerkstdX

tBodyGyroJerkstdY

tBodyGyroJerkstdZ

tBodyAccMagmean

tBodyAccMagstd

tGravityAccMagmean

tGravityAccMagstd

tBodyAccJerkMagmean

tBodyAccJerkMagstd

tBodyGyroMagmean

tBodyGyroMagstd

tBodyGyroJerkMagmean

tBodyGyroJerkMagstd

fBodyAccmeanX

fBodyAccmeanY

fBodyAccmeanZ

fBodyAccstdX

fBodyAccstdY

fBodyAccstdZ

fBodyAccmeanFreqX

fBodyAccmeanFreqY

fBodyAccmeanFreqZ

fBodyAccJerkmeanX

fBodyAccJerkmeanY

fBodyAccJerkmeanZ

fBodyAccJerkstdX

fBodyAccJerkstdY

fBodyAccJerkstdZ

fBodyAccJerkmeanFreqX

fBodyAccJerkmeanFreqY

fBodyAccJerkmeanFreqZ

fBodyGyromeanX

fBodyGyromeanY

fBodyGyromeanZ

fBodyGyrostdX

fBodyGyrostdY

fBodyGyrostdZ

fBodyGyromeanFreqX

fBodyGyromeanFreqY

fBodyGyromeanFreqZ

fBodyAccMagmean

fBodyAccMagstd

fBodyAccMagmeanFreq

fBodyBodyAccJerkMagmean

fBodyBodyAccJerkMagstd

fBodyBodyAccJerkMagmeanFreq

fBodyBodyGyroMagmean

fBodyBodyGyroMagstd

fBodyBodyGyroMagmeanFreq

fBodyBodyGyroJerkMagmean

fBodyBodyGyroJerkMagstd

fBodyBodyGyroJerkMagmeanFreq

angletBodyAccMeangravity

angletBodyAccJerkMeangravityMean

angletBodyGyroMeangravityMean

angletBodyGyroJerkMeangravityMean

angleXgravityMean

angleYgravityMean

angleZgravityMean






