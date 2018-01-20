require('dplyr')

setwd('C:/Users/user/Desktop/Work/data science courses/data/getting and cleaning data')

xtest <- read.table('./UCI HAR Dataset/test/X_test.txt')
ytest <- read.table('./UCI HAR Dataset/test/Y_test.txt')
subjecttest <- read.table('./UCI HAR Dataset/test/subject_test.txt')

xtrain <- read.table('./UCI HAR Dataset/train/X_train.txt')
ytrain <- read.table('./UCI HAR Dataset/train/Y_train.txt')
subjecttrain <- read.table('./UCI HAR Dataset/train/subject_train.txt')

features <- read.table('./UCI HAR Dataset/features.txt')
activitylabels <- read.table('./UCI HAR Dataset/activity_labels.txt')

######PULL TEST DATA
#get columnnames and index for xtest
names(xtest) = as.vector(features[,2])
xtest$rnum <- 1:nrow(xtest)

#same thing for ytest
ytest$rnum <- 1:nrow(ytest)
names(ytest) = c('activitynumber', 'rnum')

#and for subjecttest
subjecttest$rnum <- 1:nrow(subjecttest)
names(subjecttest) = c('subject', 'rnum')

#set activitylabel names for joining - only have to do this once
names(activitylabels) = c("activitynumber", 'activitylabel')

#merge datasets together, keep only the columns we are interested in
mergetestdata <- left_join(ytest,subjecttest, by='rnum') %>% 
                  left_join(.,activitylabels, by='activitynumber') %>%
                  left_join(., xtest, by='rnum') %>% 
                  select(matches("mean|std|activitylabel|subject"))


######PULL TRAINING DATA
#get columnnames and index for xtrain
names(xtrain) = as.vector(features[,2])
xtrain$rnum <- 1:nrow(xtrain)

#same thing for ytrain
ytrain$rnum <- 1:nrow(ytrain)
names(ytrain) = c('activitynumber', 'rnum')

#and for subjecttrain
subjecttrain$rnum <- 1:nrow(subjecttrain)
names(subjecttrain) = c('subject', 'rnum')

#merge datasets together, keep only the columns we are interested in
mergetraindata <- left_join(ytrain,subjecttrain, by='rnum') %>% 
      left_join(.,activitylabels, by='activitynumber') %>%
      left_join(., xtrain, by='rnum') %>% 
      select(matches("mean|std|activitylabel|subject"))

#####COMBINE DATA
tidydata <- rbind(mergetraindata, mergetestdata)
#remove unnecessary characters
names(tidydata) = gsub("-|\\(|\\)|,", "",names(tidydata))

#####GET AVERAGES
tidyavg <- tidydata %>% group_by(activitylabel, subject) %>% 
            summarise_all(funs(mean))

