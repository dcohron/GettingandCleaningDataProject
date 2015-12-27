# Course Project
# Getting and Cleaning Data

# install libraries for later use
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(sqldf)

# files will be needed later
file1<- "x_train.txt"
file2<- "y_train.txt"
file3<- "features.txt"
file4<- "activity_labels.txt"
file5<- "subject_train.txt"
file6<- "x_test.txt"
file7<- "y_test.txt"
file8<- "subject_test.txt"

# read in files for manipulation
# starting with x_train which will be the main file we are building
main<- fread(file1)
ytrain<- fread(file2)
features<- fread(file3)
activities<- fread(file4)
subtrain<- fread(file5)
xtest<- fread(file6)
ytest<- fread(file7)  
subtest<- fread(file8)


# ASSIGNMENT #1- merge the training and test sets into one data set (main)

# now let's reconstitute a single data frame from the parts
# fitting it back together like a puzzle
# merge test and train sets of x, y and sub
main<- rbind(main, xtest)
ytrain<- rbind(ytrain, ytest)
subtrain<-rbind(subtrain, subtest)

# add columns to main
main<-cbind(ytrain, subtrain, main)

# convert factor to names for variable names and add to columns
addedfeatures<- data.table(matrix(c(562,563,"Activity_Number","Subject_Number"),ncol=2,nrow=2))
features<-rbind(addedfeatures, features)
a<-make.names(features$V2)
colnames(main)<-a


# ASSIGNMENT #2- extracts only the mean and standard deviation measurements

# subset for mean and std columns
mainreduced<-select(main, matches("mean|std|Activity_Number|Subject_Number"))


# ASSIGNMENT #3- use descriptive activity names
#     add activities as row labels and reorder columns
#     so Activity_Number, Activity_Name and Subject_Number are first three columns
colnames(activities)<- c("Activity_Number", "Activity_Name")
mainreduced<-merge(activities, mainreduced, by= "Activity_Number")

# remove unneeded variables and data frames
rm(main, xtest, ytest, ytrain, subtest, subtrain, a, features, addedfeatures, activities)

# convert names to factors so they may group by
mainreduced$Activity_Name<- as.factor(mainreduced$Activity_Name)
levels(mainreduced$Activity_Name) <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")


# ASSIGNMENT #4- labels the data set with descriptive variable names
#     The names in the data set are already very descriptive,
#     Do text manipulation to clean up names

# remove "()" and "." from variable names to enhance readability
names(mainreduced)<- sub("()-", "", names(mainreduced))
names(mainreduced)<- gsub("\\.", "", names(mainreduced))


# ASSIGNMENT #5- create second tidy data set with average of each
#     variable for each activity and each subject

# group by Activity Name and Subject Number and get mean for each variable
tidymain<- group_by(mainreduced, Activity_Name, Subject_Number) %>% summarize_each(funs(mean))

# write tidy data subset as .txt file
fileout<-"tidydata.txt"
write.table(tidymain, file=fileout, row.names=FALSE, col.names=TRUE)



