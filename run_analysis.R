# This script is written for the course project 
# "Getting and Cleaning Data" on Coursera, basically it
#  prepares a tidy data that can be used for later analysis.

# 0. Read the data into R.
  ## Before running the code, remember to put the  
  ## "UCI HAR Dataset" folder in your work directory.
TrainingSet<-read.table("./UCI HAR Dataset/train/X_train.txt")
TrainingLabel<-read.table("./UCI HAR Dataset/train/y_train.txt")
TrainingSubject<-read.table("./UCI HAR Dataset/train/subject_train.txt")
TestSet<-read.table("./UCI HAR Dataset/test/X_test.txt")
TestLabel<-read.table("./UCI HAR Dataset/test/y_test.txt")
TestSubject<-read.table("./UCI HAR Dataset/test/subject_test.txt")
features<-read.table("./UCI HAR Dataset/features.txt")

# 1. Merge the training and the test sets to create one data set.
training<-cbind(TrainingLabel,TrainingSubject,TrainingSet)
test<-cbind(TestLabel,TestSubject,TestSet)
data<-rbind(training,test)
  ## Rename the columns by the features variable
names(data)<-c("Label","Subject",as.character(features[,2]))
str(data)
  ## Convert Subject variable into factor
data$Subject<-factor(data$Subject)

# 2. Extract only the measurements on the mean 
#    and standard deviation for each measurement.
  
  ## We can tell from the features_info.txt file that, 
  ## measurements on the mean all have the word "mean"
  ## within the feature names. However, "Mean" with a 
  ## capital letter is not included since it is used
  ## specifically in calculating the angels. So we can
  ## simply grip those variables with "mean" in the names.
Mean<-data[,grep("mean",names(data))]
  ## For standard deviation, the word would be "std".
SD<-data[,grep("std",names(data))]
  ## Merge them together and add the labels
Mean_SD<-cbind(data[,c(1,2)],Mean,SD)

# 3. Uses descriptive activity names 
#    to name the activities in the data set.
  
  ## Since we can easily find the relations between labels 
  ## and activity names in the "activity_labels.txt" file,
  ## I am going to convert the Label variable into factors
  ## and assign them to the according labels found in the file.
Mean_SD$Label<-factor(Mean_SD$Label, 
                   labels=c("WALKING","WALKING_UPSTAIRS",
                            "WALKING_DOWNSTAIRS","SITTING",
                            "STANDING","LAYING"))
names(Mean_SD)[1]<-"Activity"
# 4. Appropriately labels the data set 
#    with descriptive variable names.
names(Mean_SD)<-gsub("^t", "Time", names(Mean_SD))
names(Mean_SD)<-gsub("^f", "Frequency", names(Mean_SD))
names(Mean_SD)<-gsub("Acc", "Accelerometer", names(Mean_SD))
names(Mean_SD)<-gsub("Gyro", "Gyroscope", names(Mean_SD))
names(Mean_SD) <- gsub('\\.mean',".Mean",names(Mean_SD))
names(Mean_SD) <- gsub('\\.std',".StandardDeviation",names(Mean_SD))

  ## Since we have already renamed the columns in the first
  ## step, and attempts to modify the names of the measurements
  ## will only make the data messier (e.g. using the name
  ## "stdFreqBodyAccJerkMag" to replace "fBodyAccJerkMag-std()"). 
  ## Therefore, I write all the descriptive information in
  ## the CODEBOOK! Check that please!

# 5. Create a tidy data set with the average of 
#    each variable for each activity and each subject.

library(reshape2)
  ## Convert it into a castable melted data frame
Melt<-melt(Mean_SD,id=c("Activity","Subject") )
  ## Reshape the data and calculate means of each varibles,
  ## after grouping them by their activities and subjects
Average<-dcast(Melt,Activity+Subject~variable,mean)
head(Average,3)

# 6. Output the tidy dataset to a txt file
write.table(Mean_SD,file = "TidyDataset.txt")
write.table(Average,file = "TidyAverage.txt")
 ## when reading the data back into R, remember to set that sep=" "
