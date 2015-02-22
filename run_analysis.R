############################################################
### Part 1: Read in data & combine train and test sets #####
############################################################

#read in training data
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")

#read in testing data
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")

#combine train and test sets
x <- rbind(x_train,x_test)
y <- rbind(y_train,y_test)
subject <- rbind(subject_train,subject_test)

############################################################
### Part 2: Extract only mean and standard deviation #######
############################################################

#get columns for mean and average
features <- read.table("features.txt")
meanstd <- c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,
             86,121,122,123,124,125,126,161,162,163,164,
             165,166,201,202,214,215,227,228,240,241,253,
             254,266,267,268,269,270,271,345,346,347,348,
             349,350,424,425,426,427,428,429,503,504,516,
             517,529,530,542,543)
colnames <- features[meanstd,2]

#extract only those columns from x
x_new <- x[,meanstd]

############################################################
### Part 3: Use descriptive activity names for dataset #####
############################################################

activity_labels <- read.table("activity_labels.txt")

label <- function(a){
  b <- activity_labels[a,2]
  return(b)
}

activities <- sapply(y,label)

############################################################
### Part 4: Label dataset with descriptive variable names ##
############################################################

#make labels for variables
final_col_names <- c('SubjectID','Activity','MeanBodyAccelerometerMeasurement-XDirection',
                     'MeanBodyAccelerometerMeasurement-YDirection','MeanBodyAccelerometerMeasurement-ZDirection',
                     'StandardDeviationBodyAccelerometerMeasurement-XDirection','StandardDeviationBodyAccelerometerMeasurement-YDirection',
                     'StandardDeviationBodyAccelerometerMeasurement-ZDirection','MeanGravityAccelerometerMeasurement-XDirection',
                     'MeanGravityAccelerometerMeasurement-YDirection','MeanGravityAccelerometerMeasurement-ZDirection',
                     'StandardDeviationGravityAccelerometerMeasurement-XDirection','StandardDeviationGravityAccelerometerMeasurement-YDirection',
                     'StandardDeviationGravityAccelerometerMeasurement-ZDirection','MeanBodyAccelerometerMeasurement-XDirection-JerkSignal',
                     'MeanBodyAccelerometerMeasurement-YDirection-JerkSignal','MeanBodyAccelerometerMeasurement-ZDirection-JerkSignal',
                     'StandardDeviationBodyAccelerometerMeasurement-XDirection-JerkSignal','StandardDeviationBodyAccelerometerMeasurement-YDirection-JerkSignal',
                     'StandardDeviationBodyAccelerometerMeasurement-ZDirection-JerkSignal','MeanBodyGyroscopeMeasurement-XDirection',
                     'MeanBodyGyroscopeMeasurement-YDirection','MeanBodyGyroscopeMeasurement-ZDirection',
                     'StandardDeviationBodyGyroscopeMeasurement-XDirection','StandardDeviationBodyGyroscopeMeasurement-YDirection',
                     'StandardDeviationBodyGyroscopeMeasurement-ZDirection','MeanBodyGyroscopeMeasurement-XDirection-JerkSignal',
                     'MeanBodyGyroscopeMeasurement-YDirection-JerkSignal','MeanBodyGyroscopeMeasurement-ZDirection-JerkSignal',
                     'StandardDeviationBodyGyroscopeMeasurement-XDirection-JerkSignal','StandardDeviationBodyGyroscopeMeasurement-YDirection-JerkSignal',
                     'StandardDeviationBodyGyroscopeMeasurement-ZDirection-JerkSignal','MeanBodyAccelerometerMeasurement-Magnitude',
                     'StandardDeviationBodyAccelerometerMeasurement-Magnitude','MeanGravityAccelerometerMeasurement-Magnitude',
                     'StandardDeviationGravityAccelerometerMeasurement-Magnitude','MeanBodyAccelerometerMeasurement-Magnitude-JerkSignal',
                     'StandardDeviationBodyAccelerometerMeasurement-Magnitude-JerkSignal','MeanBodyGyroscopeMeasurement-Magnitude',
                     'StandardDeviationBodyGyroscopeMeasurement-Magnitude','MeanBodyGyroscopeMeasurement-Magnitude-JerkSignal',
                     'StandardDeviationBodyGyroscopeMeasurement-Magnitude-JerkSignal','MeanBodyAccelerometerMeasurement-XDirection-FastFourierTransform',
                     'MeanBodyAccelerometerMeasurement-YDirection-FastFourierTransform','MeanBodyAccelerometerMeasurement-ZDirection-FastFourierTransform',
                     'StandardDeviationBodyAccelerometerMeasurement-XDirection-FastFourierTransform','StandardDeviationBodyAccelerometerMeasurement-YDirection-FastFourierTransform',
                     'StandardDeviationBodyAccelerometerMeasurement-ZDirection-FastFourierTransform','MeanBodyAccelerometerMeasurement-XDirection-JerkSignal-FastFourierTransform',
                     'MeanBodyAccelerometerMeasurement-YDirection-JerkSignal-FastFourierTransform','MeanBodyAccelerometerMeasurement-ZDirection-JerkSignal-FastFourierTransform',
                     'StandardDeviationBodyAccelerometerMeasurement-XDirection-JerkSignal-FastFourierTransform','StandardDeviationBodyAccelerometerMeasurement-YDirection-JerkSignal-FastFourierTransform',
                     'StandardDeviationBodyAccelerometerMeasurement-ZDirection-JerkSignal-FastFourierTransform','MeanBodyGyroscopeMeasurement-XDirection-FastFourierTransform',
                     'MeanBodyGyroscopeMeasurement-YDirection-FastFourierTransform','MeanBodyGyroscopeMeasurement-ZDirection-FastFourierTransform',
                     'StandardDeviationBodyGyroscopeMeasurement-XDirection-FastFourierTransform','StandardDeviationBodyGyroscopeMeasurement-YDirection-FastFourierTransform',
                     'StandardDeviationBodyGyroscopeMeasurement-ZDirection-FastFourierTransform','MeanBodyAccelerometerMeasurement-Magnitude-FastFourierTransform',
                     'StandardDeviationBodyAccelerometerMeasurement-Magnitude-FastFourierTransform','MeanBodyAccelerometerMeasurement-Magnitude-JerkSignal-FastFourierTransform',
                     'StandardDeviationBodyAccelerometerMeasurement-Magnitude-JerkSignal-FastFourierTransform','MeanBodyGyroscopeMeasurement-Magnitude-FastFourierTransform',
                     'StandardDeviationBodyGyroscopeMeasurement-Magnitude-FastFourierTransform','MeanBodyGyroscopeMeasurement-Magnitude-JerkSignal-FastFourierTransform',
                     'StandardDeviationBodyGyroscopeMeasurement-Magnitude-JerkSignal-FastFourierTransform')

#combine all data into dataframe
df <- data.frame(subject,activities,x_new)
colnames(df) <- final_col_names

############################################################
### Part 5: Create Tidy Data Set with averages #############
############################################################

df_tidy_pre <- apply(df[which((df$SubjectID == 1) & df$Activity == 'WALKING'),3:68],2,mean)
df_tidy <- data.frame(1,'WALKING',t(df_tidy_pre))
colnames(df_tidy) <- final_col_names
df_tidy_pre <- apply(df[which((df$SubjectID == 1) & df$Activity == 'WALKING_UPSTAIRS'),3:68],2,mean)
df_tidy_pre2 <- data.frame(1,'WALKING_UPSTAIRS',t(df_tidy_pre))
colnames(df_tidy_pre2) <- final_col_names
df_tidy <- rbind(df_tidy,df_tidy_pre2)
df_tidy_pre <- apply(df[which((df$SubjectID == 1) & df$Activity == 'WALKING_DOWNSTAIRS'),3:68],2,mean)
df_tidy_pre2 <- data.frame(1,'WALKING_DOWNSTAIRS',t(df_tidy_pre))
colnames(df_tidy_pre2) <- final_col_names
df_tidy <- rbind(df_tidy,df_tidy_pre2)
df_tidy_pre <- apply(df[which((df$SubjectID == 1) & df$Activity == 'SITTING'),3:68],2,mean)
df_tidy_pre2 <- data.frame(1,'SITTING',t(df_tidy_pre))
colnames(df_tidy_pre2) <- final_col_names
df_tidy <- rbind(df_tidy,df_tidy_pre2)
df_tidy_pre <- apply(df[which((df$SubjectID == 1) & df$Activity == 'STANDING'),3:68],2,mean)
df_tidy_pre2 <- data.frame(1,'STANDING',t(df_tidy_pre))
colnames(df_tidy_pre2) <- final_col_names
df_tidy <- rbind(df_tidy,df_tidy_pre2)
df_tidy_pre <- apply(df[which((df$SubjectID == 1) & df$Activity == 'LAYING'),3:68],2,mean)
df_tidy_pre2 <- data.frame(1,'LAYING',t(df_tidy_pre))
colnames(df_tidy_pre2) <- final_col_names
df_tidy <- rbind(df_tidy,df_tidy_pre2)

for (i in 2:30){
  df_tidy_pre <- apply(df[which((df$SubjectID == i) & df$Activity == 'WALKING'),3:68],2,mean)
  df_tidy_pre2 <- data.frame(i,'WALKING',t(df_tidy_pre))
  colnames(df_tidy_pre2) <- final_col_names
  df_tidy <- rbind(df_tidy,df_tidy_pre2)
  df_tidy_pre <- apply(df[which((df$SubjectID == i) & df$Activity == 'WALKING_UPSTAIRS'),3:68],2,mean)
  df_tidy_pre2 <- data.frame(i,'WALKING_UPSTAIRS',t(df_tidy_pre))
  colnames(df_tidy_pre2) <- final_col_names
  df_tidy <- rbind(df_tidy,df_tidy_pre2)
  df_tidy_pre <- apply(df[which((df$SubjectID == i) & df$Activity == 'WALKING_DOWNSTAIRS'),3:68],2,mean)
  df_tidy_pre2 <- data.frame(i,'WALKING_DOWNSTAIRS',t(df_tidy_pre))
  colnames(df_tidy_pre2) <- final_col_names
  df_tidy <- rbind(df_tidy,df_tidy_pre2)
  df_tidy_pre <- apply(df[which((df$SubjectID == i) & df$Activity == 'SITTING'),3:68],2,mean)
  df_tidy_pre2 <- data.frame(i,'SITTING',t(df_tidy_pre))
  colnames(df_tidy_pre2) <- final_col_names
  df_tidy <- rbind(df_tidy,df_tidy_pre2)
  df_tidy_pre <- apply(df[which((df$SubjectID == i) & df$Activity == 'STANDING'),3:68],2,mean)
  df_tidy_pre2 <- data.frame(i,'STANDING',t(df_tidy_pre))
  colnames(df_tidy_pre2) <- final_col_names
  df_tidy <- rbind(df_tidy,df_tidy_pre2)
  df_tidy_pre <- apply(df[which((df$SubjectID == i) & df$Activity == 'LAYING'),3:68],2,mean)
  df_tidy_pre2 <- data.frame(i,'LAYING',t(df_tidy_pre))
  colnames(df_tidy_pre2) <- final_col_names
  df_tidy <- rbind(df_tidy,df_tidy_pre2)
}

write.table(df_tidy,file="tidy_data_set.txt",row.names=FALSE)



