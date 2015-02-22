#Code Book for 'Human Activity Recognition Using Smartphones Dataset' Tidy Data Set

##Data used from Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
##Smartlab - Non Linear Complex Systems Laboratory
##DITEN - Universit‡ degli Studi di Genova.
##Via Opera Pia 11A, I-16145, Genoa, Italy.
##activityrecognition@smartlab.ws
##www.smartlab.ws

###This data set contains 180 observations on 66 variables. Each variable (listed below with units) is actually the
###mean of the observations over each participant and activity. There were 30 different participants, and six different
###activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING), which each participant
###performed while wearing a smartphone.

###The 66 variables and their units are as follows:

####MeanBodyAccelerometerMeasurement-XDirection  no units
####MeanBodyAccelerometerMeasurement-YDirection  no units
####MeanBodyAccelerometerMeasurement-ZDirection  no units
####StandardDeviationBodyAccelerometerMeasurement-XDirection  no units
####StandardDeviationBodyAccelerometerMeasurement-YDirection  no units
####StandardDeviationBodyAccelerometerMeasurement-ZDirection  no units
####MeanGravityAccelerometerMeasurement-XDirection  standard gravity units 'g'
####MeanGravityAccelerometerMeasurement-YDirection  standard gravity units 'g'
####MeanGravityAccelerometerMeasurement-ZDirection  standard gravity units 'g'
####StandardDeviationGravityAccelerometerMeasurement-XDirection  standard gravity units 'g'
####StandardDeviationGravityAccelerometerMeasurement-YDirection  standard gravity units 'g'
####StandardDeviationGravityAccelerometerMeasurement-ZDirection  standard gravity units 'g'
####MeanBodyAccelerometerMeasurement-XDirection-JerkSignal  no units
####MeanBodyAccelerometerMeasurement-YDirection-JerkSignal  no units
####MeanBodyAccelerometerMeasurement-ZDirection-JerkSignal  no units
####StandardDeviationBodyAccelerometerMeasurement-XDirection-JerkSignal  no units
####StandardDeviationBodyAccelerometerMeasurement-YDirection-JerkSignal  no units
####StandardDeviationBodyAccelerometerMeasurement-ZDirection-JerkSignal  no units
####MeanBodyGyroscopeMeasurement-XDirection  radians per second
####MeanBodyGyroscopeMeasurement-YDirection  radians per second
####MeanBodyGyroscopeMeasurement-ZDirection  radians per second
####StandardDeviationBodyGyroscopeMeasurement-XDirection  radians per second
####StandardDeviationBodyGyroscopeMeasurement-YDirection  radians per second
####StandardDeviationBodyGyroscopeMeasurement-ZDirection  radians per second
####MeanBodyGyroscopeMeasurement-XDirection-JerkSignal  radians per second
####MeanBodyGyroscopeMeasurement-YDirection-JerkSignal  radians per second
####MeanBodyGyroscopeMeasurement-ZDirection-JerkSignal  radians per second
####StandardDeviationBodyGyroscopeMeasurement-XDirection-JerkSignal  radians per second
####StandardDeviationBodyGyroscopeMeasurement-YDirection-JerkSignal  radians per second
####StandardDeviationBodyGyroscopeMeasurement-ZDirection-JerkSignal  radians per second
####MeanBodyAccelerometerMeasurement-Magnitude  no units
####StandardDeviationBodyAccelerometerMeasurement-Magnitude  no units
####MeanGravityAccelerometerMeasurement-Magnitude  standard gravity units 'g'
####StandardDeviationGravityAccelerometerMeasurement-Magnitude  standard gravity units 'g'
####MeanBodyAccelerometerMeasurement-Magnitude-JerkSignal  no units
####StandardDeviationBodyAccelerometerMeasurement-Magnitude-JerkSignal  no units
####MeanBodyGyroscopeMeasurement-Magnitude  radians per second
####StandardDeviationBodyGyroscopeMeasurement-Magnitude
####MeanBodyGyroscopeMeasurement-Magnitude-JerkSignal  radians per second
####StandardDeviationBodyGyroscopeMeasurement-Magnitude-JerkSignal  radians per second
####MeanBodyAccelerometerMeasurement-XDirection-FastFourierTransform  no units
####MeanBodyAccelerometerMeasurement-YDirection-FastFourierTransform  no units
####MeanBodyAccelerometerMeasurement-ZDirection-FastFourierTransform  no units
####StandardDeviationBodyAccelerometerMeasurement-XDirection-FastFourierTransform  no units
####StandardDeviationBodyAccelerometerMeasurement-YDirection-FastFourierTransform  no units
####StandardDeviationBodyAccelerometerMeasurement-ZDirection-FastFourierTransform  no units
####MeanBodyAccelerometerMeasurement-XDirection-JerkSignal-FastFourierTransform  no units
####MeanBodyAccelerometerMeasurement-YDirection-JerkSignal-FastFourierTransform  no units
####MeanBodyAccelerometerMeasurement-ZDirection-JerkSignal-FastFourierTransform  no units
####StandardDeviationBodyAccelerometerMeasurement-XDirection-JerkSignal-FastFourierTransform  no units
####StandardDeviationBodyAccelerometerMeasurement-YDirection-JerkSignal-FastFourierTransform  no units
####StandardDeviationBodyAccelerometerMeasurement-ZDirection-JerkSignal-FastFourierTransform  no units
####MeanBodyGyroscopeMeasurement-XDirection-FastFourierTransform  radians per second
####MeanBodyGyroscopeMeasurement-YDirection-FastFourierTransform  radians per second
####MeanBodyGyroscopeMeasurement-ZDirection-FastFourierTransform  radians per second
####StandardDeviationBodyGyroscopeMeasurement-XDirection-FastFourierTransform  radians per second
####StandardDeviationBodyGyroscopeMeasurement-YDirection-FastFourierTransform  radians per second
####StandardDeviationBodyGyroscopeMeasurement-ZDirection-FastFourierTransform  radians per second
####MeanBodyAccelerometerMeasurement-Magnitude-FastFourierTransform  no units
####StandardDeviationBodyAccelerometerMeasurement-Magnitude-FastFourierTransform  no units
####MeanBodyAccelerometerMeasurement-Magnitude-JerkSignal-FastFourierTransform  no units
####StandardDeviationBodyAccelerometerMeasurement-Magnitude-JerkSignal-FastFourierTransform  no units
####MeanBodyGyroscopeMeasurement-Magnitude-FastFourierTransform  radians per second
####StandardDeviationBodyGyroscopeMeasurement-Magnitude-FastFourierTransform  radians per second
####MeanBodyGyroscopeMeasurement-Magnitude-JerkSignal-FastFourierTransform  radians per second
####StandardDeviationBodyGyroscopeMeasurement-Magnitude-JerkSignal-FastFourierTransform  radians per second


Reference: Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012