
run_analysis <-function (traindirectory = "C:/Users/Shelby/Documents/Github/UCIDataset/train/Inertial Signals", testdirectory ="C:/Users/Shelby/Documents/Github/UCIDataset/test/Inertial Signals") {

library(reshape2)
library(dplyr)
 
labels <- c("Walking", "WalkingUpstairs", "WalkingDownstairs", "Sitting", "Standing", "Laying")
measurementlab <- c("bodyaccelerationxdirection", "bodyaccelerationydirection","bodyaccelerationzdirection", "gyroxdirection", "gyroydirection","gyrozdirection", "totalaccelerationxdirection", "totalaccelerationydirection", "totalaccelerationzdirection" )
#finald <- function() {
setwd(traindirectory)
trainlist <- list.files(traindirectory)

subject_train <- read.table("~/UCIDataset/train/subject_train.txt", quote="\"")
names(subject_train) <- "subjectnumber"

subjectnumber<- as.vector(subject_train)

activitylabel <- read.table("~/UCIDataset/train/y_train.txt", quote="\"")

activitylabel <- unlist(activitylabel)
activity <- rep("a", length(activitylabel))
observationnumber <- 1:length(activitylabel)
testortraining <- rep("training", length(activitylabel))

for (j in 1:length(activitylabel))
{    activity[j] <- labels[activitylabel[j]]}


listd <- list(NULL)
for ( i in 1:length(trainlist))
{               
        currentdata <-read.table(trainlist[i], quote="\"")
        currentdata <- data.matrix(currentdata)
        measurement <- rep(measurementlab[[i]], length(currentdata[,1]))
        currentdata <- cbind(observationnumber, subjectnumber, testortraining, activity, measurement, currentdata)
        currentdata <- as.data.frame(currentdata)
        listd[[i]] <- currentdata }
#test <- as.array(listd)

training <- rbind_all(listd)

setwd(testdirectory)
trainlist <- list.files(testdirectory)

subject_train <- read.table("~/UCIDataset/test/subject_test.txt", quote="\"")
names(subject_train) <- "subjectnumber"
labels <- c("Walking", "WalkingUpstairs", "WalkingDownstairs", "Sitting", "Standing", "Laying")
subjectnumber<- as.vector(subject_train)

activitylabel <- read.table("~/UCIDataset/test/y_test.txt", quote="\"")
activitylabel <- unlist(activitylabel)
activity <- rep("a", length(activitylabel))
observationnumber <- seq(from = 7352, length.out= length(activitylabel), by=1)
testortraining <- rep("test", length(activitylabel))

for (j in 1:length(activitylabel))
{    activity[j] <- labels[activitylabel[j]]}


listd <- list(NULL)
for ( i in 1:length(trainlist))
{               
        currentdata <-read.table(trainlist[i], quote="\"")
        currentdata <- data.matrix(currentdata)
        measurement <- rep(measurementlab[[i]], length(currentdata[,1]))
        currentdata <- cbind(observationnumber, subjectnumber, testortraining, activity, measurement, currentdata)
        currentdata <- as.data.frame(currentdata)
        listd[[i]] <- currentdata }

testing <- rbind_all(listd)
alldata <- rbind( training, testing)
alldata <- as.data.frame(alldata)
#idvar <- names(alldata[1:5])
#measvar <-names(alldata[6:133])
vformean <- as.matrix(select(alldata, V1:V128))
observationmean <- vector( mode ="numeric", length = 92691  )
observationstandarddeviation <- vector( mode ="numeric", length =92691)          
for ( k in 1:92691)
{ observationmean[k] <-mean(vformean[k, 1:128])
  observationstandarddeviation[k] <- sd(vformean[k , 1:128]) }
finaldata1 <- cbind(select(alldata, -(V1:V128)),observationmean, observationstandarddeviation, select(alldata,V1:V128))
finaldata <- as.data.frame(finaldata1) 

#}

#finaldata <- finald()

subject <- vector ( mode ="character", length = 54  )
measurement <- vector( mode ="character", length = 54  )
activity <- vector( mode ="character", length = 54  )
mean <- vector( mode ="numeric", length = 54  )
for (n in 1:6)
{        activ <- labels[n]
         avthis1 <- filter( finaldata, activity == activ ) 
         for (m in 1:9)
         { meas <- measurementlab[m]
           avthis2 <- filter(avthis1, measurement == meas)
           avthis3 <- select(avthis2, V1:V128 )
           mean[n*9 + m - 9] <-mean(as.matrix(avthis3))
           measurement[n*9 + m - 9] <- meas
           activity[n*9 + m - 9] <- activ 
           subject [n*9 + m - 9] <- "averagedoversubject" 
         } }
activitymeanwithid <-as.data.frame(cbind(mean, measurement, activity, subject ))

activity <- vector( mode ="character", length = 270  )
measurement <- vector( mode ="character", length = 270  )
subject <- vector( mode ="character", length = 270  )
mean <- vector( mode ="numeric", length = 270  )
for ( n in 1:30)
{avthis1 <- filter( finaldata, subjectnumber == n)
 for (m in 1:9)
 {       meas <- measurementlab[m]
         avthis2 <- filter(avthis1, measurement == meas)
         avthis3 <- select(avthis2, V1:V128 )
         mean[n*9 + m - 9] <-mean(as.matrix(avthis3))
         measurement[n*9 + m - 9] <- measurementlab[m]
         subject[n*9 + m - 9] <- as.character(n)  }
 activity [n*9 + m - 9] <- "averagedoveractivity"
} 
subjectmeanwithid <-as.data.frame( cbind( mean, measurement, activity, subject))

subject <- vector( mode ="character", length = 1620  )
measurement <- vector( mode ="character", length = 1620  )
activity <- vector( mode ="character", length = 1620  )
mean <- vector( mode ="numeric", length = 1620)
for ( n in 1:30)
{avthis1 <- filter( finaldata, subjectnumber == n)
 for (m in 1:6)
 { avthis2 <- filter( avthis1, activity == activity[m])
   activ <- labels[m]
   for( k in 1:9)
   { meas <- measurementlab[k] 
     avthis3 <- filter(avthis2, measurement == meas)
     avthis4 <- select(avthis3, V1:V128 )
     mean[n*6*9 + m*9 - 6*9 + k -9] <- mean(as.matrix(avthis4))
     subject[n*6*9 + m*9 - 6*9 + k -9] <- n
     measurement[n*6*9 + m*9 - 6*9 + k -9] <- measurementlab[k] 
     
     activity[n*6*9 + m*9 - 6*9 + k -9] <-  activ}}
}

activitymeanofsubjectwithid <- as.data.frame( cbind( mean, measurement, activity, subject)) 
allav <- rbind(activitymeanofsubjectwithid, subjectmeanwithid, activitymeanwithid )
#meltav <- melt(allav, id =c("subject", "measurement", "activity"),  measured=c("activitymeanofsubject","subjectmean","activitymean"))
meltav <- melt(activitymeanofsubjectwithid, id =c("subject", "measurement", "activity"),  measured=c("mean"))
cast1 <- dcast(meltav, subject + activity ~ mean)
cast2 <- dcast(meltav, subject + activity ~ mean + measurement)
write.table(activitymeanofsubjectwithid, file ="~/tidydatafromstep5.txt", row.names =FALSE)

}