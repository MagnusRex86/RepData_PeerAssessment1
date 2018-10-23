setwd("D:/Programs/R/WD/Assignments/Reproducible Research/Week 2/RepData_PeerAssessment1")
data<-read.csv("activity.csv")
data$day<-weekdays(as.date(activity$date))
activity$DateTime <- as.POSIXct(activity$date, format = "%Y-%m-%d")
##pulling out data without NAs

clean<-activity[!is.na(activity$steps),]

#summarising total steps per date
sumTable<-aggregate(activity$step ~ activity$date,FUN=sum)
colnames(sumTable)<-c("Date","Steps")

##Creating a histogram of total steps per day
hist(sumTable$Steps, breaks=5,xlab="Steps", main = "Total Steps per Day")
##Mean of steps
StepsMean<-mean(sumTable$Steps)
print(StepsMean)
##Median of steps
StepsMedian<-median(sumTable$Steps)
print(StepsMedian)


library(plyr)
library(ggplot2)
intervalSteps<-ddply(clean, .(interval), summarize, Avg=mean(steps))
plot<-ggplot(intervalSteps,aes(x=interval,y=Avg),xlab="Interval",ylab="Average Number of Steps")
p+geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average number of Steps per Interval")

## Max steps by interval
maxSteps<-max(intervalSteps$Avg)
## Interval with max avg number of steps
intervalSteps[intervalSteps$avg==maxSteps,1]