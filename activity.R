#Loading and preprocessing the data
##reading the data and preprocess the data
AC<-read.csv("activity.csv",sep = ",")
AC$date<-as.Date(AC$date,"%Y-%m-%d")
#What is mean total number of steps taken per day
##Calculate the total number of steps taken per day
sumSteps<-as.data.frame(tapply(AC$steps, AC$date, sum,na.rm=TRUE))
sumSteps$date<-rownames(sumSteps)
colnames(sumSteps)<-c("steps","date")
rownames(sumSteps)<-1:61
sumSteps$date<-as.Date(sumSteps$date,"%Y-%m-%d")
meanSteps<-mean(sumSteps$steps)
##Make a histogram of the total number of steps taken each day
hist(sumSteps$steps,col="red",xlab = "steps per day",main = "Histogram of sum steps",breaks=30)
##Calculate and report the mean and median of the total number of steps taken per day
summary(sumSteps)
#What is the average daily activity pattern?
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
sumSteps2<-as.data.frame(tapply(AC$steps, AC$interval, mean,na.rm=TRUE))
sumSteps2$interval<-rownames(sumSteps2)
colnames(sumSteps2)<-c("steps","interval")
rownames(sumSteps2)<-1:288
with(sumSteps2,plot(interval,steps,type="l",main="Time series plot"))
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
sumSteps2[which.max(sumSteps2$steps),]$interval
#Imputing missing values
##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(AC$steps))
##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##Create a new dataset that is equal to the original dataset but with the missing data filled in.
newAC<-AC
for(i in 1:length(newAC$steps)){
        if(is.na(newAC[i,1])){
                newAC[i,1]=sumSteps2[sumSteps2$interval==newAC[i,3],1]
        }
}
##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
SumnewAC<-tapply(newAC$steps,newAC$date,sum,na.rm=TRUE)
mean(SumnewAC)
median(SumnewAC)
#Are there differences in activity patterns between weekdays and weekends?
##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
factor<-function(x){
        if(weekdays(x)<"Saturday"){
                return("weekday")
        }else{
                return("weekend")
        }
}
wfactor<-lapply(newAC$date,factor)
newAC$daytype<-unlist(wfactor)
##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
meansteps<-aggregate(steps~interval+daytype,newAC,mean)
library(lattice)
xyplot(steps ~ interval | daytype, data=meansteps, type = "l", layout = c(1, 2), xlab="Interval",ylab="Number of steps")
