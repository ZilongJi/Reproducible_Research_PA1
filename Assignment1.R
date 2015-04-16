####LOADING AND PREPROCESSING THE DATA
## Load the data(i.e. read.csv())
activity <- read.csv("activity.csv",header=TRUE,sep=",");

##first change the data frame to a data table
library(data.table)
activity <- data.table(activity)

################################################################################

####WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY
##Calculate the total number of steps taken per day.
sum_steps <- activity[,sum(steps,na.rm=T),by=date]

##make a histogram using ggplot
library(ggplot2)
p <- ggplot(sum_steps,aes(date,V1))+geom_histogram(stat="identity")+ylab("total sum of steps taken per day")
print(p)

##Calculate the mean and the median of the total number of steps taken per day
mean_steps <- activity[,mean(steps,na.rm=T),by=date];
colnames(mean_steps) <- c("date","mean")
median_steps <- activity[,median(as.double(steps,na.rm=T)),by=date];
colnames(median_steps) <- c("date","median")
mean_median_steps <- merge(mean_steps,median_steps,by="date")

################################################################################

##WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?
ave_steps <- activity[,mean(steps,na.rm=T),by=interval]
plot(ave_steps,type="l",xlab="5-minute interval",ylab="average number of steps taken")
##find 5-minute interval which contains the maximum number of steps
ind <- which.max(ave_steps$V1)
max_interval <- ave_steps$interval[ind]

################################################################################

####IMPUTING MISSING VALUES
count <- sum(is.na(activity$steps))
##using for loop to filling in all of the missing values in the dataset and create a new dataset
new_activity <- activity;
for(i in 1:nrow(new_activity)){
        if(is.na(activity$steps)[i]==T){
                Ind2 <- which(ave_steps$interval==new_activity$interval[i]);
                new_activity$steps[i] <- ave_steps$V1[Ind2]
        }
}
##make a histogram of steps taken each day
sum_steps2 <- new_activity[,sum(steps),by=date]
p <- ggplot(sum_steps2,aes(date,V1))+geom_histogram(stat="identity")+ylab("total sum of steps taken per day")
print(p)
##report the mean and median total number of steps taken per day
mean_steps2 <- new_activity[,mean(steps),by=date]
median_step2 <- new_activity[,median(as.double(steps)),by=date]
##as we can see, the values are differ from what we have seen in part one
##impact is: there is no NA any longer, which all be replaced by the average fo 5-interval minute

#################################################################################

####ARE THERE ANY DIFFERNECES IN ACTIVITY PATTERNS BETEWEEN WEEKDAYS AND WEEKENDS
library(lubridate)
new_activity$weekdays <- wday(as.Date(new_activity$date))
Indweekday <- which(new_activity$weekdays!=6 & new_activity$weekdays!=7)
new_act_weekday <- new_activity[Indweekday,]
Indweekend <- which(new_activity$weekdays==6 | new_activity$weekdays==7)
new_act_weekend <- new_activity[Indweekend,]

ave_steps_weekday <- new_act_weekday[,mean(steps),by=interval]
plot(ave_steps_weekday,type="l",xlab="5-minute interval",ylab="average number of steps taken in weekday",main="weekday")

ave_steps_weekend <- new_act_weekend[,mean(steps),by=interval]
plot(ave_steps_weekend,type="l",xlab="5-minute interval",ylab="average number of steps taken in weekend",main="weekend")
