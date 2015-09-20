# Reproducible Research: Peer Assessment 1

library("ggplot2", lib.loc="~/R/win-library/3.2")


## Loading and preprocessing the data
activity <- read.csv("~/RepData_PeerAssessment1/activity/activity.csv", stringsAsFactors=FALSE)

TidyActivity<-activity
TidyActivity$date<-as.Date(TidyActivity$date)


## What is mean total number of steps taken per day?
stepsDay<-aggregate(TidyActivity$steps, by = list(date = TidyActivity$date), FUN = "sum", na.rm = TRUE)
hist(stepsDay$x)
mean(stepsDay$x)
median(stepsDay$x)

## What is the average daily activity pattern?
stepsInterval  <- aggregate(x = TidyActivity$steps , by = list(interval = TidyActivity$interval), FUN = mean ,na.rm=TRUE)
plot(stepsInterval$interval, stepsInterval$x, type = "l", main="Daily Activity Pattern", xlab="Interval", ylab="Steps")
stepsInterval[which.max(stepsInterval$x),"interval"]
nrow(TidyActivity[is.na(TidyActivity$steps), ])


## Imputing missing values
ImputedActivity<-merge(TidyActivity, stepsInterval, by="interval")
ImputedActivity[is.na(ImputedActivity$steps), "steps"] <- ImputedActivity[is.na(ImputedActivity$steps),"x"]
stepsDayImputed<- aggregate(ImputedActivity$steps , by = list(ImputedActivity$date), FUN = sum)
hist(stepsDayImputed$x)
mean(stepsDayImputed$x)
median(stepsDayImputed$x)


## Are there differences in activity patterns between weekdays and weekends?
ImputedActivity$weekday <- as.factor(ifelse(weekdays(ImputedActivity$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday"))
WeekDayVsEnd <- aggregate(ImputedActivity$steps, by = list(ImputedActivity$interval,ImputedActivity$weekday), FUN = mean)
names(WeekDayVsEnd) <- c("interval", "Weekday", "Steps")
WeekdayPlot <- ggplot(WeekDayVsEnd,aes(interval,Steps)) +
  ggtitle("Weekday Average Steps (NA's replaced with step mean)") +
  facet_grid(. ~ Weekday) +
  geom_line(size = 1)
WeekdayPlot
