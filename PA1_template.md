# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```






1.2 Process/transform the data (if necessary) into a format suitable for your analysis

a. Let's get dates instead of character strings

```r
activity$date <- as.Date(activity$date)
```


b. Intervals are stored as a number in the form of hhmm where hh=hours and mm= minutes. We create 2 variables: time since minight, in minutes, and a string factor instead of the numeric concatenation of hours and minutes.

```r
activity$minute <- activity$interval %% 100
activity$hour <- activity$interval %/% 100
activity$elapsed <- activity$hour * 60 + activity$minute
# interval as a factor
activity$sInterval <- as.factor(sprintf("%02d:%02d", activity$hour, activity$minute))
```

We also replace the interval ID by hundredths in order to have a better visualisation (hours). 

```r
activity$interval <- activity$interval / 100
```



```r
sumStepsPerDay <- aggregate(steps ~ date, data=activity, FUN="sum", na.exclude=T)
meanStepsPerInterval <- aggregate(steps ~ sInterval, data=activity, FUN="mean", na.exclude=T)
```




## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
  steps.date <- aggregate(steps ~ date, data=activity, FUN=sum)
  barplot(steps.date$steps, names.arg=steps.date$date, xlab="date", ylab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 





  

2. Calculate and report the mean and median total number of steps taken per day


```r
  mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
  median(steps.date$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps.interval <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(steps.interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 8.35
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


I will use the means for the 5-minute intervals as fillers for missing values.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# replace missig values w
datasetNoMissing <- activity
for(r in 1:nrow(datasetNoMissing)){
  if (is.na(datasetNoMissing$steps[r])) {
    repl <- meanStepsPerInterval$steps[meanStepsPerInterval$sInterval == datasetNoMissing$sInterval[r]];
    datasetNoMissing$steps[r] <- repl;
  }
}
# we verify it worked
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
sum(is.na(datasetNoMissing$steps))
```

```
## [1] 0
```

```r
str(activity$steps)
```

```
##  int [1:17568] NA NA NA NA NA NA NA NA NA NA ...
```

```r
str(datasetNoMissing$steps)
```

```
##  num [1:17568] 1.717 0.3396 0.1321 0.1509 0.0755 ...
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps.date <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(steps.date$steps, names.arg=steps.date$date, xlab="date", ylab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
```

```
## [1] 10765
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
datasetNoMissing$day <- "weekday"
datasetNoMissing$day[weekdays(as.Date(datasetNoMissing$date), abb=T) %in% c("Sat","Sun")] <- "weekend"
```
datasetNoMissing$day contains "weekday" or "weekend".

```r
table(datasetNoMissing$day)
```

```
## 
## weekday 
##   17568
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
library(lattice)
meanStepsPerIntervalNoMissingDay <- aggregate(steps ~ interval + day, data=datasetNoMissing, FUN="mean")
xyplot(steps ~ interval | day, data=meanStepsPerIntervalNoMissingDay, type="l", grid=T, layout=c(1,2), ylab="Number of steps", xlab="5-min. intervals from midnight", main="Average  5-min. activity intervals: Weekdays vs. Weekends")
```

![](PA1_template_files/figure-html/timeSeriesPlotMissingsEstimatedWeekDayWeekEnd-1.png) 






