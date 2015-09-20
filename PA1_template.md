---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Read data from file "activity.zip" and store it in dataframe named "activity":

```r
activity <- read.csv(unz("activity.zip", "activity.csv"))
```
Change class for the date variable

```r
activity$date <- as.Date(activity$date)
```
Lets look at the head of the data:

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?
*1. Make a histogram of the total number of steps taken each day*

```r
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

*2. Calculate and report the mean and median total number of steps taken per day*

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
*1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

```r
steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(steps.interval, type = "l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

*2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*

```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values
*1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*

```r
sum(is.na(activity))
```

```
## [1] 2304
```

*2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

Using the means for the 5-minute intervals as fillers for missing values.

*3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*

```r
activity <- merge(activity, steps.interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
```

*4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

```r
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

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
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
*1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*

```r
weekdaysAsNum <- as.POSIXlt(activity$date)$wday
activity$datetype <- factor(ifelse(weekdaysAsNum < 6, "weekday", "weekend"))
```

*2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*

```r
library(lattice)
activityByInterval <- group_by(activity, interval, datetype) %>% summarise(stepsmean = mean(steps))
xyplot(stepsmean ~ interval | datetype, data = activityByInterval, layout = c(1,2), type = "l")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
