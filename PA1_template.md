# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
# Check structure for date format
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# Convert date from factor to date format
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# Group by day
steps.day <- activity %>% group_by(date) %>% summarise(DailySteps = sum(steps))

hist(steps.day$DailySteps, 
             col = "blue", 
             xlab = "Steps", 
             ylab = "Frequency", 
             main = "Steps per Day")

# Explore data
steps.day.mean <- mean(steps.day$DailySteps, na.rm = TRUE)
steps.day.median <- median(steps.day$DailySteps, na.rm = TRUE)
abline(v=steps.day.mean, col = "red", lwd = 2, lty = 1 )
abline(v=steps.day.median, col = "green", lwd = 2, lty = 2 )
```

![](PA1_template_files/figure-html/Steps per day-1.png) 

*  The mean total number of steps per day is 10,766.2 steps, indicated as red line on the histogram.

*  The median total number of steps per day is 10,765 steps, indicated by the green line on the histogram.

## What is the average daily activity pattern?

```r
steps.interval <- activity %>% group_by(interval) %>% summarise(AvgSteps = mean(steps, na.rm = TRUE))
with(steps.interval, plot(x = interval, y= AvgSteps, type= "l", xlab= "Daily 5 minute intervals", ylab="Steps"))

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Interval.max.steps <- steps.interval$interval[which.max(steps.interval$AvgSteps)]
abline(v = Interval.max.steps,col = "red", lwd = 2, lty = 2 )
```

![](PA1_template_files/figure-html/Daily Pattern-1.png) 

The most steps was taken during interval 835, as indicated by the red dotted line in the trend.


## Imputing missing values

```r
# 1. Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with `NA`s)
sum(is.na(activity))
```

```
## [1] 2304
```

There are 2304 missing values, which is a small proportion (4.4%) of the data.

The missing data would be filled with the **median of 10765 steps.**

## Are there differences in activity patterns between weekdays and weekends?


