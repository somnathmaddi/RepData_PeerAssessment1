# Reproducible Research: Peer Assessment 1
========================================================

## Loading and preprocessing the data


```r
setwd("d:\\RWork\\data\\activity")
activity <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day ?

### 1.Make a histogram of the total number of steps taken each day


```r
stepsPerDay <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(stepsPerDay$steps, breaks = nrow(stepsPerDay), main = "Total Number of Steps Each Day", 
    xlab = "Steps each Day", col = "Orange")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


### 2.Calculate and report the mean and median total number of steps taken per day

```r
meanStepsPerDay <- mean(stepsPerDay$steps)
medianStepsPerDay <- median(stepsPerDay$steps)
```


#### The mean of total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>
#### The median of total number of steps taken per day is 10765




## What is the average daily activity pattern ?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
perIntervalStepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)

plot(x = perIntervalStepsAverage$interval, y = perIntervalStepsAverage$steps, 
    type = "l", main = "Average Daily Time Interval Activity Pattern", xlab = "5-minute Intervals", 
    ylab = "Average Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps ?


```r

maxInterval <- perIntervalStepsAverage[which.max(perIntervalStepsAverage$steps), 
    "interval"]
```



#### Top 5-minute interval, on average across all the days in the dataset, Which  contains the maximum number of steps is 835



## Imputing missing values


### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missingValues <- sum(is.na(activity))
```


#### total number of missing values in the dataset is 2304


### Answer for Q2, Q3 and Q4 as follows


```r
# create a vector of steps with NAs replaced by imputed value

imputedSteps <- numeric()
for (i in 1:nrow(activity)) {
    obsrvs <- activity[i, ]
    if (is.na(obsrvs$steps)) {
        steps <- subset(perIntervalStepsAverage, interval == obsrvs$interval)$steps
    } else {
        steps <- obsrvs$steps
    }
    imputedSteps <- c(imputedSteps, steps)
}

## create a new dataset that is equal to the original dataset without missing
## data

imputedActivity <- activity
imputedActivity$steps <- imputedSteps

# total number of steps taken each day

imputedStepsPerDay <- aggregate(steps ~ date, data = imputedActivity, FUN = sum)

# make a histogram

hist(imputedStepsPerDay$steps, breaks = nrow(imputedStepsPerDay), main = "Total Number of Steps Per Day With Imputed Values", 
    xlab = "Steps Per Day", col = "red")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r

# mean and median total number of steps taken per day

imputedMeanSteps <- mean(imputedStepsPerDay$steps)
imputedmedianSteps <- median(imputedStepsPerDay$steps)
```



#### The mean of total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>
#### The mean of total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>


## Are there differences in activity patterns between weekdays and weekends ?



```r

imputedActivity$date <- as.Date(imputedActivity$date)

weekend <- c("Sunday", "Saturday")
imputedActivity$day_type <- as.factor(sapply(imputedActivity$date, function(x) ifelse(weekdays(x) %in% 
    weekend, "weekend", "weekday")))

require(plyr)
averageSteps <- ddply(imputedActivity, .(interval, day_type), summarize, steps = mean(steps))

require(lattice)
xyplot(steps ~ interval | day_type, data = averageSteps, layout = c(1, 2), type = "l", 
    xlab = "5-minute Intervals Over Day", ylab = "Number of Steps", main = "Activity Patterns on Weekends and Weekdays")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 








