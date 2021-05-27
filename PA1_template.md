#Reproducible Research : Course Prokect 1
by Suvrorup Mukherjee
27th May, 2021

##Introduction
This is my submission for the Course Project 1 for the May 2021 version of the MOOC ["Reproducible Research" - John Hopkins Univeristy]

##Required R packages
The following R packages need to be loaded for the course work. 

```r
require(data.table)
require(ggplot2)
require(plyr)
```

##Loading and Preprocessing the data

###1. Load the data

```r
activity <- read.csv("activity.csv")
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

###2. Preprocess the data 
The dates are stored as characters. We can verify this by :

```r
class(activity$date)
```

```
## [1] "character"
```

Hence we convert it into Date type.

```r
activity$date <- as.Date(activity$date)
```

##What is mean total number of steps taken per day?

###Creating new table for total steps taken every day
We can see from the table the total steps taken everyday

```r
dailySteps_data <- ddply(activity, .(date), summarise, steps = sum(steps, na.rm = TRUE))
head(dailySteps_data)
```

```
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

###Histogram for the steps taken each day 

```r
hist(dailySteps_data$steps, xlab = "Total steps taken each day", main = "Histogram of total steps")
```

![plot of chunk dailySteps_plot](figure/dailySteps_plot-1.png)

##Mean and median of total steps taken per day

```r
mean(dailySteps_data$steps)
```

```
## [1] 9354.23
```


```r
median(dailySteps_data$steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

###Create a new data table for mean steps taken at each time of the day 


```r
intervalSteps_data <- ddply(data, .(interval), summarise, steps = mean(steps, na.rm = TRUE))
head(intervalSteps_data)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

###Time series plot of average steps per day against intervals


```r
plot(intervalSteps_data$interval, intervalSteps_data$steps, type = "l")
```

![plot of chunk average_plot](figure/average_plot-1.png)

### Which interval contains the maximum number of steps?


```r
intervalSteps_data[which.max(intervalSteps_data$steps), "interval"]
```

```
## [1] 835
```


## Imputing missing values 

##The number of rows with missing values in the dataset

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

### Devise a strategy to fill in all the missing values
I'll fill the missing values with the rounded mean of the 5-minute interval.

### New dataset with missing values filled in

```r
mv <- activity
for(i in seq(dim(mv)[1])){
  if(is.na(mv[i,"steps"])){
    mv[i, "steps"] = intervalSteps_data[which(intervalSteps_data$interval == mv[i, "interval"]), "steps"]
  }
}
head(mv)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

###Histogram of total number of steps taken each day

```r
replacedvalue_steps <- ddply(mv, .(date), summarise, steps = sum(steps, na.rm = TRUE))
hist(replacedvalue_steps$steps, xlab = "Total steps taken each day", main = "Histogram of total steps in the new set")
```

![plot of chunk hist_natotal](figure/hist_natotal-1.png)

### New Mean and Median 
We notice that the mean has increased, while the median is almost the same.
The mean and median are equal if this strategy is followed.

```r
mean(replacedvalue_steps$steps)
```

```
## [1] 10766.19
```


```r
median(replacedvalue_steps$steps)
```

```
## [1] 10766.19
```

## Are there differences between weekdays and weekends?

### Create new factor variable in the datasteps 

```r
mv$wday <- ifelse(wday(mv$date)<6, "weekday", "weekend")
mv$wday <- as.factor(mv$wday)
head(mv)
```

```
##       steps       date interval    wday
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

### Multipanel plot for the mean steps taken for each interval

```r
wday_intervalSteps <- ddply(mv, .(interval, wday), summarise, steps = mean(steps, na.rm = TRUE))
ggplot(wday_intervalSteps, aes(x = interval, y = steps)) + geom_line() + xlab("time") + ggtitle("Number of Steps over an Average Day") + facet_grid(wday~.)
```

![plot of chunk wday_plot](figure/wday_plot-1.png)
