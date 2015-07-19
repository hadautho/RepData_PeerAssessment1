# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip", "activity.csv")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
library(plyr)
processed_data <- ddply(.data = data, "date", summarise, steps_total = sum(steps))
```

Histogram


```r
hist(processed_data$steps_total, breaks=30, main = "Total number of steps taken each day", xlab="Total steps taken per day")
```

![](PA1_template_files/figure-html/Histogram-1.png) 

Mean of the total number of steps taken per day


```r
mean(processed_data$steps_total, na.rm = TRUE)
```

```
## [1] 10766.19
```
Median of the total number of steps taken per day


```r
median(processed_data$steps_total, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
average_steps_interval<- ddply(.data = data, "interval", summarise, steps_avg = mean(steps, na.rm=TRUE))
```

```r
plot(average_steps_interval$interval, average_steps_interval$steps_avg, type = "l", main="Average daily activity pattern", xlab="Interval", ylab="Average Steps")
```

![](PA1_template_files/figure-html/Plot-1.png) 

Interval with the maximum number of steps taken per day

```r
average_steps_interval[average_steps_interval$steps_avg == max(average_steps_interval$steps_avg),1]
```

```
## [1] 835
```

## Imputing missing values


```r
data2 <- data
#Replace NA with the average of the interval
for(i in 1:nrow(data2)){
  if(is.na(data2$steps[i])) {
    data2$steps[i] <- mean(data2$steps[data2$interval ==  data2$interval[i]], na.rm = TRUE)
  }
}
#Summarize the data
processed_data2 <- ddply(.data = data2, "date", summarise, steps_total = sum(steps))
hist(processed_data2$steps_total, breaks=30, main = "Total number of steps taken each day (NAs replaced)", xlab="Total steps taken per day (NAs replaced)")
```

![](PA1_template_files/figure-html/processing the NA replacement-1.png) 

Mean of the total number of steps taken per day (replaced NA data)


```r
mean(processed_data2$steps_total)
```

```
## [1] 10766.19
```

Median of the total number of steps taken per day (replaced NA data)


```r
median(processed_data2$steps_total)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
#Creates the Field
data2[,"DayOfWeek"] <- ""
#Setting the column as weekday for dates from monday to friday
data2[as.POSIXlt(as.Date(data2$date))$wday %in% 1:5 , "DayOfWeek"] <- "weekday"
#Setting the column as weekend for satuday and sunday
data2[(as.POSIXlt(as.Date(data2$date))$wday == 0) | (as.POSIXlt(as.Date(data2$date))$wday == 6) , "DayOfWeek"] <- "weekend"
#Summarizing
processed_data3 <- ddply(.data = data2, c("interval","DayOfWeek"), summarise, steps_avg = mean(steps))
#separating data for the plots
data_weekend <- processed_data3[processed_data3$DayOfWeek == "weekend",]
data_weekday <- processed_data3[processed_data3$DayOfWeek == "weekday",]
```

Plots


```r
par(mfrow=c(2,1))
plot(data_weekend$interval, data_weekend$steps_avg, type = "l", main = "weekend", xlab="Interval", ylab="Average Steps")
plot(data_weekday$interval, data_weekday$steps_avg, type = "l", main = "weekday", xlab="Interval", ylab="Average Steps")
```

![](PA1_template_files/figure-html/plots-1.png) 
