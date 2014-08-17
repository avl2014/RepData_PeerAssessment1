Peer Assessment 1 for Reproducible Research
========================================================

In this assemsemnt I will create my first Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **MD** toolbar button for help on Markdown).

Data description:
 Dataset: Activity monitoring data [52K]
 File name: activity.csv

The variables included in this dataset are:
 * steps: Number of steps taking in a 5-minute interval         (missing values are coded as NA)
 * date: The date on which the measurement was taken in YYYY-MM-DD format
 * interval: Identifier for the 5-minute interval in which measurement was taken
 
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Read the data and check that dataset contains specified columns/rows:


```r
data<-read.table("activity.csv",sep=",",h=T)
dim(data)
```

```
## [1] 17568     3
```

```r
head(data)
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

What is mean total number of steps taken per day?
For this questions we can ignore missing values in the dataset

```r
datafull<-data[complete.cases(data),]
```

1. Make a histogram of the total number of steps taken each day


```r
hist(datafull$steps, main="Number of steps taken each day",xlab="Intervals")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
mean(datafull$steps)
```

```
## [1] 37.38
```

```r
median(datafull$steps)
```

```
## [1] 0
```

What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
res<-aggregate(datafull$steps, by=list(datafull$interval), FUN=mean)
colnames(res)<-c("interval","avgSteps")
plot(res, main="Number of steps", type="l",xlab="Interval",ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
res[which(res$avgSteps==max(res$avgSteps)),]$interval
```

```
## [1] 835
```


Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(data[is.na(data),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use 5-minute interval data, obtained in the previous steps (variable 'res')

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
subst<-function(x, aggregMatrix){
  intervalValue<-as.numeric(x[3])
  substValue<-aggregMatrix[which(aggregMatrix[,1]==intervalValue),2]
  if(is.na(x[1]))
  {x[1]<-as.numeric(substValue)}
  x
}
dataNoMissing<-as.data.frame(t(apply(data,1,subst,aggregMatrix=res )))
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Below is histogram for the new data set with imputed values. Histogram did not changed.


```r
dataNoMissing$steps<-as.numeric(levels(dataNoMissing$steps)[dataNoMissing$steps])

hist(dataNoMissing$steps, main="Number of steps taken each day",xlab="Intervals")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
 Both median and mean values remains the same; there is no impact of imputting missing values by average of the intervals. 


```r
mean(dataNoMissing$steps)
```

```
## [1] 37.38
```

```r
median(dataNoMissing$steps)
```

```
## [1] 0
```



Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels ?weekday" and "weekend" indicating whether a given date is a weekday or weeken
d day.


```r
data1<-dataNoMissing
data1$week<-sapply(data1$date, function(x) weekdays(as.POSIXct(x)))
weekendList<-c("Saturday","Sunday")
weekdayList<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
data1[which(data1$week %in% weekendList),]$week<-'weekend'
data1[which(data1$week %in% weekdayList),]$week<-'weekday'
```


2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
dataweekend<-subset(data1, data1$week=='weekend')
dataweekday<-subset(data1, data1$week=='weekday')

resweekend<-aggregate(dataweekend$steps, by=list(dataweekend$interval), FUN=mean)
colnames(resweekend)<-c("interval","avgSteps")
resweekend$interval<-as.numeric(levels(resweekend$interval)[resweekend$interval])

resweekday<-aggregate(dataweekday$steps, by=list(dataweekday$interval), FUN=mean)
colnames(resweekday)<-c("interval","avgSteps")
resweekday$interval<-as.numeric(levels(resweekday$interval)[resweekday$interval])
```


```r
par(mai=c(0.5,0.6,0.4,0.3),mfrow=c(2,1))

plot(resweekend, main="Weekend", type="l",xlab="Interval")
plot(resweekday, main="Weekday", type="l",xlab="Interval")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
