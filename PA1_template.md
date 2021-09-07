---
title: "PA1_template.Rmd"
author: "CLB"
date: "9/6/2021"
output: html_document
---
Set Working Directory 

```r
setwd("C:/Users/cback/OneDrive/Desktop/R Specialization/Reproducible Research")
getwd()
```

```
## [1] "C:/Users/cback/OneDrive/Desktop/R Specialization/Reproducible Research"
```
Load data,observe, preprocess data

```r
library(readr) 
activity <- read_csv("activity.csv")
```

```
## 
## -- Column specification -------------------------------------------------------------------------------------------------
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
head(activity)#read and view data
```

```
## # A tibble: 6 x 3
##   steps date       interval
##   <dbl> <date>        <dbl>
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
#preprocess data and load necessary packages
library(ggplot2)
library(dplyr)
library(chron)
```
What is mean total number of steps taken per day?  
Mean Steps per day is 10766.188679 and Median Steps per day is 10765

```r
aggsteps<- aggregate(steps ~ date, activity, FUN=sum)
head(aggsteps)#Calculate the total number of steps taken per day
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
hist(aggsteps$steps, 
     col="purple", 
     xlab = "Frequency", 
     ylab = "Steps",
     main = "Total number of steps taken each day") #histogram of 
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

```r
#totalsteps per day
activitymean <- mean(aggsteps$steps) #mean steps per day
activitymedian <- median(aggsteps$steps) #median steps per day
#Mean Steps per day is 10766.188679 and Median Steps per day is 10765
```
What is the average daily activity pattern? - Make a time series plot
Intervale 835 has 10927 steps

```r
agginterval <- aggregate(steps ~ interval, activity, FUN=sum) #aggregate intervals
plot(agginterval$interval, agginterval$steps, 
     type = "l", lwd = 2,
     xlab = "Interval", 
     ylab = "Total Steps",
     main = "Total Steps vs. 5-Minute Interval")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
filter(agginterval, steps==max(steps))#interval 835 at 10927 steps
```

```
##   interval steps
## 1      835 10927
```
Imputing missing values
2304 Nas in file

```r
table(is.na(activity)) # 2304 True
```

```
## 
## FALSE  TRUE 
## 50400  2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
#In the original data set aggregating (mean) steps over 5-minute interval
meaninterval<- aggregate(steps ~ interval, activity, FUN=mean)

#Merging the mean of total steps for a date with the original data set
activity2 <- merge(x=activity, y=meaninterval, by="interval")

#Replacing the NA values with the mean for that 5-minute interval
activity2$steps <- ifelse(is.na(activity2$steps.x), activity2$steps.y, activity2$steps.x)

#Merged dataset which will be subsetted in the next step by removing not required columns
head(activity2)
```

```
##   interval steps.x       date  steps.y    steps
## 1        0      NA 2012-10-01 1.716981 1.716981
## 2        0       0 2012-11-23 1.716981 0.000000
## 3        0       0 2012-10-28 1.716981 0.000000
## 4        0       0 2012-11-06 1.716981 0.000000
## 5        0       0 2012-11-24 1.716981 0.000000
## 6        0       0 2012-11-15 1.716981 0.000000
```
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
#Fetching only the required columns (steps, date, interval) and storing in the new data set.
activity2 <- select(activity2, steps, date, interval)

#New dataset with NA imputed by mean for that 5-minute interval
head(activity2)
```

```
##      steps       date interval
## 1 1.716981 2012-10-01        0
## 2 0.000000 2012-11-23        0
## 3 0.000000 2012-10-28        0
## 4 0.000000 2012-11-06        0
## 5 0.000000 2012-11-24        0
## 6 0.000000 2012-11-15        0
```
Make a histogram of the total number of steps taken each day.  

```r
#Aggregating(summation) of steps over date
aggsteps_new<- aggregate(steps ~ date, activity2, FUN=sum)

#Plotting
#Setting up the panel for one row and two columns
par(mfrow=c(1,2))

#Histogram after imputing NA values with mean of 5-min interval
hist(aggsteps_new$steps, 
     col="indianred",
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total number of steps taken each day (New Dataset)",
     cex.main = 0.7)

#Histogram with the original dataset
hist(aggsteps$steps, 
     col="midnightblue", 
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total number of steps taken each day (Orginal Dataset)",
     cex.main = 0.7)
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)
Calculate and report the mean and median total number of steps taken per day.
Mean is 10766.188679
Median is 10766.18879

```r
activitymean2 <- mean(aggsteps_new$steps)
aactvitymedian2 <- median(aggsteps_new$steps)
```
Do these values differ from the estimates from the first part of the assignment?
The mean is the same at 10766.188679 but the median differs from the original 
(10765) and the new (10766.188679). 

What is the impact of imputing missing data on the estimates of the total daily 
number of steps?
It does not change the mean, but the median estimates decrease by 1.19 when 
missing data is present and is higher when missing. 

Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and 
“weekend” indicating whether a given date is a weekday or weekend day.

```r
table(is.weekend(activity2$date)) #4608 weekends
```

```
## 
## FALSE  TRUE 
## 12960  4608
```

```r
#Adding new factor variable "dayofweek" indicating whether a given date is a weekday or weekend day
activity2$dayofweek <- ifelse(is.weekend(activity2$date), "weekend", "weekday")

#Number of Weekdays and Weekends
table(activity2$dayofweek)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
head(activity2)
```

```
##      steps       date interval dayofweek
## 1 1.716981 2012-10-01        0   weekday
## 2 0.000000 2012-11-23        0   weekday
## 3 0.000000 2012-10-28        0   weekend
## 4 0.000000 2012-11-06        0   weekday
## 5 0.000000 2012-11-24        0   weekend
## 6 0.000000 2012-11-15        0   weekday
```
Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
#Aggregating(mean) steps over interval and day of week
meaninterval_activity2<- aggregate(steps ~ interval + dayofweek, activity2, FUN=mean)

#Aggregated Data
head(meaninterval_activity2)
```

```
##   interval dayofweek      steps
## 1        0   weekday 2.25115304
## 2        5   weekday 0.44528302
## 3       10   weekday 0.17316562
## 4       15   weekday 0.19790356
## 5       20   weekday 0.09895178
## 6       25   weekday 1.59035639
```

```r
ggplot(meaninterval_activity2, aes(x=interval, y=steps)) + 
  geom_line(color="tan2", size=1) + 
  facet_wrap(~dayofweek, nrow=2) +
  labs(x="\nInterval", y="\nNumber of steps")
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)

