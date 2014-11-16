---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing
the data
Load data and set the date to actually POSIXlt date objects

```r
dat <- read.csv('activity.csv')
dat$date <- as.POSIXlt(as.character(dat$date),format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?



```r
steps <- tapply(dat$steps,as.character(dat$date),FUN=sum, na.rm=TRUE)

hist(steps,breaks = 20, main='Total number of steps per day',na.rm=TRUE)
```

```
## Warning in title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...):
## "na.rm" is not a graphical parameter
```

```
## Warning in axis(1, ...): "na.rm" is not a graphical parameter
```

```
## Warning in axis(2, ...): "na.rm" is not a graphical parameter
```

![plot of chunk mean_median](figure/mean_median-1.png) 

```r
origDailyMean <-mean(steps, na.rm = TRUE)
origDailyMedian <- median(steps, na.rm = TRUE)
```

## What is the average daily activity pattern?

```r
stepsPerInterval <- tapply(dat$steps, dat$interval, FUN=mean, na.rm=TRUE)

plot(rownames(stepsPerInterval),stepsPerInterval,type = "l",xlab = "interval", ylab="steps", main='Daily Activity Pattern')
```

![plot of chunk daily_activity_pattern](figure/daily_activity_pattern-1.png) 

### Most active time period. 
The 5-minute interval that, on average, contains the maximum number of steps is 0835 with and average 206.2 steps


```r
summary(stepsPerInterval)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.486  34.110  37.380  52.830 206.200
```

```r
which.max(stepsPerInterval)
```

```
## 835 
## 104
```

## Imputing missing values

### Total number of NA

```r
sum( is.na( dat$steps ) ) 
```

```
## [1] 2304
```

### Strategy 
I chose to replace missing values with the median (ignoring NA) for the interval.
Median reduces the effect of outliers.


```r
imputeMedian <- function(x){
    x[is.na(x)] <- median(x,na.rm = TRUE)
    return(x)
}

tmp <- with(dat,aggregate(steps,list(interval),FUN = imputeMedian))

dat2 <- dat
dat2$steps <- as.vector(tmp$x)
```

### Effects of Imputation

After imputing the missing values, I recreated the histgram of total number steps per day. There are distinct difference in the first two left columns.


```r
impSteps <- tapply(dat2$steps,as.character(dat$date),FUN=sum, na.rm=TRUE)

hist(impSteps,breaks = 20, main='Total number of steps per day (after imputing)')
```

![plot of chunk imputed_hist](figure/imputed_hist-1.png) 

Now compare the mean and medians for daily steps using a table to display differences



```r
impDailyMean <- mean(impSteps)
impDailyMedian <- median(impSteps)

library(xtable)
comparisionSummary <- matrix(c(origDailyMean,origDailyMedian, impDailyMean,impDailyMedian), ncol =2)
colnames(comparisionSummary) <- c('with NA', 'after imputation')
rownames(comparisionSummary) <- c('mean', 'median')
xt <- xtable(as.table(comparisionSummary))
print(xt, type='html')
```

<!-- html table generated in R 3.0.3 by xtable 1.7-4 package -->
<!-- Sun Nov 16 13:33:55 2014 -->
<table border=1>
<tr> <th>  </th> <th> with NA </th> <th> after imputation </th>  </tr>
  <tr> <td align="right"> mean </td> <td align="right"> 9354.23 </td> <td align="right"> 9503.87 </td> </tr>
  <tr> <td align="right"> median </td> <td align="right"> 10395.00 </td> <td align="right"> 10395.00 </td> </tr>
   </table>

Imputation utiliseing the median time interval step value has not effected the median daily step count, but has increased the mean daily step count. 



```r
stepsPerInterval <- tapply(dat2$steps, dat2$interval, FUN=mean, na.rm=TRUE)

plot(rownames(stepsPerInterval),stepsPerInterval,type = "l", xlab = "interval", ylab="steps", main='Daily Activity after imputing missing values')
```

![plot of chunk activity_pattern_imputed](figure/activity_pattern_imputed-1.png) 

The most noticable change is the magnitude of the maximum.  
Quick summary calculation shows overal the my method of imputing missing values has 
reduction effect


```r
summary(stepsPerInterval)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    2.16   29.64   33.00   46.57  181.60
```


## Are there differences in activity patterns between weekdays and weekends?

```r
dat2$day <- as.factor(ifelse(weekdays(dat2$date) %in% c('Saturday','Sunday'),'Weekend','Weekday'))

dat3 <- tapply(dat2$steps, list(dat2$day,dat2$interval),FUN=mean)

par(mfrow=c(1,2))
plot(colnames(dat3),dat3[2,],type = "l", xlab = "interval", ylab="steps", main="Weekend Day")
plot(colnames(dat3),dat3[1,],type = "l", xlab = "interval", ylab="step", main="Weekday Days")
```

![plot of chunk weekends](figure/weekends-1.png) 

There are differences between activity patterns on weekends and weekday.  
There is greater overall activity on weekends, especially in the afternoon 
