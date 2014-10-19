# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Load data and prepocess it into a time sequence

```r
dat <- read.csv('activity.csv')
dat$time <- sprintf('%04d', dat$interval)
```

## What is mean total number of steps taken per day?


```r
steps <- tapply(dat$steps,as.character(dat$date),FUN=sum, na.rm=TRUE)

hist(steps,breaks = 10)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(steps, na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
stepsPerInterval <- tapply(dat$steps, dat$interval, FUN=mean, na.rm=TRUE)

plot(stepsPerInterval)
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 
### Summary 
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



## Are there differences in activity patterns between weekdays and weekends?

