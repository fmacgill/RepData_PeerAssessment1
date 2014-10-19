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
y <- tapply(dat$steps, dat$interval, FUN=sum, na.rm=TRUE)

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
```
