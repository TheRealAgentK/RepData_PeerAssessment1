---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Abstract

This report presents the results of a data analysis that forms Peer Assessment 1 of the Coursera MOOC Reproducible Research.

The analysis looks at data collected from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Initially we load some libraries that this analysis is going to use:


```r
library(knitr)
library(data.table)
```

## Loading and preprocessing the data

First of all we load the data from the .csv file into memory using the read.csv function. This assumes that the unzipped .csv file is in the current working directory:


```r
movementData <- read.csv("activity.csv", header=TRUE, colClasses=c("numeric","Date","numeric"))
```

## What is mean total number of steps taken per day?

We create an aggregated variable of steps per day. Looking at the min and max value we find that they range between 41 and 21194. The histrogram therefore uses steps of 1000s for the x axis by specificying 22 breaks.

The mean and median are calculated using the mean() resp. median() functions and turn out to very close together.


```r
spd <- aggregate(steps ~ date, movementData, sum)
max(spd$steps)
```

[1] 21194

```r
min(spd$steps)
```

[1] 41

```r
hist(spd$steps,breaks=22, main="Distribution of steps per day",xlab="Steps per day",ylab="Frequency")
```

![plot of chunk stepsperday](figure/stepsperday.png) 

```r
mean(spd$steps)
```

[1] 10766

```r
median(spd$steps)
```

[1] 10765

## What is the average daily activity pattern?

We create an aggregated variable of steps per time interval and apply the mean function to that. To find the interval with the maximum number of steps, we use which.max.


```r
spi <- aggregate(movementData$steps, by=list(interval=movementData$interval),FUN=mean,na.rm=TRUE)
plot(spi,type="l",xlab="Interval Number",ylab="Number of steps (mean)",main="Average Daily Activity Pattern")
```

![plot of chunk stepsperinterval](figure/stepsperinterval.png) 

```r
spi_max <- spi[which.max(spi$x),]
```

It turns out the 835th interval with a mean of 206.1698 steps is the interval with the maximum number of steps.

## Imputing missing values

Initially we calculate the number of missing values:


```r
na_values <- sum(is.na(movementData$steps))
```

The number of missing values is 2304.

Part 2-4 of this section have not been done.

## Are there differences in activity patterns between weekdays and weekends?

This section has not been done.
