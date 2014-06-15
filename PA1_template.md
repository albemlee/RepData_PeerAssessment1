# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

- data contains the raw data directly read from activity.csv
- data1 contains the processed data used to produce histogram1.png
- data2 contains the processed data with all NA steps values filled in as the mean   of the day and it is used to produce histogram2.png
- data3 contains the original raw data with an added column that displays the days of the week that each date corresponds to


```r
data <- read.csv("activity.csv")

data1 <- data.frame(data$steps, as.Date(data$date))
names(data1) <- c("steps", "date")
data1 <- data1[complete.cases(data1), ]

data2 <- data.frame(data$steps, as.Date(data$date))
names(data2) <- c("steps", "date")
for(i in 1:nrow(data2)){
  if(is.na(data2[i, "steps"])){
    data2[i, "steps"] <- mean(data2$date==data2[i, "date"], na.rm=TRUE)
  }
}

data3 <- data
data3$date <- as.Date(data3$date)
day <- weekdays(data3$date)
data3 <- cbind(data3,day)
```

## What is mean total number of steps taken per day?

A histogram of the mean total number of steps taken per day is created in this section (histogram1.png). The code first sums the steps for each day, and then plots the results on a histogram. The code also returns the mean and median of the total number of steps taken per day.


```r
result1 <- data.frame()
for(day in data1$date){
  result1 <- rbind(result1, sum(data1[data1$date==day, "steps"]))
}
result1 <- data.frame(as.Date(unique(data1$date)), unique(result1))
names(result1) <- c("date", "steps")
png(filename = "histogram1.png")
hist(result1$steps, col="pink", main="Histogram of Steps per Day",
     xlab="Steps per Day")
dev.off()
```

```
## pdf 
##   2
```

```r
mean(result1$steps)
```

```
## [1] 10766
```

```r
median(result1$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

In this section the code first determines the mean number of steps for each interval value. It then plots the results in a time series plot (lineplot1.png) with the interval values on the x-axis and the mean number of steps on the y-axis. It also returns the interval which had the greatest mean number of steps.


```r
plotvalues <- data.frame()
for(i in data$interval){
  plotvalues = rbind(plotvalues, c(i, mean(data[data$interval==i,
                                                "steps"], na.rm=TRUE))) 
}
names(plotvalues) <- c("interval","steps")
png(filename = "lineplot1.png")
plot(plotvalues$interval,plotvalues$steps, type="l", xlab="interval",
     ylab="steps")
dev.off()
```

```
## pdf 
##   2
```

```r
unique(plotvalues[plotvalues$steps==max(plotvalues$steps),"interval"])
```

```
## [1] 835
```

## Inputing missing values

As stated before, NA steps values were filled in as the mean of the day. The total number of NA's is returned. A histogram using this as the data instead of data with the NAs removed is produced (histogram2.png). In this histogram, there seems to be a high frequency of days with lower number of steps taken compared to the original histogram. Also confirming this is the returned mean and median values, which are lower than they were for the original dataset.



```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
result2 <- data.frame()
y <- vector()
for(day in data2$date){
  y <- append(y, sum(data2[data2$date==day, "steps"]))
}
result2 <- data.frame(data2$date,y)
result2 <- unique(result2)
names(result2) <- c("date", "steps")
png(filename = "histogram2.png")
hist(result2$steps, col="pink", main="Histogram of Steps per Day",
     xlab="Steps per Day")
dev.off()
```

```
## pdf 
##   2
```

```r
mean(result2$steps)
```

```
## [1] 9355
```

```r
median(result2$steps)
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?

Two line plots with the interval values on the x-axis and the mean steps taken on the y-axis (lineplot2.png). The upper plot contains a line plot with weekday data values and the lower plot contains a line plot with weekend data values. It seems like on weekdays, a greater number of steps is taken in a short interval (between 500 and 1000) while on weekends, the steps taken are spread out more evenly throughout the day. Another difference is that on weekends, it seems that steps are being taken later in the day.



```r
plotvalues2 <- data.frame()
for(i in data3$interval){
  plotvalues2 = rbind(plotvalues2, c(i, mean(data3[data3$interval==i & data3$day %in%
                                                     c("Monday", "Tuesday", "Wednesday",
                                                       "Thursday", "Friday"),
                                                   "steps"], na.rm=TRUE))) 
}
names(plotvalues2) <- c("interval","steps")

plotvalues3 <- data.frame()
for(i in data3$interval){
  plotvalues3 = rbind(plotvalues3, c(i, mean(data3[data3$interval==i & data3$day %in%
                                                     c("Saturday","Sunday"),
                                                   "steps"], na.rm=TRUE))) 
}
names(plotvalues3) <- c("interval","steps")

png(filename = "lineplot2.png")
par(mfrow = c(2,1))
  plot(plotvalues2$interval,plotvalues2$steps, type="l", main="weekday", xlab="interval",
     ylab="steps")
  plot(plotvalues3$interval,plotvalues3$steps, type="l", main="weekend", xlab="interval",
     ylab="steps")
dev.off()
```

```
## pdf 
##   2
```

```r
unique(plotvalues2[plotvalues2$steps==max(plotvalues2$steps),"interval"])
```

```
## [1] 835
```

```r
unique(plotvalues3[plotvalues3$steps==max(plotvalues3$steps),"interval"])
```

```
## [1] 915
```
