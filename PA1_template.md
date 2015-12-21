---
title: "Reproducible Research Assignment 1 Final"
author: "Christian Segarra"
date: "December 20, 2015"
output: html_document
---


## Loading and preprocessing the data
To complete the assignment, the data first needed to be read in, and processed by removing any NA values. Any necessary packages were also loaded at this point.


```r
library(dplyr)
library(tidyr)
library(lubridate)
library(lattice)

##Read in data & remove NA values
activityComplete <- read.csv("activity.csv", header = TRUE)
notNA <- !is.na(activityComplete$steps)
activity <- activityComplete[notNA, ]
```


## What is mean total number of steps taken per day?

The next step is to find the total number of steps in a day and find the mean and median of the total number of steps per day, as done by the code below.

```r
activity <- group_by(activity, date)
dailySteps <- summarize(activity, sum(steps))
meanSteps <- round(mean(dailySteps$`sum(steps)`), digits = 4)
medianSteps <- round(median(dailySteps$`sum(steps)`), digits = 4)
```
We find the mean to be 1.0766189 &times; 10<sup>4</sup> and the median to be 1.0765 &times; 10<sup>4</sup>.  
  
Histograms showing the total number of steps in a day are below. One histogram shows the mean number of steps in a day, the second histogram shows the median number of steps taken in a day.

```r
hist(dailySteps$`sum(steps)`, breaks = 5, col = "blue", main = "Histogram Of Total Number of Steps Taken In A Day", xlab = "Number Of Steps Taken", ylab = "Frequency Of Number Of Steps Taken", mfcol = c(2, 1))
abline(v = meanSteps, lwd = 2, col = "red")
mtext(text = paste("Mean Steps = ", as.character(meanSteps)), at = c(meanSteps))
```

<img src="figure/Part 1 Plots-1.png" title="plot of chunk Part 1 Plots" alt="plot of chunk Part 1 Plots" style="display: block; margin: auto;" />

```r
hist(dailySteps$`sum(steps)`, breaks = 5, col = "blue", main = "Histogram Of Total Number of Steps Taken In A Day", xlab = "Number Of Steps Taken", ylab = "Frequency Of Number Of Steps Taken", mfcol = c(2, 1))
abline(v = medianSteps, lwd = 2, col = "orange")
mtext(text = paste("Median Steps = ", as.character(medianSteps)), at = c(medianSteps))
```

<img src="figure/Part 1 Plots-2.png" title="plot of chunk Part 1 Plots" alt="plot of chunk Part 1 Plots" style="display: block; margin: auto;" />

## What is the average daily activity pattern?


Next we needed to find the average number of steps taken during each time interval. The code below performed all necessary calculations.

```r
activity <- group_by(activity, interval)
intervalSteps <- summarize(activity, mean(steps))
```

After this, we were able to find which interval contained the highest number of steps on average.`

```r
maxStepsDisplay <- round(max(intervalSteps$`mean(steps)`), digits = 2)
maxSteps <- max(intervalSteps$`mean(steps)`)
id <- which(intervalSteps$`mean(steps)` == maxSteps)
topInterval <- intervalSteps[id, 1]
```

The top interval happens to be 835.
  
Finally, the code below plots the average number of steps per time interval.

```r
plot(intervalSteps$interval, intervalSteps$`mean(steps)`, type = "l", main = "Average Steps Taken During Each Time Interval", xlab = "Time Interval", ylab = "Average Number of Steps Taken")
mtext(paste("Max Steps = ", as.character(maxStepsDisplay), "at interval", as.character(topInterval)), at = topInterval)
```

<img src="figure/Part 2 Plot-1.png" title="plot of chunk Part 2 Plot" alt="plot of chunk Part 2 Plot" style="display: block; margin: auto;" />

## Imputing missing values

Now we will decide what to do with all missing values. First we create a new data set and calculate how many missing values there are. The code below does this.

```r
newActivityComplete <- activityComplete
totalNA <- sum(!notNA)
```
We can see that there are 2304 missing values in total.
  
Now we will fill in the missing values with the average number of steps for that time interval averaged throughout all of the days as shown by the code below.

```r
for (i in 1:nrow(newActivityComplete)) {
  
  if (is.na(newActivityComplete[i, 1])) {
    interval <- newActivityComplete[i, 3]
    id <- which(intervalSteps$interval == interval)
    newActivityComplete[i, 1] <- intervalSteps[id, 2]
    
  }
  
}
```
Now we will recalculate the mean and median to see if filling in the missing values made a difference.

```r
newActivityComplete <- group_by(newActivityComplete, date)
newDailySteps <- summarize(newActivityComplete, sum(steps))
newMeanSteps <- round(mean(newDailySteps$`sum(steps)`), digits = 4)
newMedianSteps <- round(median(newDailySteps$`sum(steps)`), digits = 4)
```
The new mean is 1.0766189 &times; 10<sup>4</sup> and the new median is 1.0766189 &times; 10<sup>4</sup>. This does not differ significantly from the previous mean of 1.0766189 &times; 10<sup>4</sup> and median of 1.0765 &times; 10<sup>4</sup>. However, because we filled in the missing values with the average steps for each time interval, the median is now equivalent to the mean, which are both equivalent to the original mean.
  
Plots of the new data are shown below.

```r
hist(newDailySteps$`sum(steps)`, breaks = 5, col = "blue", main = "Histogram Of Total Number of Steps Taken In A Day", xlab = "Number Of Steps Taken", ylab = "Frequency Of Number Of Steps Taken", mfcol = c(2, 1))
abline(v = newMeanSteps, lwd = 2, col = "red")
mtext(text = paste("New Mean Steps = ", as.character(newMeanSteps)), at = c(newMeanSteps))
```

<img src="figure/New Plots-1.png" title="plot of chunk New Plots" alt="plot of chunk New Plots" style="display: block; margin: auto;" />

```r
hist(newDailySteps$`sum(steps)`, breaks = 5, col = "blue", main = "Histogram Of Total Number of Steps Taken In A Day", xlab = "Number Of Steps Taken", ylab = "Frequency Of Number Of Steps Taken", mfcol = c(2, 1))
abline(v = newMedianSteps, lwd = 2, col = "orange")
mtext(text = paste("New Median Steps = ", as.character(newMedianSteps)), at = c(newMedianSteps))
```

<img src="figure/New Plots-2.png" title="plot of chunk New Plots" alt="plot of chunk New Plots" style="display: block; margin: auto;" />
## Are there differences in activity patterns between weekdays and weekends?

Finally, we want to see if there are any difference in activity patterns on weekdays and weekends. We will need to use the data set with no missing values. First, we will need to covert the date column from factor to date and then add a column that indicates whether the date occured on a weekday or weekend.


```r
newActivityComplete$date <- as.Date(newActivityComplete$date)
newActivityComplete$day <- wday(newActivityComplete$date, label = TRUE)
newActivityComplete$day <- as.character(newActivityComplete$day)

for (j in 1:nrow(newActivityComplete)) {
  
  if (newActivityComplete$day[j] == "Sat") {
    
    newActivityComplete$Week[j] <- "Weekend"
    
  } else if (newActivityComplete$day[j] == "Sun") {
    
    newActivityComplete$Week[j] <- "Weekend"
    
  } else {
    
    newActivityComplete$Week[j] <- "Weekday"
    
  }
  
}
```
Now we will calculate the average number of steps taken during each interval.

```r
newActivityComplete <- group_by(newActivityComplete, interval, Week)
newIntervalSteps <- summarize(newActivityComplete, mean(steps))
```

Finally, we plot the data and distinguish between Weekends and Weekdays below.


```r
xyplot(`mean(steps)` ~ interval | Week, data = newIntervalSteps, type = "l", xlab = "Interval", ylab = "Average Number Of Steps", main = "Average Number Of Steps Taken Per Interval")
```

<img src="figure/final plots-1.png" title="plot of chunk final plots" alt="plot of chunk final plots" style="display: block; margin: auto;" />

It seems like during Weekends, there were more days that had a higher number of steps taken during each interval as opposed to Weekdays, with the exception of one Weekday interval that was higher than any other weekday or Weekend interval.
