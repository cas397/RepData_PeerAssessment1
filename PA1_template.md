---
title: "Reproducible Research Assignment 1 Final"
author: "Christian Segarra"
date: "December 20, 2015"
output: html_document
---


## Loading and preprocessing the data
To complete the assignment, the data first needed to be read in, and processed by removing any NA values. Any necessary packages were also loaded at this point.

```{r, Processing Data, message = F}
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
``` {r, total steps}
activity <- group_by(activity, date)
dailySteps <- summarize(activity, sum(steps))
meanSteps <- round(mean(dailySteps$`sum(steps)`), digits = 4)
medianSteps <- round(median(dailySteps$`sum(steps)`), digits = 4)
```
We find the mean to be `r meanSteps` and the median to be `r medianSteps`.  
  
Histograms showing the total number of steps in a day are below. One histogram shows the mean number of steps in a day, the second histogram shows the median number of steps taken in a day.
```{r, Part 1 Plots, fig.align = "center"}
hist(dailySteps$`sum(steps)`, breaks = 5, col = "blue", main = "Histogram Of Total Number of Steps Taken In A Day", xlab = "Number Of Steps Taken", ylab = "Frequency Of Number Of Steps Taken", mfcol = c(2, 1))
abline(v = meanSteps, lwd = 2, col = "red")
mtext(text = paste("Mean Steps = ", as.character(meanSteps)), at = c(meanSteps))

hist(dailySteps$`sum(steps)`, breaks = 5, col = "blue", main = "Histogram Of Total Number of Steps Taken In A Day", xlab = "Number Of Steps Taken", ylab = "Frequency Of Number Of Steps Taken", mfcol = c(2, 1))
abline(v = medianSteps, lwd = 2, col = "orange")
mtext(text = paste("Median Steps = ", as.character(medianSteps)), at = c(medianSteps))
```

## What is the average daily activity pattern?


Next we needed to find the average number of steps taken during each time interval. The code below performed all necessary calculations.
```{r, mean steps}
activity <- group_by(activity, interval)
intervalSteps <- summarize(activity, mean(steps))
```

After this, we were able to find which interval contained the highest number of steps on average.`
```{r, max interval}
maxStepsDisplay <- round(max(intervalSteps$`mean(steps)`), digits = 2)
maxSteps <- max(intervalSteps$`mean(steps)`)
id <- which(intervalSteps$`mean(steps)` == maxSteps)
topInterval <- intervalSteps[id, 1]
```

The top interval happens to be `r topInterval`.
  
Finally, the code below plots the average number of steps per time interval.
```{r, Part 2 Plot, fig.align = "center"}
plot(intervalSteps$interval, intervalSteps$`mean(steps)`, type = "l", main = "Average Steps Taken During Each Time Interval", xlab = "Time Interval", ylab = "Average Number of Steps Taken")
mtext(paste("Max Steps = ", as.character(maxStepsDisplay), "at interval", as.character(topInterval)), at = topInterval)
```

## Imputing missing values

Now we will decide what to do with all missing values. First we create a new data set and calculate how many missing values there are. The code below does this.
```{r, Total NA}
newActivityComplete <- activityComplete
totalNA <- sum(!notNA)
```
We can see that there are `r totalNA` missing values in total.
  
Now we will fill in the missing values with the average number of steps for that time interval averaged throughout all of the days as shown by the code below.
```{r, Fill Values}
for (i in 1:nrow(newActivityComplete)) {
  
  if (is.na(newActivityComplete[i, 1])) {
    interval <- newActivityComplete[i, 3]
    id <- which(intervalSteps$interval == interval)
    newActivityComplete[i, 1] <- intervalSteps[id, 2]
    
  }
  
}
```
Now we will recalculate the mean and median to see if filling in the missing values made a difference.
```{r, New Values}
newActivityComplete <- group_by(newActivityComplete, date)
newDailySteps <- summarize(newActivityComplete, sum(steps))
newMeanSteps <- round(mean(newDailySteps$`sum(steps)`), digits = 4)
newMedianSteps <- round(median(newDailySteps$`sum(steps)`), digits = 4)
```
The new mean is `r newMeanSteps` and the new median is `r newMedianSteps`. This does not differ significantly from the previous mean of `r meanSteps` and median of `r medianSteps`. However, because we filled in the missing values with the average steps for each time interval, the median is now equivalent to the mean, which are both equivalent to the original mean.
  
Plots of the new data are shown below.
```{r, New Plots, fig.align = "center"}
hist(newDailySteps$`sum(steps)`, breaks = 5, col = "blue", main = "Histogram Of Total Number of Steps Taken In A Day", xlab = "Number Of Steps Taken", ylab = "Frequency Of Number Of Steps Taken", mfcol = c(2, 1))
abline(v = newMeanSteps, lwd = 2, col = "red")
mtext(text = paste("New Mean Steps = ", as.character(newMeanSteps)), at = c(newMeanSteps))

hist(newDailySteps$`sum(steps)`, breaks = 5, col = "blue", main = "Histogram Of Total Number of Steps Taken In A Day", xlab = "Number Of Steps Taken", ylab = "Frequency Of Number Of Steps Taken", mfcol = c(2, 1))
abline(v = newMedianSteps, lwd = 2, col = "orange")
mtext(text = paste("New Median Steps = ", as.character(newMedianSteps)), at = c(newMedianSteps))
```
## Are there differences in activity patterns between weekdays and weekends?

Finally, we want to see if there are any difference in activity patterns on weekdays and weekends. We will need to use the data set with no missing values. First, we will need to covert the date column from factor to date and then add a column that indicates whether the date occured on a weekday or weekend.

```{r, Adding Weekdays}
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
```{r, Weekday Weekend}
newActivityComplete <- group_by(newActivityComplete, interval, Week)
newIntervalSteps <- summarize(newActivityComplete, mean(steps))
```

Finally, we plot the data and distinguish between Weekends and Weekdays below.

```{r, final plots, fig.align = "center"}
xyplot(`mean(steps)` ~ interval | Week, data = newIntervalSteps, type = "l", xlab = "Interval", ylab = "Average Number Of Steps", main = "Average Number Of Steps Taken Per Interval")
```

It seems like during Weekends, there were more days that had a higher number of steps taken during each interval as opposed to Weekdays, with the exception of one Weekday interval that was higher than any other weekday or Weekend interval.
