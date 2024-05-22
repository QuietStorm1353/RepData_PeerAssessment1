---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

For this assignment, we will be analyzing activity data for an anonymous individual, based on step counts collected at 5 minute intervals throughout the day, for October and November of 2012. 

First, we'll unzip and read in the data file, removing missing values in the data.


```r
unzip("activity.zip", exdir = "RepData_PeerAssessment1")
activity_data <- read.csv("RepData_PeerAssessment1/activity.csv")
activity_no_na <- na.omit(activity_data)
```

Next, we'll calculate the total steps taken per day, as well as the mean and median, illustrated by the histogram below.


```r
total_steps_per_day <- aggregate(steps ~ date, data = activity_no_na, sum)

mean_steps_per_day <- mean(total_steps_per_day$steps)

median_steps_per_day <- median(total_steps_per_day$steps)

hist(total_steps_per_day$steps, main = "Total Steps Per Day", xlab = "Steps", col = "skyblue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

We see that the mean of the total steps taken per day is 10766.19.
Now, we'll look at a time series plot of the average daily pattern to find which interval, on average, contains the max number of steps.


```r
avg_steps_per_interval <- aggregate(steps ~ interval, data = activity_no_na, mean)

plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type = "l",
     xlab = "5-Minute Interval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max_interval <- avg_steps_per_interval$interval[which.max(avg_steps_per_interval$steps)]
```

Looks like the max interval is 835.  To understand what impact our null values have on our averages, we'll count how many we have,  fill each  missing value with the mean for the corresponding interval, and create a histogram of the total steps per day with the estimated values.


```r
total_missing_values <- sum(is.na(activity_data))

activity_imputed <- activity_data
activity_imputed$steps[is.na(activity_imputed$steps)] <- avg_steps_per_interval$steps

total_steps_per_day_imputed <- aggregate(steps ~ date, data = activity_imputed, sum)

mean_steps_per_day_imputed <- mean(total_steps_per_day_imputed$steps)
median_steps_per_day_imputed <- median(total_steps_per_day_imputed$steps)


hist(total_steps_per_day_imputed$steps, main = "Total Steps Per Day (Imputed)",
     xlab = "Steps", col = "salmon")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

We seem to be missing 2,304 counts.  Using our estimated values, both our mean and median steps per day are the same as our original mean; 10766.19. This does not seem to have impacted the data.

Finally, we'll take a look at the activity patterns between weekdays and weekends, using the plot below. 


```r
activity_imputed$day_type <- ifelse(weekdays(as.Date(activity_imputed$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday")

avg_steps_by_day_type <- aggregate(steps ~ interval + day_type, data = activity_imputed, mean)

library(ggplot2)
ggplot(avg_steps_by_day_type, aes(x = interval, y = steps, color = day_type)) +
  geom_line() +
  labs(x = "5-Minute Interval", y = "Average Steps", title = "Activity Patterns: Weekdays vs. Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
We can see that weekdays seem to have more activity first thing in the morning, but weekends have greater sustaine activity throughut the day. 


