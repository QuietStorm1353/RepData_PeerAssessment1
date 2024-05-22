# Unzip and load the data
unzip("activity.zip", exdir = "RepData_PeerAssessment1")
activity_data <- read.csv("RepData_PeerAssessment1/activity.csv")

# Remove rows with missing steps
activity_no_na <- na.omit(activity_data)

# Calculate total steps per day
total_steps_per_day <- aggregate(steps ~ date, data = activity_no_na, sum)
total_steps_per_day

# Create histogram of total steps per day
hist(total_steps_per_day$steps, main = "Total Steps Per Day", xlab = "Steps", col = "skyblue")

# Calculate mean and median of total steps per day
mean_steps_per_day <- mean(total_steps_per_day$steps)
mean_steps_per_day
median_steps_per_day <- median(total_steps_per_day$steps)
median_steps_per_day

# Calculate average steps per interval across all days
avg_steps_per_interval <- aggregate(steps ~ interval, data = activity_no_na, mean)

# Create time series plotof average steps per interval
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type = "l",
     xlab = "5-Minute Interval", ylab = "Average Steps")

#Calculate the max interval
max_interval <- avg_steps_per_interval$interval[which.max(avg_steps_per_interval$steps)]
max_interval

# Count total missing values in entire data frame
total_missing_values <- sum(is.na(activity_data))
total_missing_values

# Fill missing values with mean for each interval
activity_imputed <- activity_data
activity_imputed$steps[is.na(activity_imputed$steps)] <- avg_steps_per_interval$steps

# Calculate total steps per day for imputed data
total_steps_per_day_imputed <- aggregate(steps ~ date, data = activity_imputed, sum)
total_steps_per_day_imputed

# Create histogram for imputed data
hist(total_steps_per_day_imputed$steps, main = "Total Steps Per Day (Imputed)",
     xlab = "Steps", col = "salmon")

# Calculate mean and median for imputed data
mean_steps_per_day_imputed <- mean(total_steps_per_day_imputed$steps)
mean_steps_per_day_imputed
median_steps_per_day_imputed <- median(total_steps_per_day_imputed$steps)
median_steps_per_day_imputed

# Create a new factor variable for weekdays and weekends
activity_imputed$day_type <- ifelse(weekdays(as.Date(activity_imputed$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday")

# Calculate average steps per interval by day type
avg_steps_by_day_type <- aggregate(steps ~ interval + day_type, data = activity_imputed, mean)

# Create panel plot of steps by interval for weekdays vs weekends
library(ggplot2)
ggplot(avg_steps_by_day_type, aes(x = interval, y = steps, color = day_type)) +
  geom_line() +
  labs(x = "5-Minute Interval", y = "Average Steps", title = "Activity Patterns: Weekdays vs. Weekends")

