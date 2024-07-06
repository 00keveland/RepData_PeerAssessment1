#Reproducible Data Programming Assignment 1: 

#Libraries needed
library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("activity.csv")

# Process/transform the data if necessary
data$date <- as.Date(data$date)
data$steps <- as.numeric(data$steps)

# Check if there are any list columns and convert them
data <- data %>%
  mutate(across(where(is.list), ~unlist(.)))



# What is mean total number of steps taken per day?

## Calculate total number of steps taken per day
total_steps_per_day <- data %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps, na.rm = TRUE))

## Make a histogram of the total number of steps taken each day
ggplot(total_steps_per_day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "black") +
  labs(title = "Total Number of Steps Taken Each Day",
       x = "Total Steps", y = "Frequency") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

## Calculate and report the mean and median total number of steps taken per day
mean_total_steps <- mean(total_steps_per_day$total_steps)
median_total_steps <- median(total_steps_per_day$total_steps)
cat("Mean total number of steps taken per day: ", mean_total_steps, "\n")
cat("Median total number of steps taken per day: ", median_total_steps, "\n")



# What is the average daily activity pattern?

## Calculate the average daily activity pattern
average_daily_activity <- data %>%
  group_by(interval) %>%
  summarize(average_steps = mean(steps, na.rm = TRUE))

## Make a time series plot of the 5-minute interval and the average number of steps taken
ggplot(average_daily_activity, aes(x = interval, y = average_steps)) +
  geom_line(color = "blue") +
  labs(title = "Average Daily Activity Pattern",
       x = "5-minute Interval", y = "Average Number of Steps") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval <- average_daily_activity[which.max(average_daily_activity$average_steps), ]
cat("5-minute interval with maximum average steps: ", max_interval$interval, "\n")



# Imputing missing values

## Calculate the total number of missing values in the dataset
total_missing_values <- sum(is.na(data$steps))
cat("Total number of missing values: ", total_missing_values, "\n")

## Strategy for filling in all of the missing values: use the mean for that 5-minute interval
filled_data <- data
for (i in 1:nrow(filled_data)) {
  if (is.na(filled_data$steps[i])) {
    interval <- filled_data$interval[i]
    filled_data$steps[i] <- average_daily_activity[average_daily_activity$interval == interval, "average_steps"]
  }
}

## Ensure steps column is numeric and non-NA
filled_data$steps <- as.numeric(filled_data$steps)

## Make a histogram of the total number of steps taken each day with the filled-in data
total_steps_per_day_filled <- filled_data %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps))

ggplot(total_steps_per_day_filled, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "black") +
  labs(title = "Total Number of Steps Taken Each Day (Filled Data)",
       x = "Total Steps", y = "Frequency") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

## Calculate and report the mean and median total number of steps taken per day with the filled-in data
mean_total_steps_filled <- mean(total_steps_per_day_filled$total_steps)
median_total_steps_filled <- median(total_steps_per_day_filled$total_steps)
cat("Mean total number of steps taken per day (filled data): ", mean_total_steps_filled, "\n")
cat("Median total number of steps taken per day (filled data): ", median_total_steps_filled, "\n")

## Compare the estimates from the filled data with the original estimates
cat("Difference in mean: ", mean_total_steps_filled - mean_total_steps, "\n")
cat("Difference in median: ", median_total_steps_filled - median_total_steps, "\n")



# Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
filled_data$day_type <- ifelse(weekdays(filled_data$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

## Make a panel plot of the average number of steps taken, averaged across all weekday days or weekend days
average_steps_by_day_type <- filled_data %>%
  group_by(interval, day_type) %>%
  summarize(average_steps = mean(steps))

ggplot(average_steps_by_day_type, aes(x = interval, y = average_steps, color = day_type)) +
  geom_line() +
  facet_wrap(~ day_type, ncol = 1) +
  labs(title = "Average Daily Activity Pattern by Day Type",
       x = "5-minute Interval", y = "Average Number of Steps") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Save the last plot as a PNG file in the working directory
ggsave("average_daily_activity_by_day_type.png", width = 8, height = 6)
