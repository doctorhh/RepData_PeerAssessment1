library(dplyr)
library(ggplot2)
library(lubridate)
library(timeDate)
#Reading the file
data_file <- read.csv("activity.csv", header = TRUE, sep = ",")

#Converting the date into a workable format and setting the steps variable as numeric
## to prepare the processed data for further analysis
data_file$date <- ymd(data_file$date)
data_file$steps <- as.numeric(data_file$steps)


# Removing NA records
proc_data <- filter(data_file, !is.na(steps))

# Question 1
total_steps <- proc_data %>% group_by(date) %>% summarise(steps = sum(steps))

# Histogram
hist(total_steps$steps,
     main = "Histogram of steps per day", 
     xlab = "Steps per day", 
     ylab = "Frequency",
     breaks=20,  col = "blue")
mean_total_steps <- mean(total_steps$steps)
median_total_steps <- median(total_steps$steps)

# Question 2
# daily_pattern <- summarise(group_by(proc_data,interval), steps = mean(steps))
daily_pattern <- proc_data %>% group_by(interval) %>% summarize(d_steps = mean(steps))

ggplot(daily_pattern, aes(x=interval, y=d_steps)) + geom_line(color = "blue")

# Maximum of steps interval
max_step_int <- filter(daily_pattern, d_steps == max(d_steps))
max_step_int$interval
 
#Question 3
sum(is.na(data_file$steps)) 

full_data <- inner_join(data_file, daily_pattern, by="interval") %>% 
      mutate(steps = ifelse(is.na(steps),d_steps,steps))

sum(is.na(full_data))

full_total_steps <- full_data %>% group_by(date) %>% summarise(steps = sum(steps))
# Histogram
hist(full_total_steps$steps,
     main = "Histogram of steps per day", 
     xlab = "Steps per day", 
     ylab = "Frequency",
     breaks=20,  col = "blue")
mean_total_steps_full <- mean(full_total_steps$steps)
median_total_steps_full <- median(full_total_steps$steps)

#Question 4
full_data$daystype <- ifelse((isWeekday(data_file$date, wday=1:5)),"Weekdays", "Weekend")
act_pattern <- full_data %>%
      group_by(daystype,interval) %>%
      summarize(steps=mean(steps))

# Panel plot
ggplot(act_pattern, aes(x=interval, y=steps, daystype)) +
      geom_line(colour="blue") +
      facet_wrap(~daystype, ncol = 1, nrow=2)
