install.packages("ggplot2")
library(ggplot2)
install.packages("lattice")
library(lattice)

setwd("C:/Users/MIGUEL GUILLEN/Documents/R")

Actividad <- data.table::fread(input = "activity.csv")

###########################################################
# P1 - What is mean total number of steps taken per day?
###########################################################

#Calculate the total number of steps taken per day
TPasos <- Actividad[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
head(TPasos, 10)

#If you do not understand the difference between a histogram and a barplot, 
#research the difference between them. Make a histogram of the total number of steps taken each day
dev.copy(png,"Q1.png", width=480, height=480)
ggplot(TPasos, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")
dev.off()

#Calculate and report the mean and median of the total number of steps taken per day
TPasos[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]

###########################################################
# P2 - What is the average daily activity pattern?
###########################################################

#Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number 
#of steps taken, averaged across all days (y-axis)

Intervalo <- Actividad[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 

dev.copy(png,"Q2.png", width=480, height=480)
ggplot(Intervalo, aes(x = interval , y = steps)) + 
                 geom_line(color="blue", size=1) + 
       labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
dev.off()

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Intervalo[steps == max(steps), .(max_interval = interval)]

###########################################################
# P3 - Imputing missing values
###########################################################

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)
nrow(Actividad[is.na(steps),])

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
#For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Actividad[is.na(steps), "steps"] <- Actividad[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
data.table::fwrite(x = Actividad, file = "tidyData.csv", quote = FALSE)

#Make a histogram of the total number of steps taken each day and calculate and report the mean and median total 
#number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?

Total_Steps <- Actividad[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]

dev.copy(png,"Q3.png", width=480, height=480)
ggplot(Total_Steps, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")
dev.off()


###########################################################
# P4 - Are there differences in activity patterns between weekdays and weekends?
###########################################################

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether 
#a given date is a weekday or weekend day.
Actividad$day <- weekdays(Actividad$date)
Actividad$week <- ""
Actividad[Actividad$day == "Saturday" | Actividad$day == "Sunday", ]$week <- "weekend"
Actividad[!(Actividad$day == "Saturday" | Actividad$day == "Sunday"), ]$week <- "weekday"
Actividad$week <- factor(Actividad$week)

#Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and 
#the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the 
#README file in the GitHub repository to see an example of what this plot should look like using simulated data.

avg_step_imp <- aggregate(steps ~ interval + week, data = Actividad, mean)

dev.copy(png,"Q4.png", width=480, height=480)
xyplot(steps ~ interval | week, data = avg_step_imp, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps",
       main = "Average Number of Steps Taken (across all weekday days or weekend days)")
dev.off()

