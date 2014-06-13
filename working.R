## load ggplot2 library
library(ggplot2)

## load gridExtra library
library(gridExtra)

## unzip data file
unzip("activity.zip")

## read data into memory
df <- read.csv("activity.csv", header=TRUE, 
                         colClasses = c("numeric", "character", "numeric"))
## convert date string to date type
df[,"date"] <- as.Date(df[,"date"], format="%Y-%m-%d")

## output summary and first few lines of data to ensure data was read correctly
summary(df)
head(df)

## calculate sum of steps per day, ignoring any NAs
stepsPerDay <- aggregate(steps~date, df, na.rm=TRUE, FUN=sum)
## calculate mean steps per day
meanStepsPerDay <- mean(stepsPerDay$steps)
## calculate median steps per day
medianStepsPerDay <- median(stepsPerDay$steps)

## create histogram of total steps per day
plot1 <- (ggplot(stepsPerDay, aes(x=steps)) 
        + geom_histogram(colour="cadetblue4", 
                         fill="cadetblue3", 
                         binwidth=1000) 
        + geom_vline(aes(xintercept = meanStepsPerDay), 
                     colour="red")
        + labs(title = "Histogram of Steps Taken per Day", 
               x = "Number of Steps", 
               y = "Number of Days")
        + annotate("text", 
                   x = meanStepsPerDay, 
                   y = 10.3, 
                   hjust=-0.1,
                   label = "Median", 
                   colour="red")
        )

## calculate mean number of steps per each 5 minute interval, averaged over all
## days
stepsPerInt <- aggregate(steps~interval, df, na.rm=TRUE, FUN=mean)
## convert interval to a decimal number representing hours: this will make the 
## time series chart easier to plot
stepsPerInt$time <- (as.numeric(substr(sprintf("%04d", stepsPerInt$interval), 1, 2)) 
                            + as.numeric(substr(sprintf("%04d", stepsPerInt$interval), 3, 4))/60)

## create time series plot of average steps per interval
plot2 <- (ggplot(stepsPerInt, aes(x=time, y=steps)) 
        + geom_line(color="red")
        + labs(title = "Mean Number of Steps Taken at 5 Minute Intervals", 
               x = "Time (hours)", 
               y = "Steps")
        + scale_x_continuous(breaks=c(seq(0,24,by=1)))
        )

## determine which interval has the largest mean number of steps
maxInt <- stepsPerInt[stepsPerInt$steps==max(stepsPerInt$steps),]

## calculate how many rows in the original dataset have NAs
numberNA <- nrow(df) - nrow(df[complete.cases(df),])

## create copy of original data set
df2 <- df
## impute missing values (NAs) by replacing with the mean number of steps for 
## the interval
for (i in 1:nrow(df2)) {
        if (is.na(df2[i, "steps"])) {
                df2[i, "steps"] <- mean(df[df$interval == df[i, "interval"], "steps"], na.rm=TRUE)
        }
}

## calculate the total steps per day with the additional imputed values, averaged
## over all days
stepsPerDay.imputed <- aggregate(steps~date, df2, na.rm=TRUE, FUN=sum)
## calculate mean steps per day with additional imputed values
meanStepsPerDay.imputed <- mean(stepsPerDay.imputed$steps)
## calculate dian steps per day
medianStepsPerDay.imputed <- median(stepsPerDay.imputed$steps)

## create histogram of total steps per day with the additional imputed values
plot3 <- (ggplot(stepsPerDay.imputed, aes(x=steps)) 
          + geom_histogram(colour="cadetblue4", 
                           fill="cadetblue3", 
                           binwidth=1000)
          + labs(title = "Histogram of Steps Taken per Day with Imputed Data", 
                 x = "Number of Steps", 
                 y = "Number of Days")
)
## add median line to the plot
## the annotate command examines the calculated range in the plot to
## dynamically determine where to place the text label - it ensures it
## always appears above the highest peak in the histogram
plot3 <- (plot3 + geom_vline(aes(xintercept = meanStepsPerDay.imputed), 
                            colour="red")
                + annotate("text", 
                     x = meanStepsPerDay.imputed, 
                     y = ggplot_build(plot3)$panel$ranges[[1]]$y.range[2],
                     vjust=0.5,
                     hjust=-0.1,
                     label = "Median", 
                     colour="red"))

## add factor column to data frame that identifies the observation as occurring
## on a weekday or a weekend
df2$weekdayOrWeekend <- ifelse(weekdays(df2$date) %in% c("Saturday", "Sunday"), 
                               "weekend", "weekday")

## calculate mean number of steps per each 5 minute interval, averaged over all
## days. Group into two data frames based on weekday or weekend.
stepsPerInt.weekday <- aggregate(steps~interval, df2[df2$weekdayOrWeekend=="weekday",], na.rm=TRUE, FUN=mean)
stepsPerInt.weekend <- aggregate(steps~interval, df2[df2$weekdayOrWeekend=="weekend",], na.rm=TRUE, FUN=mean)

## add decimal time value to both data frames
stepsPerInt.weekday$time <- (as.numeric(substr(sprintf("%04d", stepsPerInt.weekday$interval), 1, 2)) 
                     + as.numeric(substr(sprintf("%04d", stepsPerInt.weekday$interval), 3, 4))/60)
stepsPerInt.weekend$time <- (as.numeric(substr(sprintf("%04d", stepsPerInt.weekend$interval), 1, 2)) 
                             + as.numeric(substr(sprintf("%04d", stepsPerInt.weekend$interval), 3, 4))/60)

## determine maximum number of steps in both data frames so that the following
## plots can use the same scale. Round to nearest 50
maxY.weekday <- max(stepsPerInt.weekday$steps)
maxY.weekend <- max(stepsPerInt.weekend$steps)
maxY <- ifelse(maxY.weekday > maxY.weekend, maxY.weekday, maxY.weekend)
maxY <- round(maxY/50)*50

## create time series plot of average steps per interval
plot4 <- (ggplot(stepsPerInt.weekend, aes(x=time, y=steps)) 
          + geom_line(color="blue")
          + labs(title = "Weekend",
                 x=NULL,
                 y=NULL)
          + scale_x_continuous(breaks=c(seq(0, 24, by=2)))
          + ylim(0, maxY)
          + theme(plot.margin=unit(c(5,5,0,5),"mm"))
)

plot5 <- (ggplot(stepsPerInt.weekday, aes(x=time, y=steps)) 
          + geom_line(color="blue")
          + labs(title = "Weekday",
                 x=NULL,
                 y=NULL)
          + scale_x_continuous(breaks=c(seq(0, 24, by=2)))
          + ylim(0, maxY)
          + theme(plot.margin=unit(c(0,5,5,5),"mm"))
)

grid.arrange(plot4, plot5, nrow=2, left="Steps", sub="Time (hours)", main = "Mean Number of Steps per 5 Minute Interval: Weekend vs. Weekday")