# Reproducible Research (Project 1)
Eric Pawlakos  
June 30, 2016  



## Loading and preprocessing the dataset

Begin by downloading the required dataset

```r
if(!file.exists("data")){
  dir.create("data")
}
```

Download and extract the zip file

```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  unzip(temp)
  unlink(temp)
}
```

Read in the dataset

```r
activity <- read.csv("activity.csv")
```

Preprocessing the data

```r
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime <- as.POSIXct(activity$date, format = "%Y-%m-%d")
```
 
## What is mean total number of steps taken per day? 

```r
totalsteps <- aggregate(steps ~ date, activity, FUN = sum)
hist(totalsteps$steps, xlab = "Number of Steps", main = "Total Steps Each Day")
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

What is the mean and median? 

```r
smean <- mean(totalsteps$steps)
smedian <- median(totalsteps$steps)
```


## What is the average daily pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalsteps <- aggregate(steps ~ interval, activity, mean)
plot(intervalsteps$interval, intervalsteps$steps, type = "l",
     xlab = "Interval", ylab = "Number of Steps",
     main = "Average Number of Steps Taken Per Day (by intervals)")
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalmax <- intervalsteps[which.max(intervalsteps$steps), 1]
intervalmax
```

```
## [1] 835
```


## Imputing missing values 

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totalNA <- sum(is.na(activity))
totalNA
```

```
## [1] 2304
```

# Devise a strategy for filling in all of the missing values in the dataset. 

My strategy for filling in the missing values will be to use the mean steps for the day of missing data 

```r
activity2 <- activity
NAs <- is.na(activity2$steps)
intervalavg <- tapply(activity2$steps, activity2$interval, mean, na.rm = TRUE,
                      simplify = TRUE)
activity2$steps[NAs] <- intervalavg[as.character(activity2$interval[NAs])]
```
Create a histogram of the imputed data and compare with the original non-imputed data 


```r
intervalsteps2 <- tapply(activity2$steps, activity2$date, sum, na.rm=TRUE, simplify=T)
hist(intervalsteps2, xlab = "Steps", main = "Histogram of imputed and non-imputed data",
     col = "blue")
hist(totalsteps$steps, col = "red", add = TRUE)
legend("topright", c("Imputed Data", "Original Data"), fill=c("blue", "red"))
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Compare the mean and median with the original data

```r
smean2 <- mean(intervalsteps2)
smedian2 <- median(intervalsteps2)
```


Create a new categorial variable that splits days into if it is 
a weekday or a weekend.

```r
names(activity2)
```

```
## [1] "steps"    "date"     "interval" "day"      "DateTime"
```

```r
activity2$dayType <- ifelse(activity2$day %in% c("Saturday", "Sunday"),
                            "Weekend", "Weekday")
```

Now we need to create a time series plot using day type as a factor 
in order to compare steps on weekdays and weekends.

```r
library(lattice)
library(plyr)

interval2 <- ddply(activity2, .(interval, dayType), summarize, Avg = mean(steps))
xyplot(Avg~interval|dayType, data=interval2, type="l",  
       layout = c(1,2), xlab="Interval", ylab = "Average Number of Steps",
       main = "Average Steps Per Interval")
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
