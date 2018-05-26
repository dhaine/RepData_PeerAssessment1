---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(tidyverse)
library(ggplot2)
activity <- read_csv("activity.zip")
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```

## What is mean total number of steps taken per day?

```r
by_day <- group_by(activity, date)
day_tot <- summarize(by_day, sum_steps = sum(steps, na.rm = TRUE))
summary(day_tot$sum_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

The mean number of steps per day is 9354.2, and
the median is 10395.

Here's the histogram of the total number of steps per day.


```r
ggplot(day_tot, aes(sum_steps)) +
    geom_histogram(bins = 20, colour = "darkblue", fill = "lightblue") +
    labs(x = "Number of steps",
         title = "Total Number of Steps Taken Each Day")
```

![](/home/denis/workspace/06-courses/Data Science Specialization/05_Reproducible research/week2/RepData_PeerAssessment1/PA1_template[exported]_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## What is the average daily activity pattern?

```r
by_interval <- group_by(activity, interval)
intrvl_avg <- summarize(by_interval, avg_steps = mean(steps, na.rm = TRUE))
```


```r
ggplot(intrvl_avg, aes(x = interval, y = avg_steps)) +   
    geom_line() +  
    labs(title = "Average Daily Activity Pattern",
         x = "Interval",
         y = "Average number of steps per interval")
```

![](/home/denis/workspace/06-courses/Data Science Specialization/05_Reproducible research/week2/RepData_PeerAssessment1/PA1_template[exported]_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
intrvl_max <- intrvl_avg[which.max(intrvl_avg$avg_steps), ]
```

The interval that, on average, contains the maximum number of steps is the 
835<sup>th</sup>, with 206 steps.

## Imputing missing values

```r
missing <- sum(is.na(activity$steps))
```

The total number of missing values in the dataset is 2304.

Missing values are replaced by the average for that interval.


```r
activity_imput <- transform(activity,
                            steps = ifelse(is.na(activity$steps),
                                           intrvl_avg$avg_steps[match(activity$interval,
                                                                      intrvl_avg$interval)],
                                           activity$steps))
```


```r
by_day_imput <- group_by(activity_imput, date)
day_tot_imput <- summarize(by_day, sum_steps = sum(steps, na.rm = TRUE))
ggplot(day_tot_imput, aes(sum_steps)) +
    geom_histogram(bins = 20, colour = "darkblue", fill = "lightblue") +
    labs(x = "Number of steps",
         title = "Total Number of Steps Taken Each Day",
         subtitle = "After imputation of missing values")
```

![](/home/denis/workspace/06-courses/Data Science Specialization/05_Reproducible research/week2/RepData_PeerAssessment1/PA1_template[exported]_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
library(knitr)
kable(matrix(c(mean(day_tot$sum_steps), median(day_tot$sum_steps),
                mean(day_tot_imput$sum_steps), median(day_tot_imput$sum_steps)),
             nrow = 2, dimnames = list(c("mean", "median"), c("raw", "imputed")),
             byrow = FALSE))
```

               raw    imputed
-------  ---------  ---------
mean       9354.23    9354.23
median    10395.00   10395.00

Mean and median values are the same in raw and imputed datasets.

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
activity_imput$day <- as.factor(with(activity_imput,
                                     ifelse(wday(date, label = TRUE) %in%
                                            c("sam", "dim"),
                                            "weekend",
                                            "weekday")))
```


```r
by_interval <- group_by(activity_imput, interval, day)
intrvl_avg <- summarize(by_interval, avg_steps = mean(steps, na.rm = TRUE))
```


```r
ggplot(intrvl_avg, aes(x = interval, y = avg_steps)) +   
    geom_line() +
    facet_wrap(~ day, nrow = 2) +
    labs(title = "Average Daily Activity Pattern",
         x = "Interval",
         y = "Number of steps")
```

![](/home/denis/workspace/06-courses/Data Science Specialization/05_Reproducible research/week2/RepData_PeerAssessment1/PA1_template[exported]_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
