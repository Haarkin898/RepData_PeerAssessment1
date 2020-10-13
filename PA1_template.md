Load libraries needed for the analysis (Quest 1)
------------------------------------------------

            library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

            library(lubridate)

    ## Warning: package 'lubridate' was built under R version 3.6.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

            library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.6.3

read in the data set and assign it to an Object
-----------------------------------------------

            ACM = read.csv("./activity.csv")
            head(ACM, 3)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10

            str(ACM)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

refine the date variable, convert to date class and add as variable
-------------------------------------------------------------------

            A <- as.Date(ACM$date, format = "%Y-%m-%d")
            class(A)

    ## [1] "Date"

            ACM$A <- A
            ACM = rename(ACM, Date = A)
            ACM$date = NULL

Group data by Date and analyze
------------------------------

            ACMbyDate = group_by(ACM, Date)
            ACMsum = summarise(ACMbyDate , steps = sum(steps, na.rm = TRUE) )

Plot histogram of above analysis
--------------------------------

            with(ACMsum, hist(steps, main = "Total number of steps per day", 
                              xlab ="Steps per day"))

![](PA1_template_files/figure-markdown_strict/HistogramACM-1.png)

Determine mean and median steps per day
---------------------------------------

     ACMsum = rename(ACMsum, TotalSteps = steps)
     Mean = mean(ACMsum$TotalSteps)
     Median = median(ACMsum$TotalSteps)

The mean and median of the total number of steps taken per day are
9354.2295082 and 10395 respectively

Average daily activity time series
----------------------------------

            ACMbyInterval = group_by(ACM, interval)
            ACMbyInterval = summarise(ACMbyInterval , steps = mean(steps, 
                                                                   na.rm = TRUE) )

Plot analysis from above
------------------------

            with(ACMbyInterval, plot(interval, steps, type = "l", 
            main = "Average Daily Activity Pattern", xlab = "Time Interval"
                                     , ylab = "Average Steps per Interval"))

![](PA1_template_files/figure-markdown_strict/plot-1.png)

interval with the maximum number of steps
-----------------------------------------

            maxInterval = ACMbyInterval[which.max(ACMbyInterval$steps),]$interval

The interval with the maximum number of steps is 835

In-filling missing values with interval mean
--------------------------------------------

filling with daily mean is unlikely to change the output of the analysis
as this will only return the same number of NA as the original dataset.
The dplyr library was used in this analysis

            sumACM = sum(is.na(ACM))
            
            impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
            
            ACM3 <- ACM
            
            ACM3 = ACM3 %>%
                    group_by(interval) %>%
                    mutate(
                            steps = impute.mean(steps)
                    )
            
            sumACM3 = sum(is.na(ACM3))

The sum of NAs in the original data, ACM, and the infilled data, ACM3,
are 2304 and 0 respectively.

Histogram Plot of refined dataset ACM3 with respect to Total number of Steps per day
------------------------------------------------------------------------------------

Group data by Date and analyze

            ACM3byDate = group_by(ACM3, Date)
            ACM3sum = summarise(ACM3byDate , steps = sum(steps, na.rm = TRUE) )
            Mean2 = mean(ACM3sum$steps)
            Median2 = median(ACM3sum$steps)

The mean and median of the total number of steps taken per day are
1.076618910^{4} and 1.076618910^{4} respectively. This is slightly
different from 9354.2295082 and 10395 from the prior analysis. In this
case the mean and the median are now the same and higher in value. This
process has change the skewness from left skewed to a symmetrical
dataset.

Plot histogram of above analysis
--------------------------------

            with(ACM3sum, hist(steps, main = "Total number of steps per day", 
                              xlab ="Steps per day"))

![](PA1_template_files/figure-markdown_strict/HistogramACM3-1.png)

Time series analysis based on day type i.e. weekday vs weekends
---------------------------------------------------------------

            Day <- weekdays(ACM3$Date)
            ACM3$Day = Day
            ACM3$Daytype <- ifelse(ACM3$Day %in% c("Saturday", "Sunday"), "weekend",
                                   "weekday")
            ACM3$Daytype = factor(ACM3$Daytype)
            
            ACMbyInt = group_by(ACM3, interval, Daytype)
            ACMbyInt = summarize(ACMbyInt, steps = mean(steps))

\#\#Plot

            g <- ggplot(ACMbyInt, aes(x=interval, y=steps)) +
                    geom_line() + facet_grid(Daytype~.) +
                    labs (x = "Time Interval", y = "Average Steps per Interval") +
                    labs (title = "Average Daily Activity Pattern by Day Type")
            g

![](PA1_template_files/figure-markdown_strict/ggplot-1.png)
