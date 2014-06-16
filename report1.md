Quantified Grad Student Analysis
========================================================





```r
opts_chunk$set(tidy = FALSE, fig.height = 4, cache = TRUE, cache.path = "./.knitrcache_rep1/")
options(stringsAsFactors = FALSE)
```


## Loading Packages

```r
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
library(ggplot2)
```


## Outline
I'll be working with two datasets:
- `clock`: which is when I come into campus, and when I leave
- `gleeo`: which is the tracked time I spent on different tasks throughout each day

In this analysis I am going to:

1. load in the data
 - add in a week count to gleeo and clock
 - add the duration of campustime in clock

2. Answer the question: *How much time do I invest in Grad School?*, which can be broken down into
 - how much time do I spend on campus, per day / week
  - this is the clock time
 - how much time am I working on grad school stuff, per day / week
  - this is the gleeo tracked time
 - What percent of time that I am on campus am I actually getting work done?
  - How efficient am I? (account for time worked not at campus)
 - At the end I want to see "How many hours do I work a week", so that has to be one output.
 
 
 


## import the data

```r
gleeo.df <- read.csv(file="2014-05-31_GleeoExport.csv")
clock.df <- read.csv(file="2014-06-15_clocktime.csv")
# Renaming gleeo.df$Start.Date as date for ease of use
names(gleeo.df)[4] <- "date"
```



## Modify the data
### adding week count to `gleeo.df` and `clock.df`
I want to add in a week count, weeks start on Sunday. The reason for this is that I tend to think in week long work blocks.


```r
# saturday
format(as.Date("2014-06-7"), "%U")
```

```
## [1] "22"
```

```r
# sunday
format(as.Date("2014-06-8"), "%U")
```

```
## [1] "23"
```

So, apparently I can set something as a date, and then use the format function to get the week number. It seems that the week starts by default on a Sunday, so that's neat.

since my times don't span over a day, I only need to worry about one of the dates in my `gleeo` data frame


```r
gleeo.df$week <- format(ymd(gleeo.df$date), "%U")
clock.df$week <- format(ymd(clock.df$date), "%U")
```


> I had the date in clocktime set as 3/3/2014, which is not the same as 2014-03-03, and hence `as.Date()` had issues with it. So I changed it in my google docs. Also it's nice to have clock and gleeo have the same date format.

### Calculating time stuff
Go Go Lubridate

### Adding the weekday to `gleeo.df` and `clock.df`


```r
gleeo.df$day <- wday(ymd(gleeo.df$date), label=TRUE)
clock.df$day <- wday(ymd(clock.df$date), label=TRUE)
```



### Trimming extra days from the `clock.df`
Turns out I had an extra week in the beginning, and then a couple days at the end. So I'm going to trim those dates in `clock.df` to only those that are tracked in `gleeo.df`


```r
clock.df <- clock.df[ymd(clock.df$date) %in% ymd(gleeo.df$date), ]
```


### Getting Tracked time

Here I'm just going to generate a lot of values, and deal with organizing them at the end.


```r
# Adding campustime
clock.df$campustime <- interval(
         ymd_hms(paste(clock.df$date, clock.df$in_time)),
         ymd_hms(paste(clock.df$date, clock.df$out_time))
         ) / ehours(1)
```



```r
# Adding tracked time per day (trackedtime)
dates <- group_by(gleeo.df, date)
gleeotime <- summarize(dates, tottrackedtime = sum(Decimal.Duration))
names(gleeotime)[1] <- "date"
 ## correcting for personal time
projects <- group_by(gleeo.df, Project, date)
projtimes <- summarize(projects, totime = sum(Decimal.Duration))
perstime <- projtimes[which(projtimes$Project == "personal"), 2:3]
names(perstime)[2] <- "perstime"

# left join keeps all the rows of x, not y
trackedtime <- left_join(x=gleeotime, y=perstime, by="date")
# missing values of perstime are NAs, need to be zeros for further work.
trackedtime$perstime[is.na(trackedtime$perstime)] <- 0

# prodtime, tracked time doing productive things
trackedtime$prodtime <- trackedtime$tottrackedtime - trackedtime$perstime

trackedtime$day <- wday(ymd(trackedtime$date), label=TRUE)
trackedtime$week <- format(ymd(trackedtime$date), "%U")
```




```r
# Adding ctrackedtime
# getting an interval for the time that I was at campus
campusinter <- interval(
         ymd_hms(paste(clock.df$date, clock.df$in_time)),
         ymd_hms(paste(clock.df$date, clock.df$out_time)))
gleeo.df$taskinter <- interval(
         ymd_hm(paste(gleeo.df$date, gleeo.df$Start.Time)),
         ymd_hm(paste(gleeo.df$date, gleeo.df$End.Time)))
# finding which values fall in these intervals
   # gleeo.df$taskinter %within% campusinter[1] 
    # I'll have to revisit this ... I can think of a way to get this
    # info, but it involves a lot of for loops

# Adding lost time at campus (campus - tracked)

```


## How much time do I invest in Grad School?



### how much time do I spend on campus, per day / week
  - this is the clock time

```r
qplot(x=day, y=campustime, data=clock.df, geom=c("boxplot", "point"), fill=day)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-121.png) 

```r
qplot(x=week, y=campustime, data=clock.df, geom=c( "point"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-122.png) 

```r

ggplot(clock.df, aes(x=week, y=campustime, group=1)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth() 
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-123.png) 


How much time do I spend at campus per week?

```r
weekgroup <- group_by(x=clock.df, week)
weekcampus <- summarize(weekgroup, weektime = sum(campustime))
qplot(week, weektime, data=weekcampus)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 



### how much time am I working on grad school stuff, per day / week
  - this is the gleeo tracked time


```r
ggplot(trackedtime, aes(x=week, y=prodtime, group=1)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth() 
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


Productive time per week?

```r
gweekgroup <- group_by(x=trackedtime, week)
prodweek <- summarize(gweekgroup, weekprodtime = sum(prodtime))

catch <- data.frame(campus=weekcampus$weektime, prod=prodweek$weekprodtime  )
ggplot(catch, aes(x=campus, y=prod)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)   # Add linear regression line 
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

```r
                             #  (by default includes 95% confidence region)
```




### What percent of time that I am on campus am I actually getting work done?
  - How efficient am I? (account for time worked not at campus)


### At the end I want to see "How many hours do I work a week", so that has to be one output.

