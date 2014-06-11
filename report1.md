Quantified Grad Student Analysis
========================================================





```r
options(stringsAsFactors = FALSE)
```


## Loading Packages

```r
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
```




## import the data

```r
gleeo.df <- read.csv(file = "2014-05-31_GleeoExport.csv")
clock.df <- read.csv(file = "2014-06-03_clocktime.csv")
```



## Modify the data
### adding week count to Gleeo
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

So, apparently I can set something as a date, and then use te format function to get the week number. It seems that the week starts by default on a Sunday, so that's neat.

since my times don't span over a day, I only need to worry about one of the dates in my `gleeo` data frame


```r
gleeo.df$week <- format(as.Date(gleeo$Start.Date), "%U")
```

```
## Error: object 'gleeo' not found
```


### Day length in clocktime

I want to add in two parts to clock time, the time that I was there in the day, and then later I'm going to calculate the "unaccounted for time", which is the total gleeo.duration in the day - the total clock time for the day. 

### Personal Time
I've got a lot of personal time tracked, and I don't really care how much time I spent taking naps, lunches, or breaks.... only that later it is going to play a role in how I will quantify how much time I tracked during the day, and how much I didn't = the time that I wasted and was not productive.




## dicking around with dplyr


```r
gleeo_df <- tbl_df(gleeo.df)
gleeo_df <- mutate(gleeo_df, hrDuration = Duration/60)
```

```
## Error: non-numeric argument to binary operator
```

```r

projects <- group_by(x = gleeo.df, Project)
time <- summarize(projects, timetot = sum(Decimal.Duration))
arrange(time, desc(timetot))
```

```
## Source: local data frame [14 x 2]
## 
##     Project timetot
## 1    thesis  56.817
## 2  personal  56.100
## 3   general  48.033
## 4   saponin  45.433
## 5   reading  40.117
## 6  outreach  39.450
## 7   meeting  32.583
## 8   rhizpop  20.017
## 9   hacking  15.683
## 10 teaching  15.650
## 11  seminar  14.083
## 12  funding   8.883
## 13 planning   7.400
## 14    study   0.750
```

