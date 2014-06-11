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
gleeo$week <- format(as.Date(gleeo$Start.Date), "%U")
```

```
## Error: object 'gleeo' not found
```


### Day length in clocktime


### Personal Time
I've got a lot of personal time tracked, and I don't really care how much time I spent taking naps, lunches, or breaks.... only that later it is going to play a role in how I will quantify how much time I tracked during the day, and how much I didn't = the time that I wasted and was not productive.




## dicking around with dplyr


```r
gleeo_df <- tbl_df(gleeo)
```

```
## Error: object 'gleeo' not found
```

```r
gleeo_df <- mutate(gleeo_df, hrDuration = Duration/60)
```

```
## Error: object 'gleeo_df' not found
```

```r

projects <- group_by(x = gleeo, Project)
```

```
## Error: object 'gleeo' not found
```

```r
time <- summarize(projects, timetot = sum(Decimal.Duration))
```

```
## Error: object 'projects' not found
```

```r
arrange(time, desc(timetot))
```

```
## Error: no applicable method for 'arrange' applied to an object of class
## "function"
```
