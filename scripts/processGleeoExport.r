# this script takes as input `export_all.csv` from Gleeo
# and returns an R object ready for another script to do an analysis on it

library(dplyr)
library(lubridate)

setwd("~/Dropbox/proj/qgs/data/phase2/")

options(stringsAsFactors=FALSE)

# load in the data

gleeo.df <- read.csv(file = "export_all.csv", header = TRUE )

# add a date, week and day column
gleeo.df$date <- gleeo.df$Start.Date
gleeo.df$week <- week(gleeo.df$date)
gleeo.df$day <- wday(gleeo.df$date, label = TRUE)
gleeo.df$month <- month(gleeo.df$date, label = TRUE)

##########################################
# Making a day.df to store data about each day

 # getting the totalTaskTime, or the productive time per day
day.df <- gleeo.df %>% group_by(date) %>% filter(Project != "meta", Task != "campustime") %>%
    summarize(totalTaskTime = sum(Decimal.Duration))

# select task time that is within the campus times
 # making a df of the campus times
campus.df <- gleeo.df %>% filter(Project == "meta", Task == "campustime")

# making an interval for the campus time 
intrcamp <- interval(
    ymd_hm(paste(campus.df$date, campus.df$Start.Time)),
    ymd_hm(paste(campus.df$date, campus.df$End.Time))
)

# making an interval for the task times
intrtask <- interval(
    ymd_hm(paste(gleeo.df$Start.Date, gleeo.df$Start.Time)),
    ymd_hm(paste(gleeo.df$End.Date, gleeo.df$End.Time))
    )

# finding the times that were logged while I was at campus
taskAtCampus <- int_overlaps(intrtask, intrcamp)

# totaling this time as campusTask

# add in campus time to the day.df

camptime <- campus.df %>% group_by(date) %>%
    summarize(campusTime = sum(Decimal.Duration))
day.df <- merge(day.df, camptime, by = "date")

# calculate efficiency
# day.df$efficiency <- day.df$campusTask / day.df$campusTime


# getting the task time, or the productive time per day
day.df <- gleeo.df %>% group_by(date) %>% filter(Project != "meta", Task != "campustime") %>%
    summarize(taskTime = sum(Decimal.Duration))

# grouping by date allows me to lump campus times that were broken into periods in a single day
campus.df <- gleeo.df %>% filter(Project == "meta", Task == "campustime") %>% 
    group_by(date) %>% 
    summarize(campustime = sum(Decimal.Duration))

catch <- merge(x = day.df,
                    y = campus.df,
                    by = date)

catch <- merge(x = day.df, y = campus.df, by = "date")


