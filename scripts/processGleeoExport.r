# this script takes as input `export_all.csv` from Gleeo
# and returns an R object ready for another script to do an analysis on it

library(data.table)
library(dplyr)
library(lubridate)

options(stringsAsFactors=FALSE)



setwd("~/Dropbox/proj/qgs/data/phase2/")

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
day.df <- gleeo.df %>% group_by(date) %>%
    filter(Project != "meta", Project != "personal") %>%
    summarize(totalTaskTime = sum(Decimal.Duration))

# select task time that is within the campus times
 # making a df of the campus times
campus.df <- gleeo.df %>% filter(Project == "meta", Task == "campustime")

 # making a df of only the task times
task.df <- gleeo.df %>% filter(Project != "meta", Project != "personal")

# using data.table to do the interval searching
 # see: stackoverflow.com/questions/27725034/

# adding start and end columns to task.df using data.table
task.dt <- setDT(task.df)
task.dt[, `:=`(start = as.POSIXct(paste(Start.Date, Start.Time)),
                      end = as.POSIXct(paste(End.Date, End.Time)))]

# adding start and end columns to campus.df
campus.dt <- setDT(campus.df)
campus.dt[, `:=`(start = as.POSIXct(paste(Start.Date, Start.Time)),
                 end = as.POSIXct(paste(End.Date, End.Time)))]

# setting the data.table key in campus.df
setkey(campus.df, start, end)

# getting the intervals of task time that are in campus time
ovrlps <- !is.na(
    foverlaps(x = task.dt, y= campus.dt, type= "any", which= TRUE)$yid
)

# totaling this time as campusTask
campusTask <- task.df[ovrlps] %>% group_by(date) %>%
    summarize(campusTask = sum(Decimal.Duration))
day.df <- merge(day.df, campusTask, by = "date")

# add in campus time to the day.df
camptime <- campus.df %>% group_by(date) %>%
    summarize(campusTime = sum(Decimal.Duration))
day.df <- merge(day.df, camptime, by = "date")

# finding the amount of personal time per day to correct campus time with
perstime <- gleeo.df %>% filter(Project == "personal")
# have to do the same thing to make sure I'm looking at personal time at campus
pers.dt <- setDT(perstime)
pers.dt[, `:=`(start = as.POSIXct(paste(Start.Date, Start.Time)),
               end = as.POSIXct(paste(End.Date, End.Time)))]
ovrlps <- !is.na(
    foverlaps(x = pers.dt, y= campus.dt, type= "any", which= TRUE)$yid
)

campusPers <- pers.dt[ovrlps] %>% group_by(date) %>%
    summarize(campusPers = sum(Decimal.Duration))
day.df <- merge(day.df, campusPers, by = "date")


# calculate efficiency
day.df$efficiency <- day.df$campusTask / (day.df$campusTime - day.df$campusPers)

# adding in the week, day, month to day.df
 # need to specify the package since data.table has same functions
day.df$week <- lubridate::week(day.df$date)
day.df$day <- lubridate::wday(day.df$date, label = TRUE)
day.df$month <- lubridate::month(day.df$date, label = TRUE)

################################################
# All done with the processing of the data
# saving the data as .RData to bring in with other scripts

save(gleeo.df, file="gleeo.df.RData")
save(day.df, file="day.df.RData")



## ----------------------------------------------

# mkaing intervals from the start and end date stuff
# # making an interval for the campus time 
# intrcamp <- interval(
#     ymd_hm(paste(campus.df$date, campus.df$Start.Time)),
#     ymd_hm(paste(campus.df$date, campus.df$End.Time))
# )
# 
# # making an interval for the task times
# intrtask <- interval(
#     ymd_hm(paste(gleeo.df$Start.Date, gleeo.df$Start.Time)),
#     ymd_hm(paste(gleeo.df$End.Date, gleeo.df$End.Time))
# )
# 
# # finding the times that were logged while I was at campus
# taskAtCampus <- int_overlaps(intrtask, intrcamp)
