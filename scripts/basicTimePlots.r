library(dplyr)
library(ggplot2)

# loading in the data from processGleeoExport.r
setwd("~/Dropbox/proj/qgs/data/phase2/")
load("day.df.RData")
load("gleeo.df.RData")


############3
#  Efficiency
###################



# plot of average efficiency per week, 

day.df <- day.df %>% group_by(week) %>%
    mutate(weeklyMeanEfficiency = mean(efficiency)) 

day.df %>% ggplot(aes(x = week, y = weeklyMeanEfficiency)) + geom_point()
day.df %>% ggplot(aes(x = week, y = efficiency)) + geom_point(alpha = .5, size = 4)


###############
# Task Time
#####################

# how much stuff am I getting done per week?
day.df <- day.df %>% group_by(week) %>%
    mutate(meanWeeklyTaskTime = mean(totalTaskTime),
           weeklyTaskTime = sum(totalTaskTime))


day.df %>% ggplot(aes(x = week, y = meanWeeklyTaskTime)) +
    geom_point(size = 4)

day.df %>% ggplot(aes(x = week, y = weeklyTaskTime)) +
    geom_point(alpha = .5, size = 4)



# campus time during the week
day.df <- day.df %>% group_by(week) %>%
    filter(day != "Sun", day != "Sat") %>%
    mutate(meanWeeklyCampusTime = mean(campusTime),
           weeklyCampusTime = sum(campusTime))

day.df %>% ggplot(aes(x = week, y = weeklyCampusTime)) +
    geom_point(alpha = .5, size = 4) + 
    geom_point(aes(y = weeklyTaskTime), color= "red", size = 4)







