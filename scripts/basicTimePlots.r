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
