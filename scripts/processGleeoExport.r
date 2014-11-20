# this script takes as input `export_all.csv` from Gleeo
# and returns an R object ready for another script to do an analysis on it

library(dplyr)

setwd("~/Dropbox/proj/qgs/data/phase2/")


# load in the data

gleeo.df <- read.csv(file = "export_all.csv", header = TRUE )

# add a date column
gleeo.df$date <- gleeo.df$Start.Date


