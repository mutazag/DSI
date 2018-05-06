# Mutaz Abu Ghazaleh 
# 13184383
# 
# DSI AT2 
# 
# Meeting Data Set


setwd("C:/mdsi/dsi/at2")


library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(tidyverse)
library(stringr)


meetings <- read_csv("meetings.csv")

dim(meetings)
str(meetings)
head(meetings)
glimpse(meetings)


# set column names. Subject had a unicode character in it, this will help solve
# the issue
colnames(meetings) <- c("Subject","Start",  "End",    "Duration")

# events in the calendar include types other than meetings, e.g birthday, rent
# and anniversary reminders. Cancelled meetings also need to be removed, and
# events with the term WWC are not meetings that i attend so they also need to
# be removed
reg_list <- "bday|birthday|^rent|anniversary|wwc|^canceled"
meetings <- meetings %>% filter(!str_detect(Subject, reg_list))

# dates are in the formate of "Fri 27/04/2018 2:00 PM", this will be parsed to a
# date time, and them team duration in minutes will be calculated as the
# difference in time between end and start time.
lubridate::parse_date_time2("Fri 27/04/2018 2:00 PM", "dmdyHMp")
meetings$Start <- parse_date_time2(meetings$Start, "dmdyHMp")
meetings$End <- parse_date_time2(meetings$End, "dmdyHMp")
meetings$DurationMin <- as.numeric(difftime(meetings$End, meetings$Start, units = "mins" ))


# outliers are found as events that are longer than 24 hrs (1440 mins), outliers
# will be removed
summary(meetings$DurationMin)
meetings$DurationMin
ggplot(meetings, aes(as.numeric(DurationMin))) + geom_boxplot()
hist(meetings$DurationMin)
plot(meetings$DurationMin)
meetings <- meetings %>% filter(!DurationMin > 1440)
hist(meetings$DurationMin)
plot(meetings$DurationMin)
# noticed meetings with duration > 300 
meetings %>% filter(DurationMin > 300) %>% select(Subject, DurationMin)
# items with duration 360 turned to be all place holdesrs, not meetings so i will remove them 
meetings <- meetings %>% filter(!(DurationMin == 360 | DurationMin == 540 | DurationMin == 0))
hist(meetings$DurationMin)
plot(meetings$DurationMin)
boxplot(meetings$DurationMin)
range(meetings$DurationMin)
summary(meetings$DurationMin)
meetings %>% filter(DurationMin < 30)
