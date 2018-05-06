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


meetings <- read_csv("./raw/meetings.csv")

dim(meetings)
str(meetings)
head(meetings)
glimpse(meetings)


# set column names. Subject had a unicode character in it, this will help solve
# the issue
colnames(meetings) <- c("Subject","Start",  "End",    "Duration")



# dates are in the format of "Fri 27/04/2018 2:00 PM", this will be parsed to a
# date time, and them team duration in minutes will be calculated as the
# difference in time between end and start time.
# also remove the week day name from the string first

# date1 <- "Sat 27/04/2018 2:00 PM"
# grepl("Fri", date1)
# gsub(pattern = "Mon |Tue |Wed |Thu |Fri |Sat |Sun ", "", meetings$Start)
# lubridate::parse_date_time2("Fri 27/04/2018 2:00 PM", "admyHMp")


meetings$Start <- gsub(pattern = "Mon |Tue |Wed |Thu |Fri |Sat |Sun ", "", meetings$Start)
meetings$End <- gsub(pattern = "Mon |Tue |Wed |Thu |Fri |Sat |Sun ", "", meetings$End)

meetings$Start <- parse_date_time(meetings$Start, "dmyHMp",  tz ="Australia/Sydney")
meetings$End <- parse_date_time(meetings$End, "dmyHMp",  tz ="Australia/Sydney")



# check and remove meetings that out of range 
range(meetings$Start)
range(meetings$End)

meetings <- meetings %>% 
  filter(Start >= as.Date("2018-04-01") & Start <= as.Date("2018-04-30")) %>% 
  filter(End >= as.Date("2018-04-01") & End <= as.Date("2018-04-30"))

# events in the calendar include types other than meetings, e.g birthday, rent
# and anniversary reminders. Cancelled meetings also need to be removed, and
# events with the term WWC are not meetings that i attend so they also need to
# be removed
reg_list <- "bday|birthday|^rent|anniversary|wwc|^canceled"
meetings <- meetings %>% filter(!str_detect(Subject, reg_list))



meetings$DurationMin <- as.numeric(difftime(meetings$End, meetings$Start, units = "mins" ))
# outliers are found as events that are longer than 24 hrs (1440 mins), outliers
# will be removed
summary(meetings$DurationMin)
hist(meetings$DurationMin)
meetings <- meetings %>% filter(!DurationMin >= 1440)
hist(meetings$DurationMin)
# noticed events with duration > 300 
meetings %>% filter(DurationMin > 300) %>% select(Subject, DurationMin)
# items with duration 360 turned to be all place holdesrs, not meetings so i
# will remove them, one item with duration 540 is a "resource assignment" this
# is also not a meeting, and any item with duration == 0 will also be discarded
meetings <- meetings %>% filter(!(DurationMin == 360 | DurationMin == 540 | DurationMin == 0))
hist(meetings$DurationMin)
range(meetings$DurationMin)
summary(meetings$DurationMin)
# meetings %>% filter(DurationMin < 30) check for dates that outside of date
# range, start < Apr 1 or > Apr 31
meetings %>% filter(Start > dmy("30/04/2018"))



### data cleanup is done for now. 

# now i want to explore the data, so first i will see how many meetings i was
# having a day, this will be a count on number of rows grouped by day of month

meetings$WeekDay <- factor(weekdays(meetings$Start), 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) 
meetings$Day <- day(meetings$Start)
meetings$WeekDay
meetings$Day
hist(meetings$Day)
barchart(meetings$WeekDay)
ggplot(meetings, aes(x=WeekDay)) + geom_bar() + scale_fill_discrete(drop=FALSE) + scale_x_discrete(drop=FALSE)


# meeting over days of month 

# prepare labels for x axis
rr <- range(meetings$Start)
labels <- data_frame(Date = seq.Date(as.Date(rr[1]), as.Date(rr[2])+1, by="day"))
labels <- labels %>% mutate(Day = day(Date), WeekDay = weekdays(Date))
# labels <- meetings %>% group_by(Day) %>% mutate(DD = first(WeekDay)) %>% select(Day, DD) %>% ungroup() %>% distinct()

ggplot(meetings, aes(x=Day)) + geom_bar() +
  scale_x_continuous(breaks=labels$Day, labels=labels$WeekDay) +
  theme(axis.text.x = element_text(angle=90)) +
  scale_y_discrete(limits=0:50)
  

