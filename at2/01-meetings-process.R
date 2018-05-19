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
# library(tidyverse)
library(stringr)
library(ggplot2)

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

meetings %>% ggplot(aes(x=day(Start), y=DurationMin, colour =DurationMin)) +
  geom_point(position="jitter") + 
  scale_x_continuous(breaks = 1:30)#, labels = 1:30, minor_breaks = NULL)

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
meetings %>% filter(Start < dmy("01/04/2018"))
meetings %>% filter(End > dmy("30/04/2018"))

# remove meetings that start after midnight and finish before 7am
meetings <- meetings %>% filter( !(hour(Start) >= 0 & hour(End) <= 7))
meetings %>% filter(hour(Start) <= 1)



### data cleanup is done for now. 

# now i want to explore the data, so first i will see how many meetings i was
# having a day, this will be a count on number of rows grouped by day of month
day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
meetings$WeekDay <- factor(weekdays(meetings$Start), 
                              levels = day_names) 
meetings$Day <- day(meetings$Start)
meetings$WeekDay
meetings$Day
hist(meetings$Day)

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
  
#### summarise meetings ####

meetings %>% group_by(Day, WeekDay) %>% 
  summarise(N = n(), 
            FirstStart = min(Start), 
            FirstStartHour = hour(FirstStart), 
            LastEnd = max(End),
            LastEndHour = hour(LastEnd),
            AvgMeetingDuration = mean(DurationMin),
            TotalMeetingTime = sum(DurationMin),
            WorkingDayDuration = as.numeric(difftime(LastEnd, FirstStart, units = "mins" ))) %>% 
  ungroup() %>% 
  mutate(PctTimeInMeetings = TotalMeetingTime / max(TotalMeetingTime))-> meetings_summary

meetings_summary %>% 
  ggplot(aes(x=WeekDay, y=PctTimeInMeetings)) + 
  geom_boxplot()
  geom_bar(stat="identity")


write_csv(meetings_summary, "meetings_summary.csv")


#### meetings observations ####
boxplot(meetings_summary$TotalMeetingTime)
summary(meetings_summary$TotalMeetingTime)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 30.0   142.5   195.0   243.8   371.2   540.0 
# 75% of the of the days is spent in meetings longer than 2:20 hrs to 6.20 hrs
# this for days where i recoreded metings 
summary(meetings_summary$N)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    2.00    3.00    3.65    5.00    9.00 
# on working days i will have on average 4 meetings
# with most of the days (25% to 75%) (read how to interpet summary)
# i will have between 2 to five meetings

# to reduce times in meetings (to improve productivity) i need to target few changes: 
# start by participating in shorter meetings and less meetings in a day

summary(meetings_summary$FirstStartHour)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.00    9.75   10.00   10.95 11.25      16.00 
# 1st qut start hour is 9:45 and median start is 10:00, this makes
# sense as i usually need to drop the kids to school and come back home by 9 am
# to start work. hence most of my meetings in the morning will only start after
# 9am

summary(meetings_summary$LastEndHour)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.0    15.0    16.5    16.9    18.0    21.0 

# on avg i seem to wrap up my meetings at 4:30, with 3rd qu of 18:00 (what does
# 3rd qut mean) there are instances where my meetings wrapped up at 9pm those
# are mostly days when i had to attend evening classes
