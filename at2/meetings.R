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


# set col1 name to Subject
colnames(meetings) <- c("Subject","Start",  "End",    "Duration")

ex_list <- as.vector(c("bday", "birthday","rent", "anniversary"))
reg_list <- "bday|birthday|^rent|anniversary|wwc|^canceled"
ex_list2 <- c("samer", "rent", "anniversary")
meetings$Subject <- tolower(meetings$Subject)
meetings %>% filter(stringr::str_detect(Subject, ex_list))
meetings %>% filter(Subject %in% "samer") %>% tail()
sublist <- meetings %>% filter(!str_detect(Subject, reg_list))

filter(.data = meetings, stringr::str_detect(meetings$Subject, ex_list))

lubridate::parse_date_time2("Fri 27/04/2018 2:00 PM", "dmdyHMp")
meetings$Start <- parse_date_time2(meetings$Start, "dmdyHMp")
meetings$End <- parse_date_time2(meetings$End, "dmdyHMp")
meetings$DurationMin <- as.numeric(difftime(meetings$End, meetings$Start, units = "mins" ))

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
