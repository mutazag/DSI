#### mood data - reduce to summary - 1 entry per user per day #### 


setwd("C:/mdsi/dsi/at2")


library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)


day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mood_levels <- c("awful","bad", "meh", "good", "rad")

#### read mode file ####
mood <- read_csv("./mood_clean.csv",
                 col_types = list(
                   readr::col_integer(), 
                   readr::col_integer(), 
                   readr::col_date(format=""),
                   readr::col_factor(levels = day_names, ordered = T),
                   readr::col_time(format=""), 
                   readr::col_factor(levels = mood_levels, ordered = T), 
                   readr::col_character(), 
                   readr::col_character(), 
                   readr::col_character(), 
                   readr::col_double(), 
                   readr::col_character(), 
                   readr::col_double()))



#### find duplicates ####

mood %>% select(userid, date) %>% duplicated() -> mood_dupe

mood %>% 
  group_by(userid, date) %>% 
  summarise(N = n(), 
            sentiment.score = mean(sentiment.score))
# 16 groups of duplicates 


#### what to do with duplicates ? #### 


## summarise mood by user by day 
# example 
# day 1, user 1 
# rad = 2
# good = 4
# rad.score -> 2 / 6 = .33
# good.score -> 4/6 = .33
# <mood> = count how many times a mood was record in a day  
# <mood>.score = <mood> / sum(<mood>s) 

