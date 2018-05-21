#### combine data sets #### 


#### header #### 



setwd("C:/mdsi/dsi/at2")


library(readr)
library(dplyr)
library(lubridate)


#### read data files ####

# start with the normalised data files, 1 entry per user per day, 1 entry per
# day for weather
sleep <- read_csv("./sleep_adj.csv",locale=readr::locale(tz="Australia/Sydney"))
mood <- read_csv("./mood_adj.csv", locale=readr::locale(tz="Australia/Sydney"))
meetings <- read_csv("./meetings_summary.csv", locale=readr::locale(tz="Australia/Sydney"))
weather <- read_csv("./weather_summary_clean.csv",locale=readr::locale(tz="Australia/Sydney") )


#### normalise data sets #### 

#reduce data sets to important columns only, and rename all date fields to date
#(lowercase)


sleep <- sleep %>% 
  rename(date = day, 
         weekday = WeekDay,
         slp.start = start,
         slp.end = end)

mood <- mood %>% 
  select(-N, -mood.mean) %>% 
  rename(mood.score = mood.rounded)

meetings <- meetings %>% 
  mutate(date = as.Date(FirstStart)) %>% 
  rename(number.meetings = N) %>% 
  select(-WeekDay - Day)

weather <- weather %>% 
  select(date = Date,
         temp.high, 
         temp.low,
         humidity.high, 
         precip.sum, 
         weather.event = events)


#### combine group data ####
group_df <- left_join(sleep, mood, c("userid", "date")) 
group_df <- left_join(group_df, weather, c("date"))


#### combine personal data ####
personal_df <- group_df %>% filter(userid==6)
personal_df <- left_join(personal_df, meetings, "date")


#### save files to disk ####
write_csv(group_df, "./combined_group.csv")
write_csv(personal_df, "./combined_personal.csv")


