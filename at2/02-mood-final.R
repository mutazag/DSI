#### mood data - reduce to summary - 1 entry per user per day #### 


setwd("C:/mdsi/dsi/at2")


library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)


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

mood %>% select(userid, date,mood,time) %>% duplicated() -> mood_dupe
mood <- mood[-mood_dupe,]


#### handle multoke entried per day by summarising mood using leichhardt scale ####
mood.adj <- mood %>% mutate( mood.score = case_when(
  mood == 'awful' ~ 1,
  mood == 'bad' ~ 2, 
  mood == 'meh' ~ 3, 
  mood == 'good' ~ 4, 
  mood == 'rad' ~ 5
))



mood.adj %>% 
  group_by(userid, date) %>% 
  summarise(N = n(), 
            sentiment.score = mean(sentiment.score),
            mood.mean = mean(mood.score),
            mood.rounded =   round(mood.mean,0),
            mood = case_when(
              round(mood.mean,0) == 1 ~ 'awful',
              round(mood.mean,0) == 2 ~ 'bad',
              round(mood.mean,0) == 3 ~ 'meh',
              round(mood.mean,0) == 4 ~ 'good',
              round(mood.mean,0) == 5 ~ 'rad'
              
            )) -> mood.adj


write_csv(mood.adj, "./mood_adj.csv")

mood.adj %>% filter(userid==1) %>% 
  ggplot(aes(x=date, y=mood.rounded)) + 
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%b %d", 
               expand = c(0,0), 
               minor_breaks = NULL)  +
  scale_y_continuous(minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title = "Mood Entries by Day", 
       
       x = 'Day', 
       y = 'Number of Entries', 
       fill = "User"
  )


