#### Summarise mood #### 


setwd("C:/mdsi/dsi/at2")


library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mood_levels <- c("awful","bad", "meh", "good", "rad")

mood <- read_csv("mood_clean.csv", 
                 locale=readr::locale(tz="Australia/Sydney"), 
                 col_types = readr::cols(
                   weekday = readr::col_factor(levels = day_names, ordered = T),
                   mood = readr::col_factor(levels = mood_levels, ordered = T)
                 ))

mood$date <- as.Date(mood$date, tz="Australia/Sydney")
mood$userid<- factor(mood$userid)



#### number of days collected ####
mood_summary <- mood %>% 
  group_by(userid) %>% 
  summarise( N = n(), first = min(date), last=max(date)) %>%
  mutate(days_collected = difftime(last,first, units="days")) %>% 
  ungroup()


# as.difftime(
#   as.character(sleep$time.in.bed), 
#   units = "mins")
#### entries per day ####
mood %>% ggplot(aes(x=date, fill=userid)) + 
  geom_bar() +
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



#### personal mood and activities #### 

mood <- mood %>% mutate( mood.score = case_when(
  mood == 'awful' ~ 1,
  mood == 'bad' ~ 2, 
  mood == 'meh' ~ 3, 
  mood == 'good' ~ 4, 
  mood == 'rad' ~ 5
))


top_moodactivities <- function(mood, uid = 6){
mood %>% filter(userid == uid) %>% 
  group_by(activities) %>% 
  mutate(activity.mood.score = mean(mood.score), 
         activity.sentiment.score = mean(sentiment.score)) %>% 
  filter(!is.na(activities)) %>%
  ggplot(aes(x=reorder(activities, -activity.mood.score), 
             y=activity.mood.score)) + 
  geom_point(shape = 23, size = 4, aes(fill = activity.mood.score)) + 
    theme_light() +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.text.x  = element_text(angle=45, vjust=0.5, size=8), 
        axis.text.y = element_text(angle=0)) + 
  scale_y_continuous( limits = c(0,5), 
                      labels=c("", mood_levels), 
                      minor_breaks = NULL) +
  labs(title="Top mood lifting activities",
       x = "", 
       y = "") -> p
  
  
  # coord_flip() + 
  return(p)
}


mood%>% top_moodactivities(uid = 6)


mood_by_day <- function(mood, uid = 6){
  mood %>% filter(userid == uid) %>% 
    group_by(weekday) %>% 
    mutate(activity.mood.score = mean(mood.score), 
           activity.sentiment.score = mean(sentiment.score)) %>% 
    filter(!is.na(activities)) %>%
    ggplot(aes(x=weekday, 
               y=activity.mood.score)) + 
    geom_point(shape = 23, size = 4, aes(fill = activity.mood.score)) + 
      theme_light() +
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
          axis.text.x  = element_text(angle=45, vjust=0.5, size=8), 
          axis.text.y = element_text(angle=45)) + 
    scale_y_continuous( limits = c(0,5), 
                        labels=c("", mood_levels), 
                        minor_breaks = NULL) +
      labs(title="Daily Mood",
         x = "", 
         y = "") -> p
  
  return(p)
}





mood %>% mood_by_day(uid=6)

