#### analysis of combined data sets #### 


#### header #### 



setwd("C:/mdsi/dsi/at2")

library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(scales)


#### read data files ####
day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mood_levels <- c("awful","bad", "meh", "good", "rad")

personal <- read_csv("./combined_personal.csv",
                     locale=readr::locale(tz="Australia/Sydney"),
                     col_types = cols(
                       userid = readr::col_factor(NULL),
                       weekday = readr::col_factor(levels = day_names, ordered = T),
                       mood = readr::col_factor(levels = mood_levels, ordered = T)
                     ))
group <- read_csv("./combined_group.csv",
                     locale=readr::locale(tz="Australia/Sydney"),
                     col_types = cols(
                       userid = readr::col_factor(NULL),
                       weekday = readr::col_factor(levels = day_names, ordered = T),
                       mood = readr::col_factor(levels = mood_levels, ordered = T)
                     ))


#### Am i spending more time in meetings on longer working days? ####


personal %>% 
  ggplot(aes(x=LastEndHour -9, y=TotalMeetingTime)) + 
  geom_point(aes(color=mood)) + 
  scale_x_continuous(name = "Day Working Hours", breaks = 0:14)+ 
  scale_y_continuous(name = "Time in Meetings") + 
  theme_minimal() + 
  geom_line(stat="smooth", method="lm", size=.5, linetype = "dashed", alpha=0.5) +
  labs(
    title="Work Hours and Meetings"
  )

personal %>% 
  ggplot(aes(x=date,y=LastEndHour -9, fill=mood)) + 
  geom_bar(stat = "identity", breaks=date_breaks("day")) +
  scale_y_continuous(breaks=0:16) +
  labs(title="How long are my working days", 
       x = "Day Working Hours",
       y = "Count") +
  theme_minimal() 
  

personal %>% 
  ggplot(aes(x=LastEndHour,y=slp.quality)) + 
  geom_point() + geom_smooth(method="auto")

personal %>% 
  ggplot(aes(x=temp.low,y=slp.quality, fill=mood)) + 
  geom_point(aes(color=mood)) 


q_plot <- function(x){ 
  boxplot(x)
  hist(x)
}

# 
# q_plot(personal$slp.quality)
# 
# q_plot(personal$time.in.bed)
# 
# q_plot(personal$sentiment.score)
# 
# q_plot(personal$mood.score)
# 
# q_plot(personal$AvgMeetingDuration)
# 
# q_plot(personal$FirstStartHour)
# 
# q_plot(personal$LastEndHour)
# 
# q_plot(personal$TotalMeetingTime)
# 
# 
# q_plot(personal$mood)


#### personal - relation between pctie in meetings and mood ####


#### sleep Quality #### 
# sleep quality and mood # 
sleepquality_mood <- function(df){
df %>% 
  ggplot(aes(x=mood.score, y=slp.quality)) + 
    theme_light() + 
  geom_point(aes(colour=factor(userid)))+ 
    scale_x_continuous(name = "Mood Score", breaks = 0:14)+ 
    scale_y_continuous(name = "Sleep Quality", labels = scales::percent) + 
    labs(title="Mood and Sleep Qauality",
         color = "User") + 
    geom_line(stat="smooth", 
              method="lm", 
              size=.5, 
              linetype = "dashed", 
              alpha=0.5)  -> p
  return(p)
}

personal %>% 
  sleepquality_mood() + 
  labs(caption = "personal data") 

group %>% 
  sleepquality_mood() + 
  labs(caption = "group data") 
cor(group$mood.score, group$slp.quality)



# low temp and sleep quality 
sleepquality_temp <- function(df){
  df %>% 
    ggplot(aes(x=temp.low, y=slp.quality)) + 
    theme_light() + 
    geom_point(aes(colour=factor(userid)))+ 
    scale_x_continuous(name = "Temperature (celsius) ")+ 
    scale_y_continuous(name = "Sleep Quality", labels = scales::percent) + 
    labs(title="Temperature and Sleep Qauality",
         color = "User") + 
    geom_line(stat="smooth", 
              method="lm", 
              size=.5, 
              linetype = "dashed", 
              alpha=0.5)  -> p
  return(p)
}

personal %>% 
  sleepquality_temp() + 
  labs(caption = "personal data") 

group %>% 
  sleepquality_temp() + 
  labs(caption = "group data") 
cor(group$temp.low, group$slp.quality)

# sleep duration and sleep quality 
sleepduration_quality <- function (df) {
  df %>% 
    ggplot(aes(x=time.in.bed,y=slp.quality)) + 
    theme_minimal() + 
    geom_point(aes(colour=factor(userid)))+ 
    scale_x_continuous(name = "Time in Bed (h)")+ 
    scale_y_continuous(name = "Sleep Quality", labels = scales::percent) + 
    labs(title="Time in Bed and Sleep Quality ",
         color = "User") + 
    geom_line(stat="smooth", 
              method="lm", 
              size=.5, 
              linetype = "dashed", 
              alpha=0.5)  -> p
  return(p)
}


group %>% sleepduration_quality() +
  labs(caption = "group data")

personal %>% sleepduration_quality() + 
  labs(caption = "personal data")


# late meetings, pct time in meetings 


latemeetings_sleepquality <- function (df) {
  df %>% 
    ggplot(aes(x=LastEndHour,y=slp.quality)) + 
    theme_minimal() + 
    geom_point(aes(colour=factor(userid)))+ 
    scale_x_continuous(name = "Last Meetings End")+ 
    scale_y_continuous(name = "Sleep Quality", labels = scales::percent) + 
    labs(title="Work (Last Meeting End) and Sleep Quality ",
         color = "User") + 
    geom_line(stat="smooth", 
              method="lm", 
              size=.5, 
              linetype = "dashed", 
              alpha=0.5)  -> p
  return(p)
}


personal %>% latemeetings_sleepquality() + 
  labs(caption = "personal data")
cor(personal$LastEndHour, personal$slp.quality)





pctTimeInMeeting_sleepquality <- function (df) {
  df %>% 
    ggplot(aes(x=PctTimeInMeetings,y=slp.quality)) + 
    theme_minimal() + 
    geom_point(aes(colour=factor(userid)))+ 
    scale_x_continuous(name = "% time spent in meetings", labels = scales::percent)+ 
    scale_y_continuous(name = "Sleep Quality", labels = scales::percent) + 
    labs(title="Work (% in meetings) and Sleep Quality ",
         color = "User") + 
    geom_line(stat="smooth", 
              method="lm", 
              size=.5, 
              linetype = "dashed", 
              alpha=0.5)  -> p
  return(p)
}


personal %>% pctTimeInMeeting_sleepquality() + 
  labs(caption = "personal data")
cor(personal$PctTimeInMeetings, personal$slp.quality)
