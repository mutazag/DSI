#### Summarise mood #### 


setwd("C:/mdsi/dsi/at2")


library(readr)
library(dplyr)
library(lubridate)


mood <- read_csv("mood_clean.csv")

mood$date <- as.Date(mood$date, tz="Australia/Sydney")
mood$userid<- factor(mood$userid)



#### number of days collected ####
mood_summary <- mood %>% 
  group_by(userid) %>% 
  summarise( N = n(), first = min(date), last=max(date)) %>%
  mutate(days_collected = difftime(last,first, units="days"))
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
