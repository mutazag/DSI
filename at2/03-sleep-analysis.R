#### Summarise Sleep Data #### 


setwd("C:/mdsi/dsi/at2")


library(readr)
library(dplyr)
library(lubridate)

sleep <- read_csv("sleep_clean.csv",locale=readr::locale(tz="Australia/Sydney"))
range(sleep$end)


#### test for duplicates ####

sleep %>% duplicated() %>% sum()

# 1 duplicate was found. 

# remove duplicates, add a column for record day, use the sleep end time to calucate the record day
df <- sleep %>% 
  unique() %>%
  mutate( day = as.Date(end,tz="Australia/Sydney") ) %>% arrange(userid,day)

df$userid <- as.factor(df$userid)

#### observations ####
# users with more than one sleep entry per day 

df_summary <- df %>% 
  group_by(userid,day) %>% 
  summarise(N = n()) %>% 
  ungroup()

range(df_summary$day)
df %>% ggplot(aes(x=day, fill=userid)) + 
  geom_bar() +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%b %d", 
               expand = c(0,0), 
               minor_breaks = NULL)  +
  scale_y_continuous(minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title = "Sleep Entries by Day", 
       subtitle ="group by User", 
       x = 'Day', 
       y = 'Number of Entries', 
       fill = "User"
  )
# most users started collecting data regularly on Apr 01, everyone stopped
# collecting data afer Apr 30, few users collected multiple entries per day. We
# will need to go deeper in the data to deermine how to handle the multiple
# entries. some of the conditions to check for are whether those entries were
# captured for sleep during the day, in such case the entries may be discarded.
# also checking if there was duplicate entries




#### bed time ####

# when do users go to bed, rounded down to the closest 1 hr

df <- df %>% mutate(go.to.bed = hour(start), wake.up = hour(end))
# hour(start + hours(0))-0)
df %>% ggplot(aes(go.to.bed)) + geom_bar()

df <- df %>% mutate(night.sleep = case_when(
  (go.to.bed >=9 & go.to.bed <= 17) ~ FALSE, 
  TRUE~ TRUE)) 

df %>% ggplot(aes(x = go.to.bed, fill=userid)) + 
  geom_bar(position = "stack") + ylim(0,80) +
  scale_x_continuous(breaks = 0:23, 
                     labels = 0:23, 
                     minor_breaks = NULL, limits = c(-1,24)) + 
  labs( title = "When do people go to bed?", 
        subtitle = "count of sleep start by hour", 
        caption = paste0("..."), 
        x = "Go to bed time (hr)", 
        y = "Number of records", 
        fill = "User")+theme_minimal()

df %>% ggplot(aes(x = wake.up, fill=userid)) + 
  geom_bar(position = "stack") + ylim(0,80) + 
  scale_x_continuous(breaks = 0:23, 
                     labels = 0:23, 
                     minor_breaks = NULL, limits = c(-1,24)) + 
   labs( title = "And what time do they wake up?", 
        subtitle = "count of sleep end by hour", 
        x = "Wake up time (hr)", 
        y = "Number of records", 
        fill = "User")+theme_minimal()

boxplot(df_gotobed$go.to.bed)

## most of the recorded sleep sessions sarted at 20:00, some users went to sleep
## as late as 3 and 4 am, will be interesting if there was any special reason
## for user 6 to go to bed at 4 am


#### day nap?? ####


#### why did user 6 stay up late till 4 am? ####
df %>% filter(go.to.bed == 4 )
# two entries
# user 6 went to bed after four on the morning of Friday Apr 20. The night before was the deadline to submit DSI assignment, and I worked all night into the next morning to turn it. i did submit it at 4 am the on Friday, 4 hrs later fater the deadline. 

df %>% filter(userid==6, go.to.bed == 4) 


#### save summary sleep data ####
 write_csv(df, "./sleep_summary.csv")