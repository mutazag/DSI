### Process Group Data 

setwd("C:/mdsi/dsi/at2")
list.files()

library(readr)
library(dplyr)
library(lubridate)


sleep <- read_csv("./raw/group_sleep_data_2018.05.13.csv")
dataset_colnames <- c("userid", "start", "end", "slp.quality", "time.in.bed", "sleep.notes", "user.notes", "comment", "heart.rate", "wakeup")

dim(sleep)
colnames(sleep) <- dataset_colnames


# get rid of extra columns at the end
# sleep[,11:ncol(sleep)] <- NULL
sleep <- sleep[,dataset_colnames]
sleep <- sleep %>% 
  filter(!is.na(sleep$userid)) 


  
sleep <- sleep %>% 
  select (-user.notes, -comment, -wakeup, -sleep.notes, -heart.rate)

dim(sleep) 
glimpse(sleep)
summary(sleep)

write_csv(sleep, "sleep_noNA.csv")
