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

dim(sleep) 
glimpse(sleep)
summary(sleep)
  
sleep <- sleep %>% 
  select (-user.notes, -comment, -wakeup, -sleep.notes, -heart.rate)



# fix start and end dates
sleep$userid


# group start date by format
a <- "(\\d{4}-\\d{2}-\\d{2} \\d{2}:)((\\d{2}:\\d{2})|(\\d{2}))"
b <- "(\\d/\\d{2}/\\d{4} (\\d{2}|\\d))((\\d{2}:\\d{2})|(:\\d{2}))"
c <- "(\\d\\.\\d{2}\\.\\d{4} (\\d{2}|\\d))((\\d{2}:\\d{2})|(:\\d{2}))"

# A: 2018-03-31 00:43:00 or 2018-03-31 00:43
# B: 29/04/2018 00:17 or 04/29/2018 12:52
# C: 02.04.2018 9:48

d <- "04.23.2018 2:11"

grepl(c,d)
sleep <- sleep %>% mutate( startFormat = case_when(grepl(a,sleep$start)~ "A", 
                                                   grepl(b,sleep$start)~ "B",
                                                   grepl(c,sleep$start)~ "C"))

sleep <- sleep %>% mutate( endFormat = case_when(grepl(a,sleep$end)~ "A", 
                                                 grepl(b,sleep$end)~ "B",
                                                 grepl(c,sleep$end)~ "C"))

a_filter <- which(sleep$startFormat == "A")
b_filter <- which(sleep$startFormat == "B")
c_filter <- which(sleep$startFormat == "C")
sleep[a_filter,]
sleep[b_filter,]
sleep[c_filter,]
sleep %>% filter(startFormat=="A") %>% mutate(nn=1:n()) %>% select(nn)

# dont do the mdy for starter so not to mix up months and days
sleep$start2<-parse_date_time(x = sleep$start,
                orders = c("YmdHMS", "YmdHM"),
                locale = "eng", tz ="Australia/Sydney")

sleep$Start[is.na(sleep$start2)][1:20] ## "dmYHM"
sleep$Start[is.na(sleep$start2)][21:51] ## "mdYHM"


#examine the ones that faild
which(sleep$startFormat == "B")


sleep$start2[is.na(sleep$start2)] <- parse_date_time(sleep$Start[is.na(sleep$start2)], orders = c("YmdHM", "mdYHM"), locale = "eng", tz="Australia/Sydney")
sleep$start2
range(sleep$start2, na.rm = TRUE)
# clean start and End times

tail(sleep[,"Start"])
nrow(sleep)


library(ggplot2)

ggplot(sleep, aes(User)) + geom_bar()
