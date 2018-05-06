### Process Group Data 

setwd("C:/mdsi/dsi/at2")
list.files()

library(readr)
library(dplyr)
library(lubridate)


sleep <- read_csv("./raw/group_2018.05.03_sleep.csv")

dim(sleep)
colnames(sleep)
glimpse(sleep)
summary(sleep)

# get rid of extra columns at the end
sleep[,11:ncol(sleep)] <- NULL



# trip the tail of sleep where user is null 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   3.000   4.000   3.894   5.000   6.000     897 
summary(sleep$User)
sleep <- subset(sleep[!is.na(sleep$User),]) 

# fix start and end dates
sleep$Start

# dont do the mdy for starter so not to mix up months and days
sleep$start2<-parse_date_time(x = sleep$Start,
                orders = c("YmdHMS", "YmdHM"),
                locale = "eng", tz ="Australia/Sydney")

sleep$Start[is.na(sleep$start2)][1:20] ## "dmYHM"
sleep$Start[is.na(sleep$start2)][21:51] ## "mdYHM"
#examine the ones that faild
sleep$start2[is.na(sleep$start2)] <- parse_date_time(sleep$Start[is.na(sleep$start2)], orders = c("YmdHM", "mdYHM"), locale = "eng", tz="Australia/Sydney")
sleep$start2
range(sleep$start2, na.rm = TRUE)
# clean start and End times

tail(sleep[,"Start"])
nrow(sleep)


library(ggplot2)

ggplot(sleep, aes(User)) + geom_bar()
