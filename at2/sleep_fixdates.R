### Process Group Data 

setwd("C:/mdsi/dsi/at2")
list.files()

library(readr)
library(dplyr)
library(lubridate)

#### load the sleep data with no NAs ####
sleep <- read_csv("./sleep_noNA.csv")

dim(sleep) 
glimpse(sleep)
summary(sleep)

#### fix dates #### 

# dates will be fixed by handling exceptions where the format is mdy, then after
# that will apply a ymd transform followed by dmy start date
sleep$start2 <- as.POSIXct(NA)
sleep$end2 <- as.POSIXct(NA)


# fix exceptions first, as it turned out, all exceptions are related to user 5
start_date_exceptions <- sleep$userid == 5#81:100
end_date_exceptions <- sleep$userid == 5#80:100

sleep$start2[start_date_exceptions] <-   parse_date_time(
  sleep$start[start_date_exceptions], 
  orders = c("YdmHMS", "mdYHM"), locale = "eng", tz="Australia/Sydney")

sleep$end2[end_date_exceptions] <-   parse_date_time(
  sleep$end[end_date_exceptions], 
  orders = c("YdmHMS", "mdYHM"), locale = "eng", tz="Australia/Sydney")

sleep$start2[start_date_exceptions]
sleep$end2[end_date_exceptions] 

# convert the ymd formatted values 

sleep$start2[!start_date_exceptions] <-   parse_date_time(
  sleep$start[!start_date_exceptions], 
  orders = c("YmdHMS", "YmdHM", "dmYHM"), locale = "eng", tz="Australia/Sydney")

sleep$end2[!end_date_exceptions] <-   parse_date_time(
  sleep$end[!end_date_exceptions], 
  orders = c("YmdHMS", "YmdHM", "dmYHM"), locale = "eng", tz="Australia/Sydney")

# update the start,end fields and drop temporary start2,end2 fields
sleep$start <- sleep$start2
sleep$end <- sleep$end2

sleep$start2 <- NULL
sleep$end2 <- NULL



#### format duration field to use minutes as a UOM 
sleep$time.in.bed <- as.numeric(
  as.difftime(
    as.character(sleep$time.in.bed), 
    units = "mins"))

#### format the skeep quality field to a numberic fraction 
sleep$slp.quality <- as.numeric(sub("%","",sleep$slp.quality))/100
summary(sleep$slp.quality)


write_csv(sleep, "sleep_clean.csv")
