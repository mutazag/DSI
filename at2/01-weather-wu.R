#### weather data - processing #### 


setwd("C:/mdsi/dsi/at2")


library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

list.files("./raw")
weather <- read_csv("./raw/syd_weather_apr_wu_csv.csv",
                    skip = 1,
                    locale=readr::locale(tz="Australia/Sydney"))


#### prep and clean weather data #### 
weather$Date <- dmy(weather$Date)
range(weather$Date)

weather$Day <- weather$Apr
weather$Apr <- NULL 

#### weekday columns ### 
# day of the week as numeric (Monday is 1)
weather$weekdayno <- as.numeric(format(weather$Date, format = "%u"))
weather$weekday <- weekdays(weather$Date)

#### save weather to file ####

write_csv(weather, "weather_summary_clean.csv")

