#### mood data - processing #### 


setwd("C:/mdsi/dsi/at2")


library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

#### read mode file ####
mood <- read_csv("./raw/group_mood_2018.05.13.csv",locale=readr::locale(tz="Australia/Sydney"))
range(sleep$end)


#### data set prep ####
dataset_columns <- c("id","userid","year","date","weekday","time","mood","activities",
                    "note","key.phrase","sentiment.score","keyphrase.delimited")

colnames(mood) <- dataset_columns
mood <- mood[,dataset_columns]
mood <- mood[!is.na(mood$userid),]

  mood$id <- NULL

mood$mood <- factor(mood$mood, 
                       levels = c("awful","bad", "meh", "good", "rad"), 
                       ordered = TRUE)

week.days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mood$userid <- factor(mood$userid)
mood$weekday <- factor(mood$weekday, levels = week.days, ordered = T)

mood$time <- gsub("[.]",":",mood$time)


#### missing values #### 

#### check the fields with missing data 
mood_missing <- lapply(colnames(mood), function(c){

  no_missing <- sum(is.na(mood[,c]))
  ratio_missing <- no_missing / nrow(mood)
  return(data.frame(colname=c, missing = no_missing, ration = ratio_missing))
}) 

as.data.frame.list(mood_missing)
matrix(mood_missing)
summary(mood)
sum(is.na(mood$userid)) / nrow(sleep)


#### number of mood entries ####
mood %>% ggplot(aes(weekday, fill = mood)) + geom_bar()+ 
  scale_x_discrete(labels = week.days)

mood %>% ggplot(aes(userid, fill=mood) )+ geom_bar()

# what are most common mood entries
mood %>% ggplot( aes(mood, fill=userid))+ geom_bar()


#what is the sentiment that goes with moods (average)
mood %>% group_by(mood) %>% summarise(N = n(), sentiment = mean(sentiment.score)) -> mood_summary


mid <- mean(mood$sentiment.score)
mood_summary %>% ggplot(aes(x=mood, y=N, fill=sentiment)) + geom_bar(stat="identity") +
  scale_fill_gradientn(colors = rainbow(5))
  # scale_fill_gradient2(midpoint = mid, low="red", mid = "purple", high="orange")
# mood_summary %>% ggplot(aes(x=mood, y=sentiment,fill=N)) + geom_bar(stat="identity") +
#   scale_fill_gradientn(colors = rainbow(5))
# 

#### mood sentitment mean box plot ####


ggplot(aes(x=WeekDay, y=slp.quality, fill=slp.quality.weekday.mean)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  labs( title = "What is the sleep quality over the week days?", 
        subtitle = "Night sleep only",
        x = "Day of Week", 
        y = "Sleep Quality", 
        fill = "Avg Sleep Quality")+theme_minimal() +
  scale_fill_gradient(low = "red",high = "green") + 
  scale_y_continuous(labels = scales::percent)

mood <- mood %>% 
  arrange(mood) %>%
  group_by(mood) %>%
  mutate(sentiment.mean = mean(sentiment.score))

mood_sentimentmean <- function(mood){
  mood %>% 
    ggplot(aes(x=mood, y=sentiment.score, fill=sentiment.mean)) +
    geom_boxplot() + theme_minimal() + 
    scale_fill_gradientn(colors = rainbow(5)) + 
    stat_summary(fun.y=mean, geom="point", shape="O", size=4) + 
    scale_y_continuous(labels = scales::percent) +
    labs( title = "Does the sentiment in the notes match the users' mood?", 
          x = "Mood", 
          y = "Sentiment", 
          fill = "Avg Sentiment") -> p
  
  return(p)
}



mood %>% mood_sentimentmean() + 
  labs(caption = "group data")

mood %>% filter(userid==6) %>% 
  mood_sentimentmean() + 
  labs(caption = "personal data")



#### word clouds ####
# mood_summary %>% ggplot(aes(x=mood, y=sentiment, fill=sentiment)) + 
#   geom_text(aes(label=N),hjust=0, vjust=-2)+ # label text is confisusing 
#   scale_y_continuous(breaks = 0:1, limits = 0:1) +
#   geom_bar(stat="identity")  +
#   scale_fill_gradientn(colors = rainbow(5)) +
#   theme_minimal()

library(wordcloud)
library(tm)
phrases <- list(
 rad = unlist(lapply(mood$keyphrase.delimited[mood$mood == "rad"], function(x){ unlist(strsplit(x, " | ", fixed = T ))})),
 good = unlist(lapply(mood$keyphrase.delimited[mood$mood == "good"], function(x){ unlist(strsplit(x, " | ", fixed = T ))})),
 meh = unlist(lapply(mood$keyphrase.delimited[mood$mood == "meh"], function(x){ unlist(strsplit(x, " | ", fixed = T ))})), 
 bad = unlist(lapply(mood$keyphrase.delimited[mood$mood == "bad"], function(x){ unlist(strsplit(x, " | ", fixed = T ))})), 
 awful =  unlist(lapply(mood$keyphrase.delimited[mood$mood == "awful"], function(x){ unlist(strsplit(x, " | ", fixed = T ))}))
)

stopwords= c ("day", "week","mood", "good")
for (i in 1:length(phrases)) {
  w <- tolower(phrases[[i]])
  w <- removeWords(w, stopwords)
  w <- trimws(w)
  w <- tm::stripWhitespace(w)
  w<- tm::removePunctuation(w)
  w <- w[!(is.na(w)  | w == "")]
  # print(w)
  # print(l)
  w <- table(w)
  phrases[[i]] <- w

}

set.seed(42)
wordcloud(words = names(phrases$rad), as.numeric(phrases$rad), min.freq = 1)
wordcloud(words = names(phrases$good), as.numeric(phrases$good), min.freq = 1)
wordcloud(words = names(phrases$meh), as.numeric(phrases$meh), min.freq = 1)
wordcloud(words = names(phrases$bad), as.numeric(phrases$bad), min.freq = 1)
wordcloud(words = names(phrases$awful), as.numeric(phrases$awful), min.freq = 1)


mood$date <- dmy(paste(mood$date, "2018"))

write_csv(mood, "mood_clean.csv")

## summarise mood by user by day 
# example 
# day 1, user 1 
# rad = 2
# good = 4
# rad.score -> 2 / 6 = .33
# good.score -> 4/6 = .33
# <mood> = count how many times a mood was record in a day  
# <mood>.score = <mood> / sum(<mood>s)

