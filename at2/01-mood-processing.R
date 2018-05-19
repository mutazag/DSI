#### mood data - processing #### 


setwd("C:/mdsi/dsi/at2")


library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)


mood <- read_csv("./raw/group_mood_2018.05.13.csv",locale=readr::locale(tz="Australia/Sydney"))
range(sleep$end)

dataset_columns <- c("id","userid","year","date","weekday","time","mood","activities",
                    "note","key.phrase","sentiment.score","keyphrase.delimited")

colnames(mood) <- dataset_columns
mood <- mood[,dataset_columns]
mood <- mood[!is.na(mood$userid),]

mood$mood <- factor(mood$mood, 
                       
                       levels = c("awful","bad", "meh", "good", "rad"), 
                       ordered = TRUE)
week.days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mood$userid <- factor(mood$userid)
mood$weekday <- factor(mood$weekday, levels = week.days, ordered = T)
mood %>% ggplot(aes(weekday, fill = mood)) + geom_bar()+ 
  scale_x_discrete(labels = week.days)


mood %>% ggplot(aes(mood, fill = weekdays)) + geom_bar()+
  scale_x_discrete()
mood %>% ggplot(aes(userid, fill=mood) )+ geom_bar()
mood %>% ggplot( aes(mood, fill=userid))+ geom_bar()

mood %>% group_by(mood) %>% summarise(N = n(), sentiment = mean(sentiment.score)) -> mood_summary
mid <- mean(mood$sentiment.score)
mood_summary %>% ggplot(aes(x=mood, y=N, fill=sentiment)) + geom_bar(stat="identity") +
  scale_fill_gradientn(colors = rainbow(5))
  # scale_fill_gradient2(midpoint = mid, low="red", mid = "purple", high="orange")
mood_summary %>% ggplot(aes(x=mood, y=sentiment,fill=N)) + geom_bar(stat="identity") +
  scale_fill_gradientn(colors = rainbow(5))

mood %>% 
  arrange(mood) %>%
  group_by(mood) %>%
  mutate(sentiment.mean = mean(sentiment.score)) %>% 
  ggplot(aes(x=mood, y=sentiment.score, fill=sentiment.mean)) +
  geom_boxplot() +
  scale_fill_gradientn(colors = rainbow(5)) 


mood_summary %>% ggplot(aes(x=mood, y=sentiment, fill=sentiment)) + 
  geom_text(aes(label=N),hjust=0, vjust=-2)+ # label text is confisusing 
  scale_y_continuous(breaks = 0:1, limits = 0:1) +
  geom_bar(stat="identity")  +
  scale_fill_gradientn(colors = rainbow(5)) +
  theme_minimal()

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


## summarise mood by user by day 
# example 
# day 1, user 1 
# rad = 2
# good = 4
# rad.score -> 2 / 6 = .33
# good.score -> 4/6 = .33
# <mood> = count how many times a mood was record in a day  
# <mood>.score = <mood> / sum(<mood>s)