

data <- read.csv("a - sheet1.csv", stringsAsFactors = FALSE, header = TRUE)

str(data)


names <- c("user", "date", "dayOfWeek", "Steps", "Walk.Time", "KM", "Dest", "Water", "Sleep.Time", "Sleep.Quality", "Comments")
colnames(data) <- names


str(data)


# data$dayOfWeek = as.factor(data$dayOfWeek, levels
#install.packages("ggplot2")
library(ggplot2)


# facet the plot by user, and color by cut 
ggplot(data = data, 
       aes(x = Walk.Time, y = Steps, color = user)) + geom_point() +
geom_smooth(method="auto")   + ggtitle("Walk time and Steps ")

ggplot(data = data, 
       aes(x = Steps, y = Sleep.Quality, color = user)) + geom_point() +
  geom_smooth(method="auto")   + 
  ggtitle("Is there a relation between number of steps and sleep quality ")

# boxplot
ggplot(data = data, 
       aes(x = user, y = Steps)) + geom_boxplot() + ggtitle("User - Steps")

ggplot(data = data, 
       aes(Steps, dayOfWeek) ) + geom_bar( stat="identity")


ggplot(data = data, 
       aes(x = dayOfWeek, y = Sleep.Quality)) + geom_boxplot() + ggtitle("Day of Week - Sleep Quality")
