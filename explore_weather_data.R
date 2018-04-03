weather <- read.csv("C:/Users/mutaz/Downloads/syd_weath.csv", stringsAsFactors = FALSE)

weather$date <- as.POSIXct(weather$date)


head(weather)
dim(weather)



attach(weather)
range(date)
summary(weather)
str(weather)
weather$cond <- as.factor(cond)
plot(temp, weather)
par(mfrow = c(1,1))

hist(weather$temp)
d <- density(weather$temp)
plot(d)
boxplot(weather$temp)
plot(temp, dew_pt)
str(weather)
plot(cond)
