
library(rtweet)
lafoodbanktweets<-get_timeline("LAFoodBank", n=3000)


names(lafoodbanktweets)

max(lafoodbanktweets$retweet_count)

library(dplyr)

#this is how we select only tweets that received more than 300 retweets
great_tweet<-subset(lafoodbanktweets, retweet_count>300)

#this is the way to do the same thing in "base" r instead of the dplyr package
#great_tweet<-lafoodbanktweets[max(lafoodbanktweets$retweet_count),]
  
#this is how we would look at the text of the tweets that have more than 300 retweets
great_tweet$text


min(lafoodbanktweets$retweet_count)

hist(lafoodbanktweets$retweet_count)

#find mentions of VIU

viu_tweets<-search_tweets("Venice International University")

one_person<-viu_tweets[viu_tweets$screen_name=="pietrafocaia",]

one_person<-subset(viu_tweets, screen_name="pietrafocaia")

#tell R where to save files (or what to use as your "working directory")
setwd("~/Desktop")

#save it as a .csv file (in case we want to send it to someone who can only use excel or open office)
write_as_csv(viu_tweets, "VIU Tweets.csv")

#save it as an R data file
save(viu_tweets, file="VIU Tweets.Rdata")

load("/Users/christopherandrewbail/Desktop/VIU Tweets.Rdata")

#pull data on multiple organizations at once
class_data<-get_timeline(c("WWF","greenpeace","amnesty"), n=3000)

#find all tweets that got more than 1000 retweets
top_tweets<-class_data[class_data$retweet_count>50000,]




