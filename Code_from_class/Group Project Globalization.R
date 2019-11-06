

#vector of hashtags

hongkonghashtags<-c("#HongKongers",
  "#HongKong",
  "#MillionMaskMarch",
  "#UmbrellaRevolution", 
  "#HongKongProtests", 
  "#StandwithHongKong",
  "#HongKongers")

twitterdata<-as.data.frame(NULL)

hongkonghashtags[1]

for(i in 1:length(hongkonghashtags)){
data<-search_tweets(hongkonghashtags[i])
twitterdata<-rbind(twitterdata, data)
print(i) 
Sys.sleep(1)
}

twitter_users<-unique(twitterdata$screen_name)
length(twitter_users)
  
hashtag_users<-lookup_users(twitter_users)

names(hashtag_users)

#show me all of the people with more than 10,000 followers
influential_people<-hashtag_users[hashtag_users$followers_count>10000,]

influential_people$screen_name


brexitfolks<-search_tweets("#brexit", n=1000)

brexit_tweeters<-unique(brexitfolks$screen_name)

brexit_people<-lookup_users(brexit_tweeters)

top_brexit_tweeters<-brexit_people[brexit_people$followers_count>100000 &
                                  brexit_people$friends_count<1000 &
                                  brexit_people$verified=TRUE,]

top_brexit_tweeters$screen_name

#install.packages("instaR")

library(instaR)

test<-read.csv("http://cbail.github.io/Senators_Twitter_Data.csv")

test$twitter_id

sanders_tweets<-get_timeline("SenSanders", n=3199)

sanders_tweets$text

mean(sanders_tweets$favorite_count)




