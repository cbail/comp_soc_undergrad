
#IN THIS CODE WE ATTEMPT TO CREATE A SAMPLE OF INFLUENTIAL POLITICIANS 
#AND CELEBRITIES USING HASHTAGS

#First, we create a vector of hashtags (this is not complete, but it's a reasonable start)

#remember we use 'c()' to create a vector in R, this one is called "hongkonghashtags"
hongkonghashtags<-c("#HongKongers",
  "#HongKong",
  "#MillionMaskMarch",
  "#UmbrellaRevolution", 
  "#HongKongProtests", 
  "#StandwithHongKong",
  "#HongKongers")

#Next, we want to ask Twitter to search for mentions of each hashtag. 
#rather than doing each one one-by-one, however, let's pretend we have a long
#list (say 1000 hashtags). In this case, we may want to automate the data
#collection within a loop. The code below does this.

#first, we create a blank data frame (this is where we are going to store the 
#data that we collect)
twitterdata<-as.data.frame(NULL)

#each time we go through the loop, a different hashtag is going to be selected. To 
#get a better sense of how this works, try running the code below, which allows you
#to see the first hashtag
hongkonghashtags[1]

#now we need a way of cycling through each hashtag (or "iterating"). To do this,
# we will use somethign called a for loop

#the first part of this chunk of code tells are, create an abstract variable called
#"i"... and allow i to assume a value between 1 and the length of our vector (which is 7)
#this means that the first time through the loop, i will =1 and the second time i will=2
# and so on. 
for(i in 1:length(hongkonghashtags)){
#in this line of code, we use the search_tweets function to search for each of the hashtags in our vector
data<-search_tweets(hongkonghashtags[i])
#now we need a way to save each batch of data we collect. We use the rbind function
#to stitch everything together.
twitterdata<-rbind(twitterdata, data)
#finally, we use the print function below to debug our code. This also helps us monitor
#wether the code is working. If we run this entire loop, we should see 1, 2, 3, 4, etc 
#appear in the console. If the code breaks half way through, we can figure out what number 
#or which iteration of the loop was causing the problem.
print(i) 
#finally, we add a one second pause below using the sys.sleep function as follows
#this avoids twitter "rate limiting us" (stopping us from collecting too much data)
#within too short a period of time
Sys.sleep(1)
}

#now we have a new data frame called twitterdata. To get the names of all the people
#in the data, we use the "unique" function to get a list of all the different Twitter users
#who made a tweet that mentioned one of our hashtags
twitter_users<-unique(twitterdata$screen_name)
#this next line lets us see how many people there are
length(twitter_users)
  
#now we want to know how many followers these people have, so we use another function
#from the rtweet package called lookup_users
hashtag_users<-lookup_users(twitter_users)

#let's have a look at the variables available in the new dataset we created.
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




