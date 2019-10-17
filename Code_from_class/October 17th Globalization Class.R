#first we load the package
library(rtweet)

#you will need to add lines of code here that define your credentials (access token, secret etc)

#this is the line of code that asks Twitter to authenticate you
create_token(app=app_name, consumer_key=consumer_key, consumer_secret=consumer_secret)

#this is how we get tweets about a keyword or hashtag by anyone
stormi_tweets<-search_tweets("#stormi", n=1000, include_rts = TRUE)

#this is how we get tweets from a single twitter account
italy_politican_tweets<-get_timeline("matteosalvinimi")

#this is how we get tweets from two people at once

#first, we create a vector
italy_leaders<-c("matteosalvinimi","matteorenzi")

#now we pass the vector to the same get_timeline function
italy_politican_tweets<-get_timeline(user=italy_leaders)

#we could have also spelled it all out liek this:
italy_politican_tweets<-get_timeline(c("matteosalvinimi","matteorenzi"))

#indexing a vector (choosing something within a vector)
number_vector<-c(1,3000,3,4)
number_vector[2]
italy_leaders[2]
#a vector means a list of things, and an element is one part of the list

#let's try applying a function to our vector
max(number_vector)

#we found the largest number was 3,000. Yay!

#now what if we want to find the most popular tweet in our dataset

#first, we have to learn how to find a column within our dataset
#the name of the column in our dataset that contains the # of retweets
#is called retweet_count

italy_politican_tweets$retweet_count

#the most popular tweet received how many retweets? the code below will tell us
max(italy_politican_tweets$retweet_count)
#minimum # of tweets
min(italy_politican_tweets$retweet_count)
#average # of tweets
mean(italy_politican_tweets$retweet_count)

#find the most liked tweet
max(italy_politican_tweets$favorite_count)

#now let's subset the dataframe to show us only the top tweet (we know from the code above that this tweet got 6,827 likes)
the_best_tweet<-italy_politican_tweets[italy_politican_tweets$favorite_count==6827,]
#the comma position determines whether we ask for rows or columns


#now lets try to find every tweet that received more than 1,000 favorites
best_tweets<-italy_politican_tweets[italy_politican_tweets$favorite_count>1000,]



#here is a cheatsheet of many different functions in base r that are popular
#https://rstudio.com/wp-content/uploads/2016/05/base-r.pdf







