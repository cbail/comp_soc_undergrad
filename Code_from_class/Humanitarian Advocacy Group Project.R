
#sampling strategy #1: define the sample by text


#let's grab th network of one climate change group
library(rtweet)

#get the names of the accounts greenpeace follows
greenfollows<-get_friends("@greenpeace")

#this is their numeric id
greenfollows$user_id

#lookup their infomratio musing their numeric id
greenfollowstuff<-lookup_users(greenfollows$user_id)


#install.packages("tidytext")
library(tidytext)
library(dplyr)

#first we are creating a dataframe that contains
#all of the words in the descriptions of the peopl
#who greenpeace follows
all_the_words<- greenfollowstuff %>%
  select(description) %>%
    unnest_tokens("word", description)

#now we need to remove common words like "and" or "the" that we call "stopwords"
data("stop_words")
no_stop_words<-all_the_words %>%
  anti_join(stop_words)

#next, let's count the top words after removing stopwords

final_word_list<-no_stop_words %>%
  count(word) %>%
    arrange(desc(n))

words_to_define_sample<-c("greenpeace",
                          "climate change",
                          "green energy",
                          "environmental",
                          "#ClimateEmergency")

#install.packages("stringr")
library(stringr)
green_sample<-greenfollowstuff[str_detect(greenfollowstuff$description, words_to_define_sample),]
green_sample$description

#let's create a network visualization that will
#allow us to map who is connected to who on 
#twitter among the 34 accounts in our sample

#we are going to write a loop to iterate over each
#name of the twitter user in our sample, and collect
#the names of the people who those people follow

my_green_network<-as.data.frame(NULL)


for(i in 1:nrow(green_sample)){
  temporarydata<-get_friends(green_sample$screen_name[i])
  my_green_network<-rbind(my_green_network, temporarydata)
  #we print i to help us keep track of how fast the code is going and where it breaks (if it breaks)
  print(i)
  #to avoid rate limiting we can pause for 60 seconds
  Sys.sleep(60)
}

green_sample$screen_name[15]


#how to assign column names
names(my_green_network)<-c("name1","name2")



green_sample$screen_name




#SAMPLING STRATEGY #2: BUILD A NETWORK

network<-get_friends(greenfollows$user_id,
                     retryonratelimit = TRUE)





names(greenfollowstuff)
# 
# topfollows<-greenfollowstuff[greenfollowstuff$followers_count>10000,]
# 
# greenfollowstuff$location


# we are now shifting back towards studying climate change and the
#acqua alta in Italy.

library(rtweet)
acqua_alta_tweets<-search_tweets("flood in venice", n=1000)


#view text of tweets to find hashtags
acqua_alta_tweets$text

#here is our list of crowd-sourced hashtags: https://docs.google.com/spreadsheets/d/1_yJJtv2MCA0QrJU3Drxin7NLQHSkowQw3LTHwMtQW3Y/edit?usp=sharing

hashtags<-read.csv("~/Documents/GitHub/comp_soc_undergrad/Code_from_class/hashtags.csv",
                   stringsAsFactors = FALSE)

our_hashtags<-as.character(hashtags$Acqua.ALta.Hashtags)


#turns out we can't search for multiple hashtags at once,
# so let's write a loop

#to begin, let's remind ourselves how loops work
#here's a loop that counts to three

for(j in 1:3){
  print(j)
}

#what we want to do is cycle through our list of hashtags and collect
#tweets

acqua_alta_data<-as.data.frame(NULL)

for(i in 1:length(our_hashtags)){
  #each time through the loop we search twitter for the hashtag
  #we ask for 10,000 tweets that are not retweets and are recent (not popular)
  venice_flood_tweets<-search_tweets(our_hashtags[i], 
                                     n=10000,
                                     include_rts=FALSE,
                                     type="recent")
  #then we bind each dataset to the dataset before it 
  acqua_alta_data<-rbind(acqua_alta_data, venice_flood_tweets)
  #we print the value of i to debug our code
  print(i)
  #we pause R each time through the loop to avoid rate limiting
  Sys.sleep(3)
}

#we got rate limited, we need a longer Sys.sleep interval above

#this line lets you check the rate limits
myratelimits<-rate_limits()

#first let's look at what languages we have
table(acqua_alta_data$lang)

#let's just analyze the English tweets

english_tweets<-acqua_alta_data[acqua_alta_data$lang=="en",]

library(syuzhet)

english_tweets$sentiment<-get_sentiment(english_tweets$text)

library(ggplot2)

ggplot(english_tweets, aes(x=sentiment, y=retweet_count))+
         geom_point()+
         geom_smooth()+
         theme_minimal()


#let's create a variable that describes whether there is significant negative
#tone to each tweet

english_tweets$neg_tweet<-0
english_tweets$neg_tweet[english_tweets$sentiment<0]<-1

table(english_tweets$neg_tweet)

english_tweets$followers_count

our_model<-lm(retweet_count~
    neg_tweet +
    followers_count,
    data=english_tweets
    )

summary(our_model)








retweetcount= neg_tweet+ followers + error




















