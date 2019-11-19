
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
acqua_alta_tweets<-search_tweets("venice flood", n=1000)


#view text of tweets to find hashtags
acqua_alta_tweets$text

#here is our crowd-sourced hashtags: https://docs.google.com/spreadsheets/d/1_yJJtv2MCA0QrJU3Drxin7NLQHSkowQw3LTHwMtQW3Y/edit?usp=sharing


