
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


#to save your data 

save(nameoffile, file="Sarah Data.Rdata")

#to find the location of the data check your working
#directory

getwd()

#if you want to set the location of your directory

setwd("~/Desktop")

#read in files from each student
load("/Users/christopherandrewbail/Desktop/Sarah Data.Rdata")  

#because both Sarah and Emma used the same name for hte data we 
#have to rename it
sarah_tweets<-english_tweets

load("/Users/christopherandrewbail/Desktop/Emma's Data.Rdata")

emma_tweets<-english_tweets

load("/Users/christopherandrewbail/Desktop/Jacopo's tweet.Rdata")

jacopo_data<-acqua_alta_data

load("/Users/christopherandrewbail/Desktop/ruggseadata.Rdata")

ruggero_data<-acqua_alta_data

load("/Users/christopherandrewbail/Desktop/Gio's english tweets.Rdata")

gio_tweets<-english_tweets

load("/Users/christopherandrewbail/Desktop/Francesca Data.RData")

francesca_tweets<-acqua_alta_data

emma_tweets$sentiment<-NULL
sarah_tweets$sentiment<-NULL
gio_tweets$sentiment<-NULL
emma_tweets$neg_tweet<-NULL
sarah_tweets$neg_tweet<-NULL
gio_tweets$neg_tweet<-NULL

#this is how we combine the datasets using the rbind function
all_the_tweets<-rbind(emma_tweets, 
                      sarah_tweets,
                      jacopo_data,
                      ruggero_data,
                      gio_tweets,
                      francesca_tweets
                      )

#we can de-duplicate the dataset using the "unique" function

non_duplicate_tweets<-unique(all_the_tweets)

save(non_duplicate_tweets, file="All Tweets.Rdata")

#now let's try using the NRC sentiment analysis method

library(syuzhet)
non_duplicate_tweets$sentiment<-get_sentiment(non_duplicate_tweets$text,
                                              method="nrc")

#lets take a look
head(non_duplicate_tweets$sentiment)

hist(non_duplicate_tweets$sentiment)



library(ggplot2)

ggplot(non_duplicate_tweets, aes(x=sentiment, y=retweet_count))+
  geom_point()+
  geom_smooth()+
  theme_minimal()+
  ylim(0,5000)


#let's create a variable that describes whether there is significant negative
#tone to each tweet

non_duplicate_tweets$neg_tweet<-0
non_duplicate_tweets$neg_tweet[non_duplicate_tweets$sentiment<0]<-1

table(non_duplicate_tweets$neg_tweet)

non_duplicate_tweets$followers_count[1]

our_model<-lm(retweet_count~
                neg_tweet +
                followers_count,
              data=non_duplicate_tweets
)

summary(our_model)

table(non_duplicate_tweets$lang)

?get_sentiment()

#now we found out that lots of tweets aren't
#being analyzed if they are in non-romance languages


#let's limit our analysis to english or italian


language<-english_data

english_data<-non_duplicate_tweets[non_duplicate_tweets$lang=="en",]

italian_data<-non_duplicate_tweets[non_duplicate_tweets$lang=="it",]


#rerun the sentiment analysis specifying the language

english_data$sentiment<-get_sentiment(english_data$text,
                                      method="nrc",
                                      language="english")

italian_data$sentiment<-get_sentiment(italian_data$text,
                                      method="nrc",
                                      language="italian")


english_data$neg_tweet<-0
english_data$neg_tweet[english_data$sentiment< -1]<-1

table(english_data$neg_tweet)

negative_tweets<-english_data[english_data$neg_tweet==1,]
head(negative_tweets$text, 10)


ggplot(english_data, aes(x=sentiment, y=retweet_count))+
  geom_point()+
  geom_smooth()+
  theme_minimal()+
  ylim(0,5000)

#create and indicator of visual data

english_data$av<-0
english_data$av[!is.na(english_data$media_type)]<-1

#standardize the coefficients
english_data$followers_standardized<-scale(english_data$followers_count)




our_model<-lm(retweet_count~
                neg_tweet +
                av +
                followers_count,
              data=english_data
)

summary(our_model)

#install.packages("coefplot")
library(coefplot)

coefplot(our_model, intercept=FALSE)+
  theme_minimal()


#make list of top hashtags

non_duplicate_tweets$text[1]


library(stringi)
counts_of_hashtags<-as.data.frame(NULL)

for (i in 1:nrow(hashtags)){
 temp<-non_duplicate_tweets[str_detect(non_duplicate_tweets$text, hashtags$Acqua.ALta.Hashtags[i]),]
 row<-as.data.frame(cbind(hashtags$Acqua.ALta.Hashtags[i], nrow(temp)))
 names(row)<-c("Hashtag","Count")
 counts_of_hashtags<-data.frame(rbind(counts_of_hashtags, row))
 print(i)
}

#change factor variables into numeric variables

counts_of_hashtags$Hashtag<-as.character(counts_of_hashtags$Hashtag)
counts_of_hashtags$Count<-as.numeric(as.character(counts_of_hashtags$Count))

hashtag_count<-ggplot(counts_of_hashtags, aes(x=Hashtag, y=Count))+
  geom_bar(stat="identity")+
  theme_minimal()+
  #theme(axis.text.x = element_text(angle = 90))+
  xlab("")+
  ylab("Number of Mentions in the Data")+
  coord_flip()

setwd("~/Desktop")
ggsave(hashtag_count, file="hashtag count.png", width=12, height=8, dpi=200)



english_data$time<-as.Date(english_datas$created_at, format="%Y-%m-%d %x")

library(dplyr)
plotting_data<-
  english_data %>%
  group_by(time) %>%
    summarise(feeling=mean(sentiment))

library(ggplot2)

ggplot(plotting_data, aes(x=time, y=feeling))+
  geom_line(colour="blue")+
  theme_minimal()+
  xlab("Date")+
  ylab("Average Sentiment (Higher Values=Positive Sentiment)")

length(table(non_duplicate_tweets$lang))



