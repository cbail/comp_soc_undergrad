
#Today we are going to learn basic sentiment analysis and plotting in R

#load rtweet package
library(rtweet)

#authenticate
create_token(app=app_name, consumer_key=consumer_key, consumer_secret=consumer_secret)

#get tweets from WWF 
wwf<-get_timeline("WWF")

#use the head command to briefly browse the text of the tweets
#the $ is what we use to tell R what part of the dataframe we want to analyze (which variable)
wwf$text

#let's say we want to assess the sentiment of each tweet in our dataset
#to do this, we will use a tool called sentiment analysis
#to use sentiment analysis, we will install a new R package

#install.packages("syuzhet")

library(syuzhet)

#this is how we code the sentiment of a sentence
get_sentiment("bears are scary")

#this is how I pull up the text of the fourth tweet in our dataframe
wwf$text[4]

#this is how we get the sentiment score for the fourth tweet in our dataset
get_sentiment(wwf$text[4])

#now let's get the scores for all of the tweets
get_sentiment(wwf$text)

#these were ouputted in the console, but, if we want to visualize
#them we need them to be a variable

wwf$sentiment<-get_sentiment(wwf$text)

#let's use the head command to look at the first five scores

head(wwf$sentiment)

#let's find the most positive tweet in the dataset

max(wwf$sentiment)

#the largest score

min(wwf$sentiment)

#the average score
mean(wwf$sentiment)

#let's find the most positive tweets

really_positive_tweets<-wwf[wwf$sentiment>3.5,]

#let's find the most negative tweets
really_negative_tweets<-wwf[wwf$sentiment<0,]

#let's sort them by how negative they are
sorted_negative_tweets<-really_negative_tweets[order(really_negative_tweets$sentiment, decreasing=TRUE),]

#let's plot sentiment against retweet count
plot(wwf$sentiment, wwf$retweet_count)

#this plot is UGLY

#So, we're going to use the ggplot2 package to produce
# a prettier graph, and one that will have a trend line

#install.packages("ggplot2")
library(ggplot2)

ggplot(wwf, aes(x=sentiment, y=retweet_count))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  xlab("Sentiment Score")+
  ylab("Retweet Count")

# now let's try to see if we can find a trend across multiple organizations

#this is how we create a vector in r
non_profits<-c("wwf","unicef","amnesty")


#let's get the data for all the organizations in our vector above
advocacy_data<-get_timeline(non_profits, n=1000)

#now let's code the sentiment of each tweet for all organizations

advocacy_data$sentiment<-get_sentiment(advocacy_data$text)

#now let's make an ugly plot

plot(advocacy_data$sentiment, advocacy_data$retweet_count)


ggplot(advocacy_data, aes(x=sentiment, y=retweet_count))+
  geom_point(size=.5)+
  geom_smooth()+
  facet_grid(~screen_name)+
  theme_bw()+
  xlab("Sentiment")+
  ylab("Retweet Count")+
  ylim(0,5000)







































