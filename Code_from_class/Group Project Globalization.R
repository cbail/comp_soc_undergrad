
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



#NOW LET'S TRY THE OTHER SAMPLING APPROACH WHERE WE START FROM A LIST OF PEOPLE

#FIRST POLITICANS

test<-read.csv("http://cbail.github.io/Senators_Twitter_Data.csv")

test$twitter_id

sanders_tweets<-get_timeline("SenSanders", n=3199)

sanders_tweets$text

mean(sanders_tweets$favorite_count)

#NOW LET'S READ IN IMDB DATA

#we downloaded a .tsv file from IMDB (the movie database), to try to see if
# it has the twitter handles of celebrities in it.
imdbdata<-read.table(file = '~/Desktop/name.basics.tsv', 
                     sep = '\t', 
                     header = TRUE,
                     fill=TRUE
                     )

names(imdbdata)

head(imdbdata)

#now let's try to create  a network of verified accounts

leos_follows<-get_friends("@LeoDiCaprio")

blerg<-lookup_users(leos_follows$user_id)

verified<-blerg[blerg$verified==TRUE,]

verified$description

verified$location

myratelimits<-rate_limit()

#let's read in the file

#install.packages("rvest")
library(rvest)

star_page<-read_html("http://profilerehab.com/actors_twitter_accounts")

section_of_starpage<-html_node(star_page, xpath='//*[@id="entry"]')

star_text<-html_text(section_of_starpage)

# install.packages("qdapRegex")
library(qdapRegex)

#this function extracts urls

stuff<-ex_url(star_text)

#install.packages("stringr")

names<-gsub("www.twitter.com/","", stuff)

#we need to get of these
#\nDoes
#\
#\nWhere

#the wonderful solution of Giovanni Maggi! Great job!
test<-gsub("\\\\n","", names)
test1<-gsub("\\\\nWhere","",test)
test2<-gsub("\\\\nWhat","",test1)
test3<-gsub("\\\\nDoes","",test2)
test4<-gsub("\\\\n","",test3)


library(rtweet)


hart_friends<-get_friends("KevinHart4real")

names_hart_friends<-lookup_users(hart_friends$user_id)

names_hart_friends$screen_name

#construct edgelist (data structure that we use for social networks)

hart_network<-cbind(hart_friends$user, names_hart_friends$screen_name)



#to look up twitter ids you can use this site https://tweeterid.com/



#next let's write a loop to get edgelists for multiple celebrities all at once

our_celebs<-c("TheRock", "channingtatum", "KevinHart4real")

celebrity_networks<-as.data.frame(NULL)

for(i in 1:length(our_celebs)){
  #first get the names of the people the celebrity follows
  temp_network<-get_friends(our_celebs[i])
  #then look up their screen names (twitter handles)
  named_connections<-lookup_users(temp_network$user_id)
  #next create an edge list for that person
  edge_list<-cbind(temp_network$user, named_connections$screen_name)
  #finally, bind all the edge lists together to create a single long edge list
  #with our blank data frame above
  celebrity_networks<-rbind(celebrity_networks, edge_list)
  #print value of i for debugging
  print(i)
  #pause to avoid rate limiting
  Sys.sleep(3)
}

#to visualize the network and analyze it we need to create a new data structure called a network
#to do this we will use the igraph package

#install.packages("igraph")
library(igraph)

#convert my edgelist to a network object
#to do this I use a function called graph.data.frame()

our_network<-graph.data.frame(celebrity_networks)

#now let's try to visualize it
plot(our_network)

#wow that's ugly

#to make it prettier we use ggraph
#install.packages('ggraph')
library(ggraph)

#good tutorial on ggraph is here: https://kateto.net/sunbelt2019

ggraph(our_network) +
  geom_edge_link() +   # add edges to the plot
  geom_node_point() 

#we want to add the names of people in our network
#but if we add everyone's name we get a mess

#so, let's try to find the influential people in our small network
#to do this we will measure each person's "network degree" 
#degree describes the total number of social connections that
#each person has. We can calculate degree in igraph as follows

out<-as.data.frame(degree(our_network))

#next, let's label only the people who have more than
#one connection in our network

#now let's insert the degree variable into our network object
V(our_network)$degree<-degree(our_network)


ggraph(our_network) +
  geom_edge_link(width=.1) +   # add edges to the plot
  geom_node_point(size=.2) +
  geom_node_text(aes(label = name, 
                     filter=V(our_network)$degree>1), 
                     repel = TRUE, size=2)
  




# out<-strsplit(test4, "[\\\\]|[^[:print:]]",fixed=FALSE)
# out[1]

#https://stackoverflow.com/questions/27721008/how-do-i-deal-with-special-characters-like-in-my-regex
#gsub("\\\\n\","",names)









