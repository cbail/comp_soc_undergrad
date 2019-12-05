
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

our_celebs<-c("TheRock", "channingtatum", 
              "KevinHart4real", "JohnStamos",
              "tomhanks","justdemi","LeoDiCaprio")


celebrity_networks<-as.data.frame(NULL)

for(i in 1:length(our_celebs)){
  #first get the names of the people the celebrity follows
  temp_network<-get_friends(our_celebs[i])
  
    if(nrow(temp_network)>0){
      #then look up their screen names (twitter handles)
      named_connections<-lookup_users(temp_network$user_id)
      #next create an edge list for that person
      edge_list<-cbind(temp_network$user, named_connections$screen_name)
      #finally, bind all the edge lists together to create a single long edge list
      #with our blank data frame above
      celebrity_networks<-rbind(celebrity_networks, edge_list)
      #print value of i for debugging
    }
  
  print(i)
  #pause to avoid rate limiting
  Sys.sleep(60)
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
  


#let's move to Gephi for easier and more intuitive visualization

#Gephi requires an edgelist and a nodelist 

#first let's make our edgelist
names(celebrity_networks)<-c("Source","Target")
names(celebrity_networks)

#but now we need to make our node list
#this needs to describe every unique person
#who ever appears in the data

name_list<-c(as.character(celebrity_networks$Source), 
                 as.character(celebrity_networks$Target))

name_list<-as.data.frame(unique(name_list))

names(name_list)<-"Source"
name_list$Label<-name_list$Source
name_list$ID<-name_list$Source

#now we need to create .csv files and then load them
#into Gephi

#we need to save it to our working directory

#to find your working directory you can use this code
getwd()

#to set your working directory to a different location you can
#use

setwd("~/Desktop")

write.csv(celebrity_networks, file="Edgelist.csv",
          row.names = FALSE)

write.csv(name_list, file="Nodelist.csv",
          row.names = FALSE)

# out<-strsplit(test4, "[\\\\]|[^[:print:]]",fixed=FALSE)
# out[1]

#https://stackoverflow.com/questions/27721008/how-do-i-deal-with-special-characters-like-in-my-regex
#gsub("\\\\n\","",names)




#let's say we want to get the gender of people in our network

#install.packages("gender")
library(gender)

#install.packages("genderdata", repos = "http://packages.ropensci.org", type = "source")

gender("Chris")


#example of an "if" loop

for (i in 1:5){
  if (i>3){
  print(i)
  }
}

#this is an if/else loop
for (i in 1:5){
  if (i>3){
    print(i)
  }else{
    print("awesome")  
  }
}

#while loop looks like this
for (i in 1:5){
  while(i>3){
  print(i)
  }
}


#now let's read in the data created by Alessio

finaledges<-read.csv("~/Desktop/celebrities.csv",
         header=TRUE,
         stringsAsFactors = FALSE
        )

library(igraph)

celeb_network<-graph.data.frame(finaledges)

V(celeb_network)$name
head(E(celeb_network))

V(celeb_network)$degree<-degree(celeb_network)

# remove isolates
total_losers <- V(celeb_network)[degree(celeb_network)<2]
pruned <- delete.vertices(celeb_network, total_losers)

length(V(celeb_network))
length(V(pruned))

pruned_data<-get.data.frame(pruned)

names(pruned_data)<-c("Source","Target")

pruned_data<-unique(pruned_data)

setwd("~/Desktop")
write.csv(pruned_data, file="New Edgelist.csv", row.names = FALSE)

#now let's remake the nodelist

nodes<-c(pruned_data$Source, pruned_data$Target)
nodes<-data.frame(unique(nodes))
names(nodes)<-"Source"
nodes$ID<-nodes$Source
nodes$Label<-nodes$Source
nodes$Source<-NULL

write.csv(nodes, file="New Nodelist.csv", row.names=FALSE)

#let's add in politicians in the list


my_politicians<-c(
                  #first congresspeople
                  "JoeLieberman","BachusAL06","RepRonPaul","Robert_Aderholt",
                  "JudgeTedPoe","SenBobCorker","VoteMarsha","RepAdamSmith","RepMikeQuigley",
                  "BillCassidy","BilbrayCa50","DrPhilRoe","RepMcClintock","Jim_Moran","RepJimMatheson",
                  "GabbyGiffords","SenSherrodBrown","DeanHeller","eltongallegly24","RepJoeBaca",
                  "USRepMikeDoyle","RepGoodlatte","repdonyoung","RepMaxineWaters","JackKingston",
                  "RandyNeugebauer","RepMikeRogersAL","RepBrianHiggins","DrPhilGingrey","repgregwalden",
                  "FrankPallone","jahimes","SenSanders","SenJeffMerkley","HarryEMitchell","PatrickMcHenry",
                  "RepWalterJones","johnthune","RepGusBilirakis","congbillposey","RepHankJohnson",
                  "PeteSessions","RepKenMarchant","TomRooney","aaronschock","GreggHarper","ConnieMackIV",
                  "CynthiaLummis","RepGregoryMeeks","DarrellIssa","LeaderHoyer","JimOberstar","JudyBiggert",
                  "SenMarkey","USRepSullivan","cbrangel","RoyBlunt","SenatorBurr","SteveAustria","RepPerlmutter",
                  "RepSchrader","VernBuchanan","AnderCrenshaw","RepMaryFallin","RepCliffStearns","SenArlenSpecter",
                  "PeterRoskam","RepSteveIsrael","RepJoeBarton","RepKevinBrady","MikeHMichaud","LeonardBoswell",
                  "SenatorCollins","ArthurDavis","MaryBonoUSA","RepMikeCoffman","SenJohnMcCain","repbenraylujan",
                  "DanaRohrabacher","RepBarrett","DavidVitter","CongressmanGT","SpeakerRyan","zachwamp",
                  "mlfudge","PaulBrounMD","JeffFortenberry","RepSires","SenatorMenendez","JerryMoran","LEETERRYNE",
                  "RepPeteKing","MicheleBachmann","Jim_Jordan","lisamurkowski","JudgeCarter","cathymcmorris",
                  "jasoninthehouse","ErikPaulsen","SenatorReid","russfeingold","bobinglis","RepMikeHonda",
                  "SenChrisDodd","virginiafoxx","clairecmc","JeffFlake","GovPenceIN","repblumenauer",
                  "BarbaraBoxer","GlennNye","LamarSmithTX21","michaelcburgess","JohnEnsign","petehoekstra",
                  "RepShimkus","kevinomccarthy","boblatta","jaredpolis","RobWittman","CandiceMiller","Randy_Forbes",
                  "CongJoeWilson","Dennis_Kucinich","JohnKerry","chelliepingree","johnculberson","tomperriello",
                  "RosLehtinene","keithellison","RepTimRyan","JohnCornyn","PeteOlson","RogerWicker","ChuckGrassley",
                  "neilabercrombie","JimDeMint","ThadMcCotter","MarkUdall","SpeakerBoehner","MarkWarner",
                  "askgeorge","RepTomPrice","TeamCantor","JohnBoozman",
                  #now presidents
                  "realDonaldTrump","BarackObama","BillClinton"
                  )


nodes$politician<-0
nodes$politician[nodes$ID %in% my_politicians]<-1

table(nodes$politician)

write.csv(nodes, file="New Nodelist.csv", row.names=FALSE)

#we are trying to eliminate irrelevant accounts from our data
#let's try to do this with some text analysis of the "descriptions:"
#of each person on Twitter


library(rtweet)

tenusers<-lookup_users(as.character(nodes$ID[1:10]))

descriptions<-lookup_users(as.character(nodes$ID))

head(descriptions$description)

accounts_to_delete<-c("business","organization")

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load_gh("trinker/entity")
# 
# library(entity)
# location_entity(descriptions$description[1])
# 
# Los Angeles
# United States
# LA

#we ran into a java issue that we don't have time to fix in class

#install.packages("tidytext")
library(tidytext)
library(dplyr)

#identify all unique words in our twitter descriptions
all_the_words<- descriptions %>%
  select(description) %>%
  unnest_tokens("word", description)


#now lets count the top words
final_word_list<-all_the_words %>%
  count(word) %>%
  arrange(desc(n))

#stop words are common prepositions or articles such as "the" "and" "or"
#we often to remove them when we do natural language processing because they 
#don't help us identify patterns

data("stop_words")
no_stop_words<-all_the_words %>%
  anti_join(stop_words)

final_word_list<-no_stop_words %>%
  count(word) %>%
  arrange(desc(n))

#now we have some candidate words for identifying patterns  in
#our data using a dictionary-based approach. A dictionary-based
#approach means we come up with a list of words and then count
#the number of times they appear in our dataset

#let's create a dictionary for comedians
#install.packages("stringr")
library(stringr)

words_to_define_sample<-c("comedian","comedy")
comedians<-descriptions[str_detect(descriptions$description, words_to_define_sample),]

nodes$comedian<-0
nodes$comedian[nodes$ID %in% comedians$screen_name]<-1
table(nodes$comedian)


#We came up with the idea of looking at the direction of influence
#between celebrities and politicians. To do this we need to
#get the tweets of everyone in the sample

library(rtweet)
trump_tweets<-get_timeline("realdonaldtrump", n=100)

celebrities<-c("@therock","@channingtatum")

politicans<-nodes[nodes$politician==1]
nonpoliticians<-nodes[nodes$politician==0]

#store count of retweets by people from other group
dataholder<-as.data.frame(NULL)
#store tweets
tweetholder<-as.data.frame(NULL)

for (i in 1:nrow(nodes)){
  user1<-get_timeline(nodes$screen_name[i], n=300)
  if(user1$politician==1){
    counter<-nrow(user1[user1$retweet_name %in% nonpoliticans,])
  }
  if(user1$politician==0){
    counter<-nrow(user1[user1$retweet_name %in% politicans,])
  }
  row<-as.data.frame(c(user1$screen_name[i], counter))
  dataholder<-rbind(dataholder, row)
  #now store tweets
  tweetholder<-rbind(tweetholderm, user1)
  Sys.sleep (20)
  print(i)
}

count<-nrow(trump_tweets[trump_tweets$retweet_name %in% celebrities,])


#now I am reading in data that you all collected
# load("/Users/christopherandrewbail/Desktop/tweetholder_501to603.Rdata")
load("/Users/christopherandrewbail/Desktop/lauras_data.Rdata")
load("/Users/christopherandrewbail/Desktop/lauras_data2.Rdata")

subsetted<-senators_timelines[,c("screen_name","text","retweet_count","mentions_screen_name","retweet_screen_name")]
new_allusers_timelines<-rbind(new_allusers_timelines, subsetted)

# finaldata<-rbind(senators_timelines new_allusers_timelines)

gen_info<-read.csv("~/Desktop/general_info.csv",
                   stringsAsFactors = FALSE)

new_allusers_timelines$screen_name<-tolower(new_allusers_timelines$screen_name)
gen_info$screen_name<-tolower(gen_info$screen_name)
new_allusers_timelines$retweet_screen_name<-tolower(new_allusers_timelines$retweet_screen_name)

library(dplyr)
merged_data<-left_join(new_allusers_timelines, gen_info)


#first let's created a list of politicans
politicians<-gen_info[gen_info$politician==1,]
politician_names<-politicians$screen_name

celebrities<-gen_info[gen_info$politician==0,]
celebrities<-celebrities$screen_name


merged_data$retweet_politician<-0
merged_data$retweet_politician[merged_data$retweet_screen_name %in% politicians]<-1

merged_data$retweet_celebrity<-0
merged_data$retweet_celebrity[merged_data$retweet_screen_name %in% celebrities]<-1


library(dplyr)

sorted<-merged_data %>%
  group_by(screen_name)%>%
    summarise(count=mean(retweet_celebrity))














merged_data$key_outcome<-0

for(i in 1:nrow(merged_data)){
  if(merged_data$politician[i]==0){
  test<-merged_data$retweet_screen_name[i] %in% politician_names  
    if(test=TRUE){
      merged_data$key_outcome[i]<-1
    }
  }
  
  if(merged_data$politician[i]==1){
    test<-merged_data$retweet_screen_name[i] %in% celebrities
    if(test=TRUE){
      merged_data$key_outcome[i]<-1
    }
  }
print(i)
}




newdata<-unique(new_allusers_timelines)






