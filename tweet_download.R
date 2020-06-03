# In this file I will build up a proper database with all the tweets needed for the analysis
# In particular, I will download the tweets in the following accounts:
# @GiuseppeConteIT
# @matteosalvinimi
# @nzingaretti
# @Antonio_Tajani

## install rtweet from CRAN (only once)
# install.packages("rtweet")

## load rtweet package
library(rtweet)

# load other packages
library(tidyverse)
library(lubridate) 
library(tidyr)
library(scales)

## quick overview of rtweet functions
vignette("intro", package = "rtweet")
 
## lookup users by screen_name or user_id
user <- c("GiuseppeConteIT", "matteosalvinimi", "nzingaretti","Antonio_Tajani")
it_politicians <- lookup_users(user)

## preview users data
it_politicians

# extract most recent tweets data from the famous tweeters
tweet <- tweets_data(it_politicians)


## Get the most recent 3,200 tweets from the selected politicians
conte      <- get_timelines("GiuseppeConteIT", n = 5000)
salvini    <- get_timelines("matteosalvinimi", n = 5000)
zingaretti <- get_timelines("nzingaretti"    , n = 5000)
tajani     <- get_timelines("Antonio_Tajani" , n = 5000)

# keep only the interesting variables
names(conte)

# filter the 4 datset based on "created_at" variable. I want only those tweets
# after the 15.01.2020, when the Covid-19 begun making also EU states warried
ref <- as.numeric(as.Date("2020-01-15"))
# filter and select conte databse
dt <- as.numeric(as.Date(conte$created_at))
ind <- dt < ref
conte_f <- conte %>% filter(!ind) %>% 
  select(source, user_id, created_at, screen_name, text, retweet_count,
         reply_to_user_id, is_retweet, favorite_count) %>%
  filter(source != "Twitter Web App")
# filter salvini databse
dt <- as.numeric(as.Date(salvini$created_at)) 
ind <- dt < ref
salvini_f <- salvini %>% filter(!ind) %>% 
  select(source, user_id, created_at, screen_name, text, retweet_count,
         reply_to_user_id, is_retweet, favorite_count) %>%
  filter(source != "Twitter Web App")
# filter zingaretti databse
dt <- as.numeric(as.Date(zingaretti$created_at))
ind <- dt < ref
zingaretti_f <- zingaretti %>% filter(!ind) %>% 
  select(source, user_id, created_at, screen_name, text, retweet_count,
         reply_to_user_id, is_retweet, favorite_count) %>%
  filter(source != "Twitter Web App")
# filter tajani databse
dt <- as.numeric(as.Date(tajani$created_at))
ind <- dt < ref
tajani_f <- tajani %>% filter(!ind) %>% 
  select(source, user_id, created_at, screen_name, text, retweet_count,
         reply_to_user_id, is_retweet, favorite_count) %>%
  filter(source != "Twitter Web App")

# saving the data frames of piliticians tweets in .txt
write.table(conte_f,"conte.txt",sep=";",row.names=FALSE)
write.table(salvini_f,"salvini.txt",sep=";",row.names=FALSE)
write.table(zingaretti_f,"zingaretti.txt",sep=";",row.names=FALSE)
write.table(tajani_f,"tajani.txt",sep=";",row.names=FALSE)
