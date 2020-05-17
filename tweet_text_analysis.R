# in this file I will analyse the dataset of italian politician's tweets in term of
# sentiment analysis (if possible). This will be the preliminary products. 
# The same anlysis will be run in .Rmd file, with the aim of having a proper .pdf file
# as final report

# load other packages
library(tidyverse)
library(lubridate)
library(tidyr)
library(scales)
library(tidytext)

# package for stopwords in italian
# install.packages("tm")
library(tm)
stopparole <- tm::stopwords("italian")

# package for sentiment analysis
# install.packages("udpipe")
library(udpipe)
dl <- udpipe_download_model(language = "italian")
str(dl)

# load datsframe
conte <- read.table("conte.txt", header=TRUE, sep = ";", fill = TRUE)
salvini <- read.table("salvini.txt", header=TRUE, sep = ";", fill = TRUE)
zingaretti <- read.table("zingaretti.txt", header=TRUE, sep = ";", fill = TRUE)
tajani <- read.table("tajani.txt", header=TRUE, sep = ";", fill = TRUE)

# from factor to character
conte[] <- lapply(conte, as.character)
salvini[] <- lapply(salvini, as.character)
zingaretti[] <- lapply(zingaretti, as.character)
tajani[] <- lapply(tajani, as.character)

# The pattern appears complex but all we are defining is a patter that starts with
# @, # or neither and is followed by any combination of letters or digits. This allow us
# to detect the special characters/words of twitter, such as hastag and tags
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# word analysis
# identify the single words in each tweets and build separate database for them
words_filter <- function(dataset){
  dataset %>%
    mutate(text = str_replace_all(text, #remove link to pictures
                                  "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    filter(!word %in% stopparole & 
             !str_detect(word, "^\\d+[A-Za-z]*\\d*[A-Za-z]*\\d*$") &
             !str_detect(word, "^[A-Za-z]{1,2}$")) %>% 
    filter(!word %in% stop_words$word) %>%
    select(word)
}

conte_w <- words_filter(conte)
salvini_w <- words_filter(salvini)
zingaretti_w <- words_filter(zingaretti)
tajani_w <- words_filter(tajani)

# what are the most common used words?
conte_count <- conte_w %>% count(word) %>% arrange(desc(n))
salvini_count <- salvini_w %>% count(word) %>% arrange(desc(n)) %>%
  filter(word != "#salvini" & word != "matteosalvinimi" &
           word != "twitter")
zingaretti_count <- zingaretti_w %>% count(word) %>% arrange(desc(n)) 
tajani_count <- tajani_w %>% count(word) %>% arrange(desc(n)) 

# sentiment analysis
# for the build-up of file FEEL_it.txt and words_values_it.txt see the R file
# it_database. After it, I use google sheets and excel to compute the translation
# which, I want to underline, is not perfect
nrc <- get_sentiments("nrc") %>%
  +     select(word, sentiment)
write.table(nrc,"nrc_it.txt",sep=";",row.names=FALSE)

# load the translate version of nrc from english to italian
nrc <- read.table("nrc_en_it.txt", sep = ";", header = TRUE, quote = "")
# function for sentiment analysis
sentiment_analysis <- function(dataset){
  sentiment_counts <- dataset %>%
    left_join(nrc, by = "word") %>%
    count(sentiment) %>%
    filter(!sentiment == "NA") %>% arrange(desc(n)) %>%
    mutate(prct = round(n/sum(n)*100, digits = 1))
  sentiment_counts
} 

conte_sent <- sentiment_analysis(conte_count)
salvini_sent <- sentiment_analysis(salvini_count)
zingaretti_sent <- sentiment_analysis(zingaretti_count)
tajani_sent <- sentiment_analysis(tajani_count)


