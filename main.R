wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

# global options
options(stringsAsFactors = F)
library(tidyverse)
library(tidytext)
require(data.table)
require(ggplot2)
require(gridExtra)
require(PerformanceAnalytics)
require(magrittr)
library(rtweet)
library(janitor)
library(tweetrmd)
library(wordcloud)
source("app_authentication.R")
source("get_tweets.R")
source("top_5_analysis.R")
source("create_wordcloud.R")
source("tidy_tokens.R")
source("sentiment_analysis.R")

# authenticate twitter
authenticate_twitter()

# get tweets for multiple days back
query <- "election2020"
end_date <- "2020-11-18" # how far back you want to go in time
tweets <- pull_max_tweets(query, end_date)

# save tweets to file
date.string <- as.character(Sys.Date())
query_file_name <- paste0("data/", query, "_", end_date, "_to_", date.string, "_tweets.csv")
save_as_csv(tweets, file_name=query_file_name)
# load back in for clean processing
tweets <- fread(query_file_name, na.strings = c("",NA))

# or load a specific file for work
file <- "data/election2020_2020-11-18_to_2020-11-24_tweets.csv"
tweets <- fread(file, na.strings = c("",NA))
query <- str_split(file, "_")[[1]][1]

# Get Top 5 summary tables 
top5_summaries(tweets, query_file_name)

# Generate nrc sentiment analysis - joy
omit_words <- c(query, "vote") # adjust as needed
joy_words <- get_sentiments_words(tweets, omit_words, "nrc", "joy")
create_wordcloud(joy_words, "output/election2020_joy_words.png")

# Generate nrc sentiment analysis - anger
omit_words <- c(query, "vote", "fraud") # adjust as needed
anger_words <- get_sentiments_words(tweets, omit_words, "nrc", "anger")
create_wordcloud(anger_words, "output/election2020_anger_words.png")


# Generate nrc sentiment analysis - disgust
omit_words <- c(query, "vote") # adjust as needed
anger_words <- get_sentiments_words(tweets, omit_words, "nrc", "disgust")
create_wordcloud(anger_words, "output/election2020_disgust_words.png")




