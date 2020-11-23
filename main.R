wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

# global options
options(stringsAsFactors = F)
# Load libraries
library(tidyverse)
library(tidytext)
require(data.table)
require(ggplot2)
require(gridExtra)
require(PerformanceAnalytics)
require(magrittr)
# load twitter library
library(rtweet)
# tools for cleaning data fast
library(janitor)
# for embedding tweets in markdown
library(tweetrmd)

source("app_authentication.R")
source("get_tweets.R")
source("top_5_analysis.R")

# get tweets for multiple days back
query <- "election2020"
end_date <- "2020-11-20" # how far back you want to go in time
tweets <- pull_max_tweets(query, end_date)

# save to file
date.string <- as.character(Sys.Date())
query_file_name <- paste0(query, "_", end_date, "_to_", date.string, "_tweets.csv")
save_as_csv(tweets, file_name=query_file_name)

# load data
input_file <- "election2020_2020-11-20_to_2020-11-23_tweets.csv" # "<ENTER HERE>"
tweets <- fread(input_file, na.strings = c("",NA))

# get "top 5" tables
source("top_5_analysis.R")



# word cloud visualization
library(wordcloud)
words <- tweets %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),
         !str_detect(word, "@\\S+")) %>%
  filter(word != query) %>%
  count(word, sort = TRUE)


# create color gradient for word cloud
cloud_filename <- paste0(query_file_name, "_wordcloud.png")

png(filename=cloud_filename,
    width=500,
    height=500,
    units="px")
wordcloud(words$word, words$n, random.order = FALSE, max.words = 100, color = alpha("blue", seq(0.4,1, 0.05)))
dev.off()


# NEXT UP: Work through tidy text mining text book: https://www.tidytextmining.com/sentiment.html


# Step 1: Create a sentiment analysis

tidy_tweets <- tweets %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),
         !str_detect(word, "@\\S+")) %>%
  filter(word != query) %>%
  filter(word != "vote")


# joy wordcloud from election2020
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

joy_words <- tidy_tweets %>%
  inner_join(nrc_joy) %>%
  count(word, sort = T)

# joy wordcloud from election2020
png(filename="election2020_joy_words.png",
    width=500,
    height=500,
    units="px",
    res=140)
wordcloud(joy_words$word,
          joy_words$n,
          random.order = FALSE,
          max.words = 200,
          color = alpha("purple", seq(0.4,1, 0.05))
          )
dev.off()

# anger wordcloud from election2020
nrc_anger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

anger_words <- tidy_tweets %>%
  inner_join(nrc_anger) %>%
  count(word, sort = T)

png(filename="election2020_anger_words.png",
    width=500,
    height=500,
    units="px",
    res=140)
wordcloud(anger_words$word,
          anger_words$n,
          random.order = FALSE,
          max.words = 200,
          color = alpha("red", seq(0.4,1, 0.05))
)
dev.off()





