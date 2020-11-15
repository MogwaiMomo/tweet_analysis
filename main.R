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
# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# tools for cleaning data fast
library(janitor)
# for embedding tweets in markdown
library(tweetrmd)


##### Step 1: Authenticate the app
# Grab app name, API key and API secret from the credentials saved in 1Password:
  
#whatever name you assigned to your created app
appname <- "voc-collector"

# api key (example below is not a real key)
key <- "xsBRv4HDRFw3YRe1jg3lTWxz8"

# api secret (example below is not a real key)
secret <- "1Qf0flgv8R8Qia96SNFtJ7JjEoVsHAoNJ4gD0tDcoZDHh3ZpPi"

# access_token
access_token <- "3302721691-53elyQLaCZwWpAiCImnxBfm48mqmeLt6QqjDz2f"

# access_secret
access_secret <- "W4H4xpBLw3g6VkYrUtJEuIYaM2JQuSZZ58FKWm2m3Nq7U"

# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


test_token <- get_token()
# uncomment to test that the token is stable:
# test_token


##### Step 2: Collect tweets by hashtag
get.tweets <- readline(prompt = "Would you like to search today's tweets? (y/n): ")

if (get.tweets == "y") {
  query <- readline(prompt = "Please type the hashtag you want to search (exclude the #): ")
  num_tweets <- as.numeric(readline(prompt = "How many tweets? (limit 18000): "))
  hashtag <- paste0("#", query)
  raw_tweets <- search_tweets(q = hashtag,
                              n = num_tweets,
                              include_rts = FALSE,
                              `-filter` = "replies",
                              lang = "en")
  print("Saving today's data to file ...")
  date.string <- as.character(Sys.Date())
  query_file_name <- paste0(query, "_", date.string, "_tweets")
  print("Saving as CSV ...")
  query_file_csv <- paste0(query_file_name, ".csv")
  save_as_csv(raw_tweets, file_name=query_file_csv)
  print("Done! Reading csv back in for more usable data ...")
  rm(raw_tweets)
  raw_tweets <- fread(query_file_csv, na.strings = c("",NA))

} else if(get.tweets == "n") {
  file <- readline(prompt = "Please type the filename of the file you want to open: ")
  # Load data back in from large CSV file
  raw_tweets <- fread(file)
  query_file_name <- str_split(file, fixed("."), n=2)[[1]][1]
  query <- str_split(query_file_name, "_", n=2)[[1]][1]
  print("Done! Have fun text-mining :)")
} else {
  print("sorry, didn't catch that.")
}

rm(get.tweets)

##### Step 3: Follow text-mining protocol

# To start, work through this tutorial from Jan 2020:
# https://medium.com/@traffordDataLab/exploring-tweets-in-r-54f6011a193d

# graph tweets by frequency over time
tw_over_time <- ts_plot(raw_tweets, by = "hours") +
  geom_line(color = "red") +
  labs(x = NULL, y = NULL,
       title = paste0("Frequency of tweets"),
       subtitle = paste0(format.Date(min(raw_tweets$created_at), "%d %B %Y"), " to ", format.Date(max(raw_tweets$created_at), "%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") + theme_minimal()
ggsave("tw_over_time.png", width = 5, height = 5)

# top tweets by location
top5_locations <- raw_tweets %>%
  filter(!is.na(place_full_name)) %>% # ignore blank values
  count(place_full_name, sort = TRUE) %>%
  top_n(5)

location_filename <- paste0(query_file_name, "_top5_locations.csv")
save_as_csv(top5_locations, file_name=location_filename)

# get top shared link (need csv read in to get the data)
top5_shared_links <- raw_tweets %>%
  filter(!is.na(urls_expanded_url)) %>%
  count(urls_expanded_url, sort = TRUE) %>%
  top_n(5)
shared_filename <- paste0(query_file_name, "_top5_shared_links.csv")
save_as_csv(top5_shared_links, file_name=shared_filename)

# most retweeted tweet
top5_retweets <- raw_tweets %>%
  arrange(-retweet_count) %>%
  head(5)
retweets_filename <- paste0(query_file_name, "_top5_retweets.csv")
save_as_csv(top5_retweets, file_name=retweets_filename)

# get a screenshot of the top retweet
top_rt <- slice(top5_retweets, 1)
top_rt_screenshot <- tweet_screenshot(tweet_url(
  top_rt$screen_name,
  str_replace(top_rt$status_id, "x", "")))

# get the most liked tweet
top5_liked_tweet <- raw_tweets %>%
  arrange(-favorite_count) %>%
  top_n(5, favorite_count) %>%
  select(created_at, screen_name, text, favorite_count)

# top 5 tweeters
top5_most_active <- raw_tweets %>%
  count(screen_name, sort = TRUE) %>%
  top_n(5)

# top 5 related hashtags
top5_hashtags <- raw_tweets %>%
  unnest_tokens(hashtag, hashtags, "tweets", to_lower = TRUE) %>%
  filter(hashtag != query) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(5)


# word cloud visualization
library(wordcloud)
words <- raw_tweets %>%
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

tidy_tweets <- raw_tweets %>%
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


# Next challenge: Pull tweets from specific dates



pull_tweets_by_date <- function(x) {
  
# Users are instead encouraged to breakup data retrieval into smaller chunks by
# leveraging retryonratelimit and then using the status_id of the oldest tweet
# as the max_id to resume searching where the previous efforts left off.
  query <- readline(prompt = "Please type the hashtag you want to search (exclude the #): ")
  hashtag <- paste0("#", query)
  raw_tweets <- search_tweets(q = hashtag,
                              include_rts = FALSE,
                              `-filter` = "replies",
                              lang = "en",
                              # max_id = last_tweet,
                              )
  print("Saving today's data to file ...")
  date.string <- as.character(Sys.Date())
  query_file_name <- paste0(query, "_", date.string, "_tweets")
  print("Saving as CSV ...")
  query_file_csv <- paste0(query_file_name, ".csv")
  save_as_csv(raw_tweets, file_name=query_file_csv)
  print("Done! Reading csv back in for more usable data ...")
  rm(raw_tweets)
  raw_tweets <- fread(query_file_csv, na.strings = c("",NA))
  
  
}
