wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

# global options
options(stringsAsFactors = F)
# Load libraries
require(tidyverse)
require(data.table)
require(ggplot2)
require(gridExtra)
require(PerformanceAnalytics)
require(magrittr)
# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# tools for cleaning data fast
library(janitor)
# for flattening lists
library(purrr)


##### Step 1: Authenticate the app
# Grab app name, API key and API secret from the credentials saved in 1Password:
  
# whatever name you assigned to your created app
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
  as_csv <- readline(prompt = "Would you like to save as CSV? (y/n): ")
  
  if (as_csv == "n") {
    # save to RDS for future use
    print("Saving as RDS ...")
    query_file_rds <- paste0(query_file_name, ".rds")
    saveRDS(raw_tweets, file=query_file_rds)
    print("Done!")
  } else {
    print("Saving as CSV ...")
    query_file_csv <- paste0(query_file_name, ".csv")
    save_as_csv(raw_tweets, file=query_file_csv)
    print("Done!")
  }
  
} else if(get.tweets == "n") {
  file <- readline(prompt = "Please type the filename of the rds file you want to open: ")
  # Load data back in from large CSV file
  tweets_to_mine <- readRDS(file)
  print("Done! Have fun text-mining :)")
} else {
  print("sorry, didn't catch that.")
}

rm(get.tweets)




##### Step 3: Follow text-mining protocol

# To start, work through this tutorial from Jan 2020: https://medium.com/@traffordDataLab/exploring-tweets-in-r-54f6011a193d

# graph tweets by frequency over time

plot1 <- ts_plot(raw_tweets, by = "hours") +
  geom_line(color = "red") +
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets with a #covid19 hashtag",
       subtitle = paste0(format.Date(min(raw_tweets$created_at), "%d %B %Y"), " to ", format.Date(max(raw_tweets$created_at), "%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") + theme_minimal()

ggsave("plot1.png", width = 5, height = 5)

# top tweets by location
# raw_tweets %>%
  # filter(!is.na(place_full_name)) %>% # ignore blank values
  # count(place_full_name, sort = TRUE) %>%
  # top_n(5)


# # get top shared link
# raw_tweets %>% 
#   filter(!is.na(urls_expanded_url)) %>% 
#   count(urls_expanded_url, sort = TRUE) %>% 
#   top_n(5)




