# global options
options(stringsAsFactors = F)
setwd(dirname(parent.frame(2)$ofile))

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
source("explore_trends.R")

# define necessary string vars
output_path <- "data/"
query <- "election2020"
end_date <- "2020-11-18" # how far back you want to go in time
date.string <- as.character(Sys.Date())
query_file_name <- paste0(output_path, query, "_", end_date, "_to_", date.string, "_tweets.csv")



start <- readline(prompt = "Load default file (F) or pull tweets (T)? ")

if (start == "F") {
  # load a specific file for work
  file <- "data/election2020_2020-11-18_to_2020-11-24_tweets.csv"
  tweets <- fread(file, na.strings = c("",NA))
  tweets$user_id <- str_remove(tweets$user_id, "x")
  tweets$status_id <- str_remove(tweets$status_id, "x")
  query <- str_split(file, "_")[[1]][1] %>%
    str_replace("data/", "")
  
} else if (start == "T") {
  # authenticate twitter
  authenticate_twitter()
  tweets <- pull_max_tweets(query, end_date)
  save_as_csv(tweets, file_name=query_file_name)
  # load back in for clean processing
  tweets <- fread(query_file_name, na.strings = c("",NA))
  
} else {
  
  print("Didn't catch that, please try again.")
  
}


# take only cols of interest for text-mining analysis
tweets %>% 
  select(c(3:5,13,14,17,78,83,84)) -> tweets

# get sentiment scores per document
sa_tweets <- document_level_sa(tweets)


# adjust variable types for analysis
sa_tweets$screen_name <- as.factor(sa_tweets$screen_name)
sa_tweets$verified <- as.factor(sa_tweets$verified)
sa_tweets$created_at <- as.Date(sa_tweets$created_at,
                         tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
                         optional = FALSE)
sa_tweets$account_created_at <- as.Date(sa_tweets$account_created_at,
                                 tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
                                 optional = FALSE)


# create quant var df
quants <- isolate_quants(sa_tweets)

# create qual var df
quals <- isolate_quals(sa_tweets)

# create NLP var df
texts <- isolate_texts(sa_tweets)




