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


# define necessary string vars
output_path <- "data/"
query <- "election2020"
end_date <- "2020-11-18" # how far back you want to go in time
date.string <- as.character(Sys.Date())
query_file_name <- paste0(output_path, query, "_", end_date, "_to_", date.string, "_tweets.csv")

# load a specific file for work
file <- "data/election2020_2020-11-18_to_2020-11-24_tweets.csv"
tweets <- fread(file, na.strings = c("",NA))
query <- str_split(file, "_")[[1]][1] %>%
  str_replace("data/", "")

# take only cols of interest for text-mining analysis
tweets %>% 
  select(c(1:5,13,14,17,78,83,84)) -> tweets

# get sentiment analysis
sa_tweets <- document_level_sa(tweets)


### Interesting questions for analysis

# Step 1 - check structure

dim(sa_tweets)
str(sa_tweets)

# What is the overall distribution of sentiment for this sample? Is it negative or positive? 

## DATAVIZ TEMPLATE

# ggplot (data = <DATA> ) +
#   <GEOM_FUNCTION> (mapping = aes( <MAPPINGS> ),
#                    stat = <STAT> , position = <POSITION> ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION> +
#   <SCALE_FUNCTION> +
#   <THEME_FUNCTION>

# Visual inspection for normality:

sent_p1 <- ggplot(sa_tweets, aes(ave_sentiment)) +
  geom_histogram() +
  stat_bin(bins = 50)
sent_p1



# Significance test for normally (H0 = is normal)

# get a smaller representation for shapiro test (5000 max)
shap_sample <- sample_n(sa_tweets, 100)

sent_p2 <- ggplot(shap_sample, aes(ave_sentiment)) +
  geom_histogram() +
  stat_bin(bins = 50)
sent_p2

shapiro.test(shap_sample$ave_sentiment)


# Calculate the average sent score of tweet sample

sent_stats <- summary(sa_tweets$ave_sentiment)
mean <- sent_stats[[4]] # slightly neg





# pull fresh tweets and save to file

# # authenticate twitter
# authenticate_twitter()
# tweets <- pull_max_tweets(query, end_date)
# save_as_csv(tweets, file_name=query_file_name)
# # load back in for clean processing
# tweets <- fread(query_file_name, na.strings = c("",NA))


# Get Top 5 summary tables 
#top5_summaries(tweets, query_file_name)

# Generate nrc sentiment analysis
# omit_words <- c(query, "vote") # adjust as needed
# nrc_words <- get_sentiments_words(tweets, omit_words, "nrc", "joy")
# create_wordcloud(cloud_words, "output/nrc_cloud.png")
# 
# # Generate sentiment analysis with AFINN
# afinn_words <- get_sentiments_words(tweets, omit_words, "afinn", "negative")
# create_wordcloud(afinn_words, "output/afinn_cloud.png")




