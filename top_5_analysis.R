top5_summaries <- function(tweets, query_file_name) {
  # top tweets by location	
  top5_locations <- tweets %>%	
    filter(!is.na(place_full_name)) %>% # ignore blank values	
    count(place_full_name, sort = TRUE) %>%	
    top_n(5)	
  
  location_filename <- paste0(query_file_name, "_top5_locations.csv")	
  save_as_csv(top5_locations, file_name=location_filename)	
  
  # get top shared link (need csv read in to get the data)	
  top5_shared_links <- tweets %>%	
    filter(!is.na(urls_expanded_url)) %>%	
    count(urls_expanded_url, sort = TRUE) %>%	
    top_n(5)	
  
  shared_filename <- paste0(query_file_name, "_top5_shared_links.csv")
  save_as_csv(top5_shared_links, file_name=shared_filename)	
  
  # most retweeted tweet	
  top5_retweets <- tweets %>%	
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
  top5_liked_tweet <- tweets %>%	
    arrange(-favorite_count) %>%	
    top_n(5, favorite_count) %>%	
    select(created_at, screen_name, text, favorite_count)	
  
  # top 5 tweeters	
  top5_most_active <- tweets %>%	
    count(screen_name, sort = TRUE) %>%	
    top_n(5)	
  
  # top 5 related hashtags	
  top5_hashtags <- tweets %>%	
    unnest_tokens(hashtag, hashtags, "tweets", to_lower = TRUE) %>%	
    filter(hashtag != query) %>%	
    count(hashtag, sort = TRUE) %>%	
    top_n(5)
}  
  

