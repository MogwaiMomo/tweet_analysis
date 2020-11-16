# get as many tweets of a given hashtag as possible from the last week. (May
# time out due to too many tweets.)
pull_tweets_last_7_days <- function(query) {
  hashtag <- paste0("#", query)
  raw_tweets <- search_tweets(q = hashtag,
                              include_rts = FALSE,
                              `-filter` = "replies",
                              lang = "en",
                              retryonratelimit = TRUE
  )
  
  
  print("Saving today's data to file ...")
  date.string <- as.character(Sys.Date())
  query_file_name <- paste0(query, "_last_7_days_from_", date.string, "_tweets")
  print("Saving as CSV ...")
  query_file_csv <- paste0(query_file_name, ".csv")
  save_as_csv(raw_tweets, file_name=query_file_csv)
  print("Done!")
  #rm(raw_tweets)
  #raw_tweets <- fread(query_file_csv, na.strings = c("",NA))
}