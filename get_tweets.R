# get as many tweets of a given hashtag as possible from the last week. (May time out due to too many tweets.)

pull_tweets <- function(query) {
  hashtag <- paste0("#", query)
  
  # initial pre-loop conditions
  end_date <- Sys.Date() - 7
  last_status_id <-  NULL
  final_data <- data.frame()

  # loop through for as many days as is permitted
  repeat {
    data <- search_tweets(q = hashtag,
                              include_rts = FALSE,
                              `-filter` = "replies",
                              lang = "en",
                              retryonratelimit = TRUE,
                              max_id = last_status_id
                              )
    last_status_id <- str_remove(data[nrow(data)]$status_id, "x")
    # append latest data iteration to final set
    final_data <- rbind(final_data, data)
    df_date <- str_split(data[nrow(data)]$created_at, " ")[[1]][1]
    if (df_date == end_date) {
      break
    }
  }
  
  date.string <- as.character(Sys.Date())
  query_file_name <- paste0(query, "_", date.string, "_to_", end_date, "_tweets")
  print("Saving as CSV ...")
  query_file_csv <- paste0(query_file_name, ".csv")
  save_as_csv(raw_tweets, file_name=query_file_csv)
  print("Done! Reading csv back in for more usable data ...")
  rm(raw_tweets)
  raw_tweets <- fread(query_file_csv, na.strings = c("",NA))
}