# Get as many tweets of a given hashtag as possible from the last week. (May time out due to too many tweets.)
pull_max_tweets <- function(query, end_date) {
  hashtag <- paste0("#", query)
  
  # initial pre-loop conditions
  last_status_id <-  NULL
  final_data <- data.frame()
  x <- 0
  # loop through for as many days as is permitted
  repeat {
    x <- x + 1
    print(paste0("Starting Round ", x, " ..."))
    data <- search_tweets(q = hashtag,
                              include_rts = FALSE,
                              `-filter` = "replies",
                              lang = "en",
                              retryonratelimit = TRUE,
                              max_id = last_status_id
                              )
    print(paste0("Done pulling tweets for Round ", x, " ..." ))
    last_status_id <- str_remove(data[nrow(data),]$status_id, "x")
    print(paste0("Last id: ", last_status_id, " ..." ))
    last_date <- str_split(data[nrow(data),]$created_at, " ")[[1]][1]
    print(paste0("Last date: ", last_date, " ..." ))
    loop_result <- isTRUE(last_date <= end_date)
    print(loop_result)
    # append collected data iteration to final set
    final_data <- rbind(final_data, data)
    print(paste0("Done Round ", x, " ..."))
    if (last_date <= end_date) {
      return(final_data)
      break
    }
  }
}




# date.string <- as.character(Sys.Date())
# query_file_name <- paste0(query, "_", end_date, "_to_", date.string,   "_tweets")
# print("Saving as CSV ...")
# query_file_csv <- paste0(query_file_name, ".csv")
# save_as_csv(final_data, file_name=query_file_csv)
# print("Done! Reading csv back in for more usable data ...")
# rm(final_data)
# final_data <- fread(query_file_csv, na.strings = c("",NA))