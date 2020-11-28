# sentiment analysis
get_sentiments_words <- function(text_df, omit_words, lexicon, emotion=NULL){
  # create tidy tokens for analysis
  tidy_tweets <- create_tidy_tokens(text_df, "text", omit_words)
  
  if (lexicon == "afinn") {
    print("don't have afinn-based analysis ready yet.")
  } else {
    sent_dataset <- get_sentiments(lexicon) %>%
      filter(sentiment == emotion)
    
    sentiment_words <- tidy_tweets %>%
      inner_join(sent_dataset) %>%
      count(word, sort = T)
  }
  
  return(sentiment_words)
}  

