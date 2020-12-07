library("sentimentr")

# sentiment analysis
get_sentiments_words <- function(text_df, omit_words, lexicon, emotion){
  # create tidy tokens for analysis
  tidy_tweets <- create_tidy_tokens(text_df, "text", omit_words)
  
  if (lexicon == "afinn") {
    
    if (emotion == "positive") {
      sent_dataset <- get_sentiments(lexicon) %>%
        filter(value >= 3) 
      
      sentiment_words <- tidy_tweets %>%
        inner_join(sent_dataset) %>%
        count(word, sort = T)
      return(sentiment_words)
      
    } else if (emotion == "negative") {
      sent_dataset <- get_sentiments(lexicon) %>%
        filter(value <= -3) 
      
      sentiment_words <- tidy_tweets %>%
        inner_join(sent_dataset) %>%
        count(word, sort = T)
      
      return(sentiment_words)
    }
  } else {
    sent_dataset <- get_sentiments(lexicon) %>%
      filter(sentiment == emotion)
    
    sentiment_words <- tidy_tweets %>%
      inner_join(sent_dataset) %>%
      count(word, sort = T)
    
    return(sentiment_words)
  }
}  

# function for clustering by sentiment using sentimentR (sentence-level analysis)

# https://cran.r-project.org/web/packages/sentimentr/readme/README.html#preferred-workflow


# calculate sentiment for each tweet 
tweets %>% 
  get_sentences() %>%
  sentiment_by(by = 'element_id') -> sent_by_tweet # assign sentiment by sentence element

# outer-join tweets with sent by tweet
tweets %>%
  full_join(sent_by_tweet, by = 'element_id') -> full_tweets



# Interesting questions for analysis

# what proportion of tweets (element_id) are positive? neutral? negative?




  
  


  



