# working through the following tutorial on LDA topic modeling
# https://www.tidytextmining.com/topicmodeling.html

library("topicmodels")
library("tm")

# Step 1: create a TDM object from my collection of tweets using the TM package (try going from tidy objects to TDMs and back)

# Step 1a: create a tidy object from the 'texts' df

texts %>%
  select(-hashtags) %>% # remove hashtags variable, which we're not using
  unnest_tokens(word, text) %>%
  count(element_id, word, sort = TRUE) %>%
  rename(document = element_id, term = word, count = n)  -> texts_td
  
# Step 1b: create  TDM using the cast function

texts_td %>%
  cast_dtm(document, term, count) -> texts_dtm





