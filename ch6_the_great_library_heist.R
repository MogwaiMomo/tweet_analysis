# https://www.tidytextmining.com/topicmodeling.html

library(gutenbergr)
library(stringr)

titles1 <- c("Twenty Thousand Leagues under the Sea", 
            "The War of the Worlds")
titles2 <- c("Pride and Prejudice", 
            "Great Expectations")


books1 <- gutenberg_works(title %in% titles1) %>%
  gutenberg_download(meta_fields = "title")
books2 <- gutenberg_works(title %in% titles2) %>%
  gutenberg_download(meta_fields = "title")
books <- rbind(books1, books2)

books_no_pp <- books %>%
  filter(title != "Pride and Prejudice")

# PROBLEM - The following code snippet is not picking up Pride & Prejudice. Need to fix.

# PLAN: Isolate filter(books ... "pride and prejudice") and see if "^chapter " is picked up. 

book_pride_prejudice <- books %>%
  filter(title == "Pride and Prejudice") %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^chapter ", ignore_case = TRUE)))) %>%
  mutate(text_trim = str_trim(text)) %>%
  select(-c(text, chapter)) %>%
  rename(text = text_trim)
  
book_pride_prejudice <- book_pride_prejudice[, c(1, 3, 2)]
  
# subset the inverse of rows 12:138 (aka delete)

book_pride_prejudice <- book_pride_prejudice[-c(12:138),]

# recreate books df with ws-trimmed Pride and Prejudice
books <- rbind(books_no_pp, book_pride_prejudice)

by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^chapter ", ignore_case = TRUE)
    ))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

word_counts <- by_chapter_word %>% # chapters are the documents
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

# create a DTM using cast_dtm() from the tidytext package
chapter_dtm <- word_counts %>%
  cast_dtm(document, word, n)

# Problem solved!
chapter_dtm


#> A LDA_VEM topic model with 4 topics.
chapter_lda <- LDA(chapter_dtm, 4, control = list(seed = 1234)) # each topic corresponds to one of the books

chapter_topics <- tidy(chapter_lda, matrix = "beta") # word-topic probabilities

# find the top 5 terms within each topic
top5_terms_per_topic <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5) %>%
  ungroup() %>%
  arrange(topic, -beta)

# graph it
top5_terms_per_topic %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Next challenge: what's the best protocol for extracting topics from customer reviews?

# Work through this 

# https://medium.com/hackernoon/natural-language-processing-of-customer-reviews-49dff6fd9e57