# word cloud visualization
library(wordcloud)
create_wordcloud <- function (df, output_filename) {
  
  words <- df %>%
    mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
           text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
           text = str_remove_all(text, "[^\x01-\x7F]")) %>%
    unnest_tokens(word, text, token = "tweets") %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]"),
           !str_detect(word, "^#"),
           !str_detect(word, "@\\S+")) %>%
    filter(word != query) %>%
    count(word, sort = TRUE)
  
  
  # create color gradient for word cloud
  cloud_filename <- paste0(query_file_name, "_wordcloud.png")
  
  png(filename=cloud_filename,
      width=500,
      height=500,
      units="px")
  wordcloud(words$word, words$n, random.order = FALSE, max.words = 100, color = alpha("blue", seq(0.4,1, 0.05)))
  
  dev.off()
}

