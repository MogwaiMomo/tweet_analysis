# word cloud visualization
library(wordcloud)
create_wordcloud <- function (tidy_df, output_filename) {

  words <- tidy_df %>%
    count(word, sort = TRUE)
  
  png(filename=output_filename,
      width=500,
      height=500,
      units="px",
      res=140)

  wordcloud(words$word, 
            words$n, 
            random.order = FALSE, 
            max.words = 200, 
            color = alpha("blue", seq(0.1,1, 0.05))
            )
  
  dev.off()
}

