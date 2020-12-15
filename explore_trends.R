isolate_quants <- function(df) {
 df %>% 
    select_if(function(col) is.numeric(col) | 
               all(col == .$element_id)) -> quants
   return(quants)
}
 
isolate_quals <- function(df) {
  df %>% 
    select_if(function(col) is.factor(col) | 
                all(col == .$element_id)) -> quals
  return(quals)
}

isolate_texts <- function(df) {
  df %>% 
    select_if(function(col) is.character(col) | 
                all(col == .$element_id)) -> texts
  return(texts)
}



  # # function to summarize key stats - NEED TO CLEAN THIS UP
  # get_stats <- function(quants){
  #   
  #   # get min, max, mean, etc.
  #   stats.table <- as.df.frame.matrix(summary(quants))
  #   names(stats.table) <- trimws(names(stats.table))
  #   # ugly hack to calculate sd & append to the summary table
  #   # get sd as vector
  #   stats.sd <- df.frame("sd" = sapply(quants, sd)) %>%
  #     round(digit = 4)
  #   # reformat
  #   formatted.stats.sd <- stats.sd %>%
  #     # preserve var names
  #     as.df.table(keep.rownames = T) %>%
  #     # add formatting to match summary table
  #     mutate(sd = paste("Sd : ", sd)) %>%
  #     # transpose to wide
  #     t %>% df.table(keep.rownames = T) %>%
  #     # drop redundant first col
  #     dplyr::select(-1)
  #   # assign first row to table names
  #   names(formatted.stats.sd) <- unlist(formatted.stats.sd[1,])
  #   # get rid of redundant first row
  #   final <- formatted.stats.sd[2,]
  #   
  #   # STILL TO DO: append sd to stats table
  #   
  #   stats.table.tmp <- rbind(stats.table[1:4,], final)
  #   stats.table <- rbind(stats.table.tmp, stats.table[5:6,])
  #   
  #   return(stats.table)
  # }
  # 
  # stats.summary <-  get_stats(quants) 
  # 
  # # 3. Graph & explore variables 
  # 
  # ## Plot all factor-based graphs
  # 
  # # set grid of plots 1rx3c
  # par(mfrow=c(1,3))
  # attach(df)
  # boxplot(mpg ~ cylinders, xlab = "cyl")
  # boxplot(mpg ~ year, xlab = "year")
  # boxplot(mpg ~ origin, xlab = "origin")
  # 
  # 
  # ## plot all single-var & two-var quant graphs
  # 
  # library(PerformanceAnalytics)
  # png(filename="plot.png", width = 1480, height = 1480)
  # plot2 <- chart.Correlation(quants,hist=T) 
  # dev.off()
  


# 
# 
# ### Interesting questions for analysis
# 
# # How are sentiment scores in this sample distributed?  
# 
# ## 1. Significance test for normality for sent scores (H0 = is normal)
# 
# ### First, get a smaller representation for shapiro test (5000 max)
# shap_sample <- sample_n(sa_tweets, 500)
# 
# ### Get significance
# shapiro.test(shap_sample$ave_sentiment)$p.value # result: not normal. 
# 
# ## 2a. Visualize as histogram
# ggplot(sa_tweets, aes(ave_sentiment)) +
#   geom_histogram(fill="black", colour="black", alpha = 0.25, binwidth=0.05) + 
#   geom_density(aes(y=0.05*..count..), colour="black", adjust=4) +
#   geom_vline(aes(xintercept = mean(ave_sentiment)), colour="red") +
#   theme(legend.position="none")
# 
# 
# ## 2b. Visualize as a boxplot (verified vs. unverified)
# ggplot(sa_tweets, aes(x=verified, y=ave_sentiment)) +
#   geom_boxplot(fill="black", colour="black", alpha = 0.25) + 
#   theme(legend.position="none")
# 
# # How does average sentiment change over time?
# 
# # Step 1. Categorize by day




#### NLP STUFF #######

# Get Top 5 summary tables 
#top5_summaries(tweets, query_file_name)

# Generate nrc sentiment analysis
# omit_words <- c(query, "vote") # adjust as needed
# nrc_words <- get_sentiments_words(tweets, omit_words, "nrc", "joy")
# create_wordcloud(cloud_words, "output/nrc_cloud.png")
# 
# # Generate sentiment analysis with AFINN
# afinn_words <- get_sentiments_words(tweets, omit_words, "afinn", "negative")
# create_wordcloud(afinn_words, "output/afinn_cloud.png")


