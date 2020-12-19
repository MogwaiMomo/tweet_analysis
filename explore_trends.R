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

   
# 3. Graph & explore variables

## quant plots
plot_quants <- function(file, df, omit_var=NULL) {
  png(filename=file, width = 1480, height = 1480)
  if (!is.null(omit_var)) {
    df %>%
      select(-all_of(omit_var)) -> df
  }
  plot <- chart.Correlation(df,hist=T)
  dev.off()
}


# group histogram

#https://community.rstudio.com/t/how-to-turn-strings-from-function-arguments-into-column-names-with-dplyr/19438

plot_group_histogram <- function(df, factor_var, num_var) {
  factor_var <- sym(factor_var)
  num_var <- sym(num_var)
  
  df %>%
    mutate(num_var = !!num_var) %>%
    mutate(factor_var = !!factor_var) %>%
    group_by(factor_var) -> grouped_df
  
  plot <- grouped_df %>%
    ggplot(aes(x=num_var, fill=factor_var)) +
    geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity')
  
  return(plot)
  
}



calculate_foo <- function(df, passed_column1, passed_column2){
  column_sym <- sym(passed_column1)
  print(column_sym)
  #df %>% 
    #mutate(new_column2 = !!column_sym)
}




# set grid of plots 1rx3c
# par(mfrow=c(1,3))
# 
#   boxplot(mpg ~ cylinders, xlab = "cyl")
#   boxplot(mpg ~ year, xlab = "year")
#   boxplot(mpg ~ origin, xlab = "origin")



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


