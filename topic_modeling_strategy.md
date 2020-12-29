# Plan of attack for finding most relevant topics associated with good vs. bad emotions in product reviews: 

1. Scrape a bunch of reviews and/or survey responses about a product into a data file.
2. Make sure there is a column for 'document' giving a unique ID to each document that is preserved in all downstream data objects.
3. Use sentimentR package to separate into sentences and get sentiment scores per sentence, then get the average sentiment per document. See "sentiment_analysis.R" in this repo for the custom function you made for this. 
4. Create a tidy version of the data (schema: document | term | count)
5. Create a tdm  version of the data
6. Generate a topic model for the entire sample
7. If possible, figure out how to calculate the sentiment score associated with each topic (is this the right way to do this?)

OVERALL QUESTION / OBJECTIVE: 

What topics are most tightly associated with positive sentiment? Negative sentiment? What is the right kind of test(s) we should use to figure this out? 

