library(SnowballC)
class(woolworthtweet)
#clean the tweets
woolworthtweet$text <- gsub("https\\S*", "", woolworthtweet$text)
woolworthtweet$text  <-  gsub("@\\S*", "", woolworthtweet$text) 
woolworthtweet$text  <-  gsub("amp", "", woolworthtweet$text)
woolworthtweet$text  <-  gsub("[\r\n]", "", woolworthtweet$text)
woolworthtweet$text  <-  gsub("[[:punct:]]", "", woolworthtweet$text)
#need to further remove numbers, etc. 
woolworthtweet

#removing stop words
wtweets <- woolworthtweet %>%
  select(text) %>%
  unnest_tokens(word, text)


wtweets <- wtweets %>%
  anti_join(stop_words)

#stemming 

wtweets %>%
  count(word, sort = TRUE) %>%
  filter(str_detect(word, "^deliver"))

class(wtweets)

###
wtweetsCorpus <-Corpus(VectorSource(wtweets$word))
wtweetsCorpus <-tm_map(wtweetsCorpus, stemDocument)
wtweetsCorpus<- tm_map(wtweetsCorpus,removeNumbers)
inspect(wtweetsCorpus[1:20])
wtweets.df <- data.frame(word = sapply(wtweetsCorpus, as.character), stringsAsFactors = FALSE)
###
r <- wtweets %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  select(word)


wtweets %>% 
  mutate(word = wordStem(word)) %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
    labs(x = "Count",
         y = "Unique Words",
         title = "Count of Unique words found in tweets")
10:46 PM
Pratik Kias Napit
Pratik sent Today at 10:46 PM
#WooliesX Code Submission
#Libraries required for data analysis and NLP 
library(rtweet)
library(tm)
library(dplyr)
library(tidyr)
library(rvest)
library(tidytext)

#libraries required for plotting 
library(ggplot2)
library(ggthemes)

#other libraries used for data analysis, sentimental analysis, lemmatisation, NLP
#and machine learning
library(SnowballC)
library(utils)
library(graphics)
library(purrr)
library(stringr) 
library(syuzhet)
library(textstem)


#Twitter API Data 
api_key <- "gnn6kcBLrv7wJ09KsJJPyKo5Q"
api_secret_key <- "TKPiYTt06pbJFNNhwOecjjY0iZxdvXF5ssO3KLJXDIraxlLizz"
access_token <- "1407665327312236545-8vUxSDZPct9BJyGUbcvJIOspKKow6S"
access_secret <- "gHVqxTfyjst4XSl22n2EIEFyiwLsiVKq8nVW3qkv0p18V"

token <- create_token(
  app = "pratikn_wooliesx",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_secret)

woolworthtweet <- search_tweets(
  "@woolworths",n=600,lang = 'en',include_rts = FALSE,retryonratelimit=F)
woolworthtweet <- woolworthtweet %>% select(text)
tweets <- as_tibble(map_df(woolworthtweet, as.data.frame))
write.csv(tweets, file="tweets.csv", row.names=FALSE)  
tweets<-read.csv("tweets.csv")
names(tweets)[1] <- 'text'
twitterCorpus <-Corpus(VectorSource(tweets$text)) #499 documents
inspect(twitterCorpus[1:10])

#Review data from webscraping 
customer_reviews.df = data.frame()
for (page_result in seq(from = 1, to = 6)) {
  link = paste0("https://www.productreview.com.au/listings/woolworths?page=", page_result, "#reviews")
  page = read_html(link)
  review = page %>% 
    html_nodes("div+ .text-break_iZk .mb-0_wJE")%>% html_text() 
  
  customer_reviews.df = rbind(customer_reviews.df, data.frame(review, stringsAsFactors = FALSE)) 
  
  print(paste("Page:", page_result)) 
  
}
#transforming data into structured Corpus
write.csv(customer_reviews.df, "customer_reviews.csv")
cus_rev.corp <- Corpus(VectorSource(customer_reviews.df$review))
cus_rev.corp_original <-Corpus(VectorSource(customer_reviews.df$review)) #this is the original corpus
inspect(cus_rev.corp[24])

#Data Analysis and data visualisation 
#clean data
twitterCorpus<- tm_map(twitterCorpus, content_transformer(tolower))
#need to make a custom stopwords dictionary 
custom_stopwords = c("the","I","get", "store","will",
                     "tell", "ask", "shop",stopwords("english"))
twitterCorpus<- tm_map(twitterCorpus,removeWords,custom_stopwords)
twitterCorpus<- tm_map(twitterCorpus,removeNumbers)
twitterCorpus<- tm_map(twitterCorpus,removePunctuation)
removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus<- tm_map(twitterCorpus,content_transformer(removeURL))
removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus<- tm_map(twitterCorpus,content_transformer(removeURL))
twitterCorpus<- tm_map(twitterCorpus,stripWhitespace)
twitterCorpus<- tm_map(twitterCorpus, 
                       content_transformer(function(x) gsub(x, pattern = "deliver ", 
                                                            replace = "delivery ")))
twitterCorpusCOPY <- twitterCorpus
twitterCorpus <- twitterCorpusCOPY #this will undo everything that happens from here 

#lemmatization 
twitterCorpus = tm_map(twitterCorpus, lemmatize_strings)


#nrc sentimental analysis
emotions <-get_nrc_sentiment(twitterCorpus$content)
barplot(colSums(emotions),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)

#turn into DTM and then into tidy 
dtmT <- DocumentTermMatrix(twitterCorpus)
tidy_dtmT <- tidy(dtmT)#gives us one word per row -> use this to turn DTM to tibble

#graph of top 15 most repeated words--> might need to remove more stop words
tidy_dtmT %>%
  count(term, sort = TRUE) %>%
  top_n(5) %>%
  mutate(term = reorder(term,n)) %>%
  ggplot(aes(x = term, y = n)) +
  geom_col(width = 0.5) +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique Words",
       title = "Count of unique words found in tweets")

#sentimental analysis 
tweet_sentiment <- tidy_dtmT %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

tweet_sentiment %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col(width = 0.5) +
  guides(fill = FALSE) +
  labs(x = NULL, y = "Word Count") +
  ggtitle("Woolworths Twitter Bing Sentimental Scores") +
  My_Theme

#seperate data analysis of Twitter tweets 
#clean the tweets
woolworthtweet$text <- gsub("https\\S*", "", woolworthtweet$text)
woolworthtweet$text  <-  gsub("@\\S*", "", woolworthtweet$text) 
woolworthtweet$text  <-  gsub("amp", "", woolworthtweet$text)
woolworthtweet$text  <-  gsub("[\r\n]", "", woolworthtweet$text)
woolworthtweet$text  <-  gsub("[[:punct:]]", "", woolworthtweet$text)
#need to further remove numbers, etc. 
woolworthtweet

#removing stop words
wtweets <- woolworthtweet %>%
  select(text) %>%
  unnest_tokens(word, text)
wtweets <- wtweets %>%
  anti_join(stop_words)

#stemming 
wtweets %>%
  count(word, sort = TRUE) %>%
  filter(str_detect(word, "^deliver"))
class(wtweets)

###
wtweetsCorpus <-Corpus(VectorSource(wtweets$word))
wtweetsCorpus <-tm_map(wtweetsCorpus, stemDocument)
wtweetsCorpus<- tm_map(wtweetsCorpus,removeNumbers)
inspect(wtweetsCorpus[1:20])
wtweets.df <- data.frame(word = sapply(wtweetsCorpus, as.character), stringsAsFactors = FALSE)
###
r <- wtweets %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  select(word)

wtweets %>% 
  mutate(word = wordStem(word)) %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique Words",
       title = "Count of Unique words found in tweets")

#data anlysis of the reviews obtained from webscraping - this includes
#data cleaning, data analysis, sentimental analysis and data visualisation 
#Data Cleaning 
cus_rev.corp<- tm_map(cus_rev.corp, content_transformer(tolower))
#need to make a custom stopwords dictionary to remove stop words and context specific words 
custom_stopwords = c("the","I","get", "store","will",
                     "tell", "ask", "shop",stopwords("english"))
cus_rev.corp<- tm_map(cus_rev.corp,removeWords,custom_stopwords)
cus_rev.corp<- tm_map(cus_rev.corp,removeNumbers)
cus_rev.corp<- tm_map(cus_rev.corp,removePunctuation)
removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
cus_rev.corp<- tm_map(cus_rev.corp,content_transformer(removeURL))
removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
cus_rev.corp<- tm_map(cus_rev.corp,content_transformer(removeURL))
cus_rev.corp<- tm_map(cus_rev.corp,stripWhitespace)
cus_rev.corp<- tm_map(cus_rev.corp, 
                      content_transformer(function(x) gsub(x, pattern = "deliver ", 
                                                           replace = "delivery ")))

inspect(cus_rev.corp[1:10])
cus_rev.corpCOPY <- cus_rev.corp
cus_rev.corp <- cus_rev.corpCOPY#this will undo anything done from further out from here
test.corp <- cus_rev.corpCOPY

#lemmatize the strings 
cus_rev.corp = tm_map(cus_rev.corp, lemmatize_strings)
inspect(cus_rev.corp[1:10]) #this lemmatisation works fairly well


#sentimental analysis 
emotions <-get_nrc_sentiment(cus_rev.corp$content)
barplot(colSums(emotions),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)
#ggplot nrc sentiment -> we did not use this plot but left it in the code in case
sentimentscores<-data.frame(colSums(emotions[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

#cus_rev.corp<-tm_map(cus_rev.corp,stemDocument) #stemming cuts down alot of words that we actually dont want 
dtmc <- DocumentTermMatrix(cus_rev.corp)
tidy_dtmc <- tidy(dtmc)#gives us one word per row -> use this to turn DTM to tibble 

cus_sentiments <- tidy_dtmc %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) 
#grah to show words that contribute to positive and negative sentiment in woolworths review
cus_sentiments %>%
  count(term,sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  slice_max(n, n = 7) %>%
  ungroup() %>%
  mutate(word = reorder(term, n)) %>%
  filter(sentiment == 'negative') %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  #facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Count",
       y = "Unique Word",
       title = "Contribution to negative reviews and sentiment") +
  My_Theme

#graph for positive vs negative sentiment $need to fix
My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 18),
  plot.title = element_text(size = 22, face = "bold"))

#graph for bing sentimental scores for review
cus_sentiments %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col(width = 0.5) +
  guides(fill = FALSE) +
  labs(x = NULL, y = "Word Count") +
  ggtitle("Woolworths Bing Sentimental Scores for Reviews") +
  My_Theme

#gives us the top sentimental counts 
cus_sentiments %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count))

#find the most negative documents, find what customers are most unhappy about 
ap_sentiments <- tidy_dtmc %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
most_negativeTweets <- ap_sentiments %>%
  count(document, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(sentiment) %>%#gives us the documents which are most negative 
  filter(sentiment <= -4) %>%
  pull(document) #pulls most negative documents 
as.numeric(most_negativeTweets)
inspect(cus_rev.corp_original[86])

#graph of top 5 most repeated words--> might need to remove more stop words manually
#from the top 5
tidy_dtmc %>%
  count(term, sort = TRUE) %>%
  top_n(5) %>%
  mutate(term = reorder(term,n)) %>%
  ggplot(aes(x = term, y = n)) +
  geom_col(width = 0.5, fill = 'Light Green') +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = NULL,
       title = "Most mentioned words in negative reviews") +
  My_Theme
