a1 <- cus_sentiments %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) 
class(a1)


ag <- ggplot() +
geom_col(data = a1,aes(sentiment, word_count, fill = sentiment)) +
geom_col(data = a2,aes(sentiment, word_count, fill = sentiment)) +
guides(fill = FALSE) +
labs(x = "NULL", y = "Word Count") +
ggtitle("Woolworths Bing Sentimental Scores for Reviews") +
My_Theme
ag

a2 <- tweet_sentiment %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count))
  
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col(width = 0.5) +
  guides(fill = FALSE) +
  labs(x = NULL, y = "Word Count") +
  ggtitle("Woolworths Twitter Bing Sentimental Scores")
  
#web scraping data for Woolies reveiws 
library(rvest)
library(dplyr)
library(tidytext)

#retrieving reviews for Woolworth through Product Review Website 
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

test <- tibble(customer_reviews.df)
paired <- test %>%
  select(review) %>%
  mutate(review = removeWords(review, stop_words$word)) %>%
  mutate(review = gsub("\\brt\\b|\\bRT\\b", "", review)) %>%
  mutate(review = gsub("http://*", "", review)) %>%
  unnest_tokens(paired_words, review, token = "ngrams", n = 2)
s = paired %>%
  count(paired_words, sort = TRUE)
write.csv(s, "paired_words.csv")

inspect(cus_rev.corp[24])

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

#can we lemmatize the strings 
library(textstem)
cus_rev.corp = tm_map(cus_rev.corp, lemmatize_strings)
#cus_rev.corp = tm_map(cus_rev.corp, PlainTextDocument) 
inspect(cus_rev.corp[1:10]) #this lemmatisation works fairly well


#sentimental analysis 
emotions <-get_nrc_sentiment(cus_rev.corp$content)
barplot(colSums(emotions),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)
#ggplot nrc sentiment 
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

#cluster analysis 
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

a <- cus_sentiments %>%
  count(term,sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  slice_max(n, n =5) %>%
  ungroup() %>%
  mutate(word = reorder(term, n)) %>%
  filter(sentiment == 'negative')
install.packages("wordcloud")
library(wordcloud)
wordcloud(word = a$word, freq = a$n, random.order = FALSE,
          rot.per = 0.35, color = factor(cyl))


#graph for positive vs negative sentiment $need to fix
My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 18),
  plot.title = element_text(size = 22, face = "bold"))

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

#graph of top 15 most repeated words--> might need to remove more stop words
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

#twitterAPI only provides tweets for the last 6-9 days 
#API key 
#gnn6kcBLrv7wJ09KsJJPyKo5Q
#API secret key 
#TKPiYTt06pbJFNNhwOecjjY0iZxdvXF5ssO3KLJXDIraxlLizz
#Bearer Token
#AAAAAAAAAAAAAAAAAAAAAErYQwEAAAAASxIOtqFGLlsDdhwE%2BFfwBD3aP7s%3DrtGNgsV622OlWB0zWXIdmaOML1jw82bczb7n9iaPYRjJPV2fer 
#Access Token 
#1407665327312236545-8vUxSDZPct9BJyGUbcvJIOspKKow6S
#Access Token Secret
#gHVqxTfyjst4XSl22n2EIEFyiwLsiVKq8nVW3qkv0p18V


install.packages("rtweet") # for harvesting tweets
install.packages("tm") # for text mining
install.packages("ggthemes")
install.packages("here")
install.packages("syuzhet")
install.packages("SnowballC")
library(rtweet)
library(tm)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(here)
library(SnowballC)
library(utils)
library(graphics)
library(purrr)
library(stringr) 
library(syuzhet)
library(tidytext)

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


wool_deliverytweet <- search_tweets(
  "@woolworths + delivery",n=600, lang = 'en', include_rts = FALSE,retryonratelimit=F)
wool_deliverytweet <- wool_deliverytweet %>% select(text)

tweets <- as_tibble(map_df(woolworthtweet, as.data.frame))
write.csv(tweets, file="tweets.csv", row.names=FALSE)  
tweets<-read.csv("tweets.csv")
names(tweets)[1] <- 'text'

tweetsdel <- as_tibble(map_df(wool_deliverytweet, as.data.frame))
write.csv(tweetsdel, file="tweetsd.csv", row.names=FALSE)  
tweetsdel<-read.csv("tweetsd.csv")
names(tweetsdel)[1] <- 'text'

twitterCorpus <-Corpus(VectorSource(tweets$text))
twitterDelCorpus <-Corpus(VectorSource(tweetsdel$text))
#data cleaning 
inspect(twitterCorpus[1:10])
inspect(twitterDelCorpus[1:10])
removeUserWords <- function(x) {
  gsub(pattern = "\\@\\w*", "", x)
}
twitterCorpus<- tm_map(twitterCorpus,content_transformer(removeUserWords))
twitterCorpus<- tm_map(twitterCorpus, content_transformer(tolower))
twitterCorpus<- tm_map(twitterCorpus,removeWords,stopwords("english"))
twitterCorpus<- tm_map( twitterCorpus,removeNumbers)
twitterCorpus<- tm_map( twitterCorpus,removePunctuation)
removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus<- tm_map(twitterCorpus,content_transformer(removeURL))
removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus<- tm_map(twitterCorpus,content_transformer(removeURL))

#removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
#twitterCorpus<-tm_map(twitterCorpus,content_transformer(removeNonAscii))
#twitterCorpus<- tm_map(twitterCorpus,removeWords,c("amp","ufef",
#"ufeft","uufefuufefuufef","uufef","s"))  
twitterCorpus<- tm_map(twitterCorpus,stripWhitespace)
inspect(twitterCorpus[1:10])
#twitterCorpus <- tm_map(twitterCorpus, removeWords,c("put", "got"))

#Term Document Matrix 
tdm <- TermDocumentMatrix(twitterCorpus)
tdm <- as.matrix(tdm)
tdm[1:20, 1:20]

w <- rowSums(tdm)
w <- subset(w, w >= 13)
barplot(w,
        las = 2,
        col = rainbow(50))

#Sentimental analysis 

emotions<-get_nrc_sentiment(twitterCorpus$content)

barplot(colSums(emotions),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)
get_sentiment(twitterCorpus$content[1:10])

#cluster analysis 
#Stem Document 
dict_twitterCorpus <- twitterCorpus
twitterCorpus<-tm_map(twitterCorpus,stemDocument)#stemming cuts down a lot of words that we actually dont want 
twitterCorpus<-tm_map(twitterCorpus,  content_transformer(stemCompletion), dictionary = dict_twitterCorpus)


inspect(twitterCorpus[1:10])
dtm <- DocumentTermMatrix(twitterCorpus)
inspect(dtm[1:2, 1001:1007])
XMA Header Image
Woolworths
productreview.com.au
Pratik Kias Napit
Pratik sent Today at 4:37 PM
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