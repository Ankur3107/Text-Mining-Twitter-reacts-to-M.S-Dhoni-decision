library(dplyr)
library(tidytext)
library(tidyr)

dhoni <- read.csv("dhoni.csv")

Text=as.character(dhoni$Tweet.Text)

## refining 

library("tm") 
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

docs <- Corpus(VectorSource(Text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "®")
docs <- tm_map(docs, toSpace, "¤")
docs <- tm_map(docs, toSpace, "©")
docs <- tm_map(docs, toSpace, "ðYT")
docs <- tm_map(docs, toSpace, "¥")
docs <- tm_map(docs, toSpace, "???")

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("rt", "a","à","http","https","ðY","¥","???"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace) 
docs <- tm_map(docs, removeWords, c("ðÿ"))

tweets <- data.frame(text = sapply(docs, as.character), stringsAsFactors = FALSE)

#wordcloud

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##

Text=as.character(tweets$text)


tweets_dhoni <- data_frame(line = 1:25932, text = Text)

word_tweets_dhoni <- tweets_dhoni %>%
  unnest_tokens(word, text)

data(stop_words)

## word cloud

library(wordcloud)
word_tweets_dhoni %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

library(reshape2)

word_tweets_dhoni %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 200)

word_tweets_dhoni %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 50)



##

head(word_tweets_dhoni)

word_tweets_dhoni <- word_tweets_dhoni %>%
  anti_join(stop_words)

word_tweets_dhoni %>%
  count(word, sort = TRUE) 

library(ggplot2)

# words Frequency
word_tweets_dhoni %>%
  count(word, sort = TRUE) %>%
  filter(n > 3000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()


#N-gram

tweets_dhoni

tweets_dhoni_bigrams <- tweets_dhoni %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

tweets_dhoni_bigrams %>%
  count(bigram, sort = TRUE)

tweets_dhoni_bigrams_separated <- tweets_dhoni_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

tweets_dhoni_bigrams_filtered <- tweets_dhoni_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


tweets_dhoni_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

tweets_dhoni_bigrams_united <- tweets_dhoni_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")



#####

tweets_dhoni_bigrams_separated %>%
  filter(word1 == "best") %>%
  count(word1, word2, sort = TRUE)

tweets_dhoni_bigrams_separated %>%
  filter(word1 == "miss") %>%
  count(word1, word2, sort = TRUE)

tweets_dhoni_bigrams_separated %>%
  filter(word1 == "captain") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")


captain_words <- tweets_dhoni_bigrams_separated %>%
  filter(word1 == "captain") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()


captain_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylab("Words preceded by \"captain\"") +
  xlab("Sentiment score * #dhoni of occurrences") +
  coord_flip()


## 

positive_words <- c("best", "no", "captain", "not","like")

negation_words <- c("not", "no", "never", "without","like")


negated_words <- tweets_dhoni_bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

negated_words %>%
  mutate(contribution = n * score) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  xlab("Words preceded by negation") +
  ylab("Sentiment score * #dhoni of occurrences") +
  coord_flip()


#Visualizing a network of bigrams with igraph


tweets_dhoni_bigrams_counts <- tweets_dhoni_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

library(igraph)

tweets_dhoni_bigrams_graph <- tweets_dhoni_bigrams_counts %>%
  filter(n > 500 & n < 3000) %>%
  graph_from_data_frame()

tweets_dhoni_bigrams_graph

library(ggraph)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(tweets_dhoni_bigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
