library(rtweet)
library(stringr)
tweets_df <- search_tweets(q = "패션", n = 10000, type = "mixed")

writexl::write_xlsx(tweets_df, "../data/tweets.xlsx")

df <- readxl::read_excel("C:/Users/Jinho/Desktop/123.xlsx")
DT::datatable(head(X123$x, n = 500), 
              class = 'cell-border stripe', 
              options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE))

library(stringr)
doc <- X123$x
docs <- str_replace_all(doc, "[^0-9a-zA-Zㄱ-ㅎㅏ-ㅣ가-힣[:space:]]", " ")
docs <- str_replace_all(docs, "[\n\t]", " ")
docs <- str_trim(docs)
docs <- str_replace_all(docs, "\\s+", " ")

View(docs)

library(tm)
library(topicmodels)
install.packages("textmineR")

install.packages("SentimentAnalysis")

library(textmineR)

library(tm)   

library(SentimentAnalysis)

useSejongDic()

corp <- VCorpus(VectorSource(docs))

tdm <- TermDocumentMatrix(corp, 
                          control = list(wordLengths = c(1,Inf), 
                                         tokenize = function(x) {
                                           ngram_tokenize(x, char = F)
                                         }))
tail(Terms(tdm))

head(Terms(tdm))

library(tidyverse)

# 희소단어 제거
tdm_r <- removeSparseTerms(tdm, sparse = 0.95) # X 
head(Terms(tdm), 20)

wordFreq <- slam::row_sums(tdm)
wordFreq_df <- data.frame(words = names(wordFreq), 
                          freq  = wordFreq)

remove_chars <- c("t", "co", "t co", "https", "https t", "https t co") # X
wordFreq_df2 <- wordFreq_df %>% 
  filter(!(words %in% remove_chars))

# Plot
theme_set(theme_bw(base_family = ""))
ggplot(wordFreq_df2 %>% filter(freq > 100), aes(reorder(words, freq), freq)) + 
  geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  coord_flip() + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

library(ggplot2)

senti_words_kr <- readr::read_delim("~/Rstudy/SentiWord_Dict.txt", delim='\t', col_names=c("term", "score"))
head(senti_words_kr)

dim(senti_words_kr)

table(senti_words_kr$term)[1:10]

x <- duplicated(senti_words_kr$term)
senti_words_kr2 <- senti_words_kr[!x, ]
senti_dic_kr <- SentimentDictionaryWeighted(words = senti_words_kr2$term, 
                                            scores = senti_words_kr2$score)
senti_dic_kr <- SentimentDictionary(senti_words_kr2$term[senti_words_kr2$score > 0], 
                                    senti_words_kr2$term[senti_words_kr2$score < 0])

summary(senti_dic_kr)

senti_words_kr$term[duplicated(senti_words_kr$term)]

res_sentiment <- analyzeSentiment(corp, #대신에 corpus,
                                  language="korean",
                                  rules=list("KoreanSentiment"=list(ruleSentiment, senti_dic_kr)),
                                  removeStopwords = F, stemming = F)

df2 <- data.frame(round(res_sentiment, 3), df)

View(df2)

theme_set(theme_minimal(base_family = ""))

df3 <- df2 %>% 
  mutate(pos_neg = if_else(KoreanSentiment >= 0, "Positive", "Negative")) %>%
  select(pos_neg, everything())

View(df3)

DT::datatable(head(df3 %>% select(x, KoreanSentiment), n = 500),
              class = 'cell-border stripe', 
              options = list(pageLength = 5,autoWidth = TRUE, scrollX = TRUE))


ggplot(df3, aes(x = factor(pos_neg))) + 
  geom_bar(stat = "count", width = 0.7, fill = "steelblue") + 
  theme_minimal()

library(writexl)

help("writexl")

df3 <- write_xlsx(df3, path="~/Rstudy/df3.xlsx")


