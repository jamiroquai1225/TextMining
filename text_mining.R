# https://github.com/dgrtwo/tidy-text-mining

# Chapter 1 정돈 텍스트 형식 

text<-c("Because I Could not stop for-", 
        "He kindly stopped for me-", 
        "The Carriage held but just Ourselves-", 
        "and Immortality")

text

install.packages('dplyr')
library(dplyr) # 정돈 도구 tibble 사용

text_df<-tibble(line=1:4, text=text)
text_df

install.packages("tidytext")
library(tidytext) # 정돈 데이터 구조 unnest_token 사용 
text_df%>% 
  unnest_tokens(word, text) # unnest_tokens 각 행마다 토큰(단어)이 한 개만 있도록 각 행이 분할됨 

install.packages("janeaustenr")

library(janeaustenr) # 제인오스틴이 탈고한 출판 소설 
library(dplyr)
library(stringr)

original_books<-austen_books()%>%
  group_by(book)%>%
  mutate(linenumber=row_number(), # mutate() linenumber 수에 해당하는 만큼 주석으로 처리함 
         chapter=cumsum(str_detect(text, regex("^chapter [\\divxlc]", # chapter(regex 사용해 모든 장이 어디서 나오는지 확인)
                                               ignore_case = TRUE))))%>% 
  ungroup()

original_books

library(tidytext)
tidy_books<-original_books%>%
  unnest_tokens(word, text) #unnest_tokens() 함수를 사용해 1행당 1토큰 형식으로 구조를 다시 구성 

tidy_books

data(stop_words)

tidy_books<-tidy_books%>% 
  anti_join(stop_words) # anti_join을 활용해 불용어(the, of, to) 제거 
# tidytext 패킵지의 stop_words 데이터 셋에는 세 개의 불용어 용어집이 있음 

tidy_books%>%
  count(word, sort=TRUE)

install.packages("ggplot2")

library(ggplot2)

tidy_books%>% # 상위 단어 수 그래프 그리기 
  count(word, sort=TRUE)%>%
  filter(n>600)%>%
  mutate(word=reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
  
install.packages("gutenbergr")
library(gutenbergr)

hgwells<-gutenberg_download(c(35,36,52,30,159)) # gutenberg_download()를 이용하여 작품 엑세스 

tidy_hgwells<-hgwells%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

tidy_hgwells%>%
  count(word, sort=TRUE)

bronte<-gutenberg_download(c(1260,768,969,9182,767))

tidy_bronte<-bronte%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

tidy_bronte%>%
  count(word, sort=TRUE)

tidy_bronte%>% # 상위 단어 수 그래프 그리기 
  count(word, sort=TRUE)%>%
  filter(n>600)%>%
  mutate(word=reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

install.packages("tidyr")
library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte, author="Brontë Sisters"),
                       mutate(tidy_hgwells, author="H.G. Wells"),
                       mutate(tidy_books, author="Jane Austen"))%>%
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`) 

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"), 
                       mutate(tidy_hgwells, author = "H.G. Wells"),  
                       mutate(tidy_books, author = "Jane Austen")) %>%  
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n / sum(n)) %>%  
  select(-n) %>%  
  spread(author, proportion) %>%  
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)


install.packages("scales")

library(scales)

# 결측값이 제거된 행에 대한 경고가 표시될 수 있음
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2)+
  geom_jitter(alpha = 0.1, size=2.5, width=0.3, height=0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  scale_color_gradient(limits = c(0,0.001),
                       low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) + 
  geom_abline(color = "gray40", lty = 2) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10(labels = percent_format()) + 
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") + 
  facet_wrap(~author, ncol = 2) + 
  theme(legend.position="none") + 
  labs(y = "Jane Austen", x = NULL) 

# 단어 집합의 유사성과 차이점을 상관 검정을 통해 정량화 시킴. 단어 빈도 상관 분석 
cor.test(data = frequency[frequency$author == "Brontë Sisters",], 
         ~ proportion + `Jane Austen`) 

cor.test(data = frequency[frequency$author == "H.G. Wells",],  
         ~ proportion + `Jane Austen`) 

# Chapter 2 정돈 데이터를 사용한 정서분석 

library(tidytext)
library(textdata)

sentiments # tidytext에 포함

# get_sentiments() 함수를 사용해 특정용어집 엑세스 
get_sentiments("afinn") 

get_sentiments("bing")

get_sentiments("nrc")

# 내부조인(inner_join)을 활용한 정서분석

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrcjoy <- get_sentiments("nrc") %>% # nrc 용어집 사용 
  filter(sentiment == "joy") 

tidy_books %>%
  filter(book == "Emma") %>% # Emma 에서 기쁨을 가장 흔하게 나타내는 단어
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE) # dplyr의 count 사용 

library(tidyr)

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

# 각 소설 속 줄거리의 자취를 따라서 정서 점수들을 그래프로 그려냄 
ggplot(janeaustensentiment, aes(index, sentiment, fill = book )) +
  geom_col(show.legend =  FALSE ) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")

pride_prejudice


# 정수 나눗셈(%/%)을 사용해 여러 줄에 걸쳐 있는 더 큰 텍스트 부분을 정의하고 count(), spread() 및 mutate()와 같은 패털을 사용해
# 이들 각 텍스트 부분들에서 순수한 정서를 찾아냄 

afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# 소설 본문 중 각 정서 용어집을 적용해 순수한 정서를 추정함 
bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# 용어집에 몇 개의 긍정 단어와 부정 단어가 있는지 살펴봄
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)


# count()에 word와 sentiment를 사용함으로써 각 단어가 정서에 얼마나 기여했는지 알아봄
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

custom_stop_word <- bind_rows(data_frame(word = c("miss"),
                                         lexicon = c("custom")),
                              stop_words)

custom_stop_word

library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)

# 정서분석을 통한 내부 조인을 사용해 긍정 단어와 부정 단어에 태그를 추가한 다음 가장 흔한 긍정 단어와 부정 단어를 찾음
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

PandP_sentences <- data_frame(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")

PandP_sentences$sentence[2]  

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVCXLC]") %>%
  ungroup()

# 텍스트를 각 장별로 데이터 프레임으로 분리 
austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n())

bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

# 각 도서에서 negative 비율이 가장 큰 챕터 
tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter !=0) %>%
  top_n(1) %>%
  ungroup()


# Chapter 3 단어와 문서의 빈도 분석:tf-idf 

library(dplyr)
library(janeaustenr)
library(tidytext)

# 제인오스틴의 소설에서 가장 일반적으로 사용되는 단어 

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)  

book_words  
  
# 소설에서 단어가 나타나는 횟수를 해당 소설의 총단어 수로 나눈 값(용어 빈도)

library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")


# 지프의 법칙에 따르면 단어가 나타나는 빈도는 순위에 반비례 함

# 지프의 법칙

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank  

# 지프의 법칙은 x축의 순위와 y축의 용어 빈도를 로그 척도에 맞춰서 그린다
# 제인오스틴 소설에 대한 지프의 법칙 

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()
  
# 멱법칙의 지수가 순위 범위의 중간 부분에 해당하는지 확인 

rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)  

# 제인오스틴의 소설로 지프의 법칙에 대한 지수를 적합하게 함 

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()
  

# tf-idf 계산

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words  

# 제인오스틴의 작품에서 tf-idf가 높은 용어 

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#  제인오스틴의 소설에서 tf-idf 값이 가장 큰 단어

book_words %>% arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# 물리학 텍스트의 말뭉치 

library(gutenbergr)  
physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
                              meta_fields = "author")

# 각 텍스트에서 각 단어가 몇 번이나 사용되었는지 살펴보기 위해 unnest_tokens(), count() 사용 

physics_words <- physics %>% 
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

physics_words

# tf-idf를 계산한 다음, tf-idf가 큰 단어를 시각화 

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
# 물리텍스트에서 tf-idf가 가장 큰 단어 

plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
  
# 불용어 제거 

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn",
                                   "fig", "file", "cg", "cb", "cm"))

physics_words <- anti_join(physics_words, mystopwords, by = "word")

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x= NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()


# Chpater 4 단어 간 관계: 엔그램과 상관 

# 엔그램에 의한 토큰화

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
austen_bigrams

# 엔그램 개수 세기와 선별하기

austen_bigrams %>%
  count(bigram, sort = TRUE)

# 불용어 제거 

library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# 새 바이그램 카운트: 
  
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts  

# 여러 열을 하나로 재결합 

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# 트라이그램(서로 이어져 있는 세 단어) 분석 

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
  
# 바이그램 분석

# 텍스트(street)의 탐색적 분석 

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1,sort = TRUE)

# 도서에 나오는 바이그램의 tf-idf 값 시각화

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf


# tf-idf를 계산한 다음, tf-idf가 큰 단어를 시각화 

# 제인 오스틴의 각 소설에서 tf-idf가 가장 큰 열두개 바이그램 

bigram_tf_idf %>% 
 arrange(desc(tf_idf)) %>% 
 group_by(book) %>% 
 top_n(12, tf_idf) %>% 
 ungroup() %>% 
 mutate(bigram = reorder(bigram, tf_idf)) %>% 
 ggplot(aes(bigram, tf_idf, fill = book)) + 
 geom_col(show.legend = FALSE) + 
 facet_wrap(~ book, ncol = 2, scales = "free") + 
 coord_flip() + 
 labs(y = "tf-idf of bigram to novel", 
      x = "") 

# 정서분석 시 바이그램을 사용해 문맥 제공하기 

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# afinn 용어집을 사용해 각 단어에 대한 정서 점수를 숫자로 부여 

AFINN <- get_sentiments("afinn")

AFINN

# not이 앞서 나오면서도 정서와 관련이 있는 가장 빈출하는 단어를 찾아냄 

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) %>%
  ungroup()

not_words  

# 어느 단어가 잘못된 방향으로 가장 많이 기여했는지 분석
# 출현횟수에 * 점수를 곱하여 확인 

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment Value * Number of occurrences") +
  coord_flip()
  

# 특정 단어 뒤에 나오는 가장 흔어가 무엇인지 시각화

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup()

negated_words  
  
# negated_words 시각화(x)
negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment Value * Number of occurrences") +
  coord_flip()  

# negated_words 시각화 
negated_words %>% 
  mutate(contribution = n * value, 
               word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>% 
  group_by(word1) %>% 
  top_n(12, abs(contribution)) %>% 
  ggplot(aes(word2, contribution, fill = n * value > 0)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ word1, scales = "free") + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) + 
  xlab("Words preceded by negation term") + 
  ylab("Sentiment value * # of occurrences") + 
  coord_flip() 

# ggraph를 사용해 바이그램 연결망 시각화하기 

library(igraph) # 연결망 분석 패키지

# 원래 카운트

bigram_counts

# 상대적으로 흔한 조합만을 선별하는 필터

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph) # 네트워크 

set.seed(2017)

# 정점레이어에 연결선 레이어, 텍스트 레이어를 덧붙임

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# 연결망 시각화 업그레이드

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) + 
  geom_node_point(color = "light blue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# 그 밖의 텍스트에 들어 있는 바이그램 시각화하기 

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort= TRUE) 
  }

visualize_bigrams <- function(bigrams) {
  set.seed(2016) 
  a <- grid::arrow(type = "closed", length = unit(.15, "inches")) 

  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void() 
  }



# 구텐베르크 프로젝트 바이그램 시각

library(gutenbergr)

kjv <- gutenberg_download(10)

library(stringr)

kjv <- bigrams <- kjv %>%
  count_bigrams()

# 드물게 출협하는 조합을 걸러 내고, 숫자에 대해서도 걸러냄

kjv <- bigrams %>%
  filter(n > 40, 
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()


# widyr 패키지와 단 어 쌍 세기 및 상관 

# 각 단원 간의 개수 세기 및 상관

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

library(widyr)

# pairwise_는 word 변수의 각 단어 쌍에 대해 하나의 행을 생선한다는 것을 의미함. 한 개 단원 안에 흔하게 나오는 단어 쌍을 셀 수 있음

# 각 절들 간에 동시 발생하는 단어를 세기

word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

word_pairs %>%
  filter(item1 == "darcy")

# 쌍 단위 상관 검사
# Widyr의 Pairwise_cor() 함수를 사용하면 동일한 절에 나타나는 빈도에 따라 단어 사이의 파이 계수를 찾을 수 있음

# 최소한 상대적으로 흔한 단어를 먼저 선별해야 함

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# pounds와 가장 관련성이 높은 단어 

word_cors %>%
  filter(item1 == "pounds")

# 여러 단어들으 선택하고 관련성이 높은 그 밖의 단어

word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~item1, scales = "free") +
  coord_flip()

# 찾은 단어의 상관과 군집을 시각화 

set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


# Chapter 5 비정돈 형식 간에 변환하기 

# DocumentTermMatrix 객체 정돈하기 

library(tm)

data("AssociatedPress", package = "topicmodels")

AssociatedPress

View(AssociatedPress)

terms <- Terms(AssociatedPress)

head(terms)

library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)

ap_td


# 감정 분석 

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

library(ggplot2)

# Bing 정서 용어집을 사용하는 경우에 긍정 정서나 부정 정서에 가장 큰 기여를 한 AP 기사의 단어 

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200)%>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

# dfm 객체 정돈하기 

library(methods)
install.packages("quanteda")

data("data_corpus_inaugural", package = "quanteda")

inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)
inaug_dfm

# tidy 메서드는 이러한 문서-특징 행렬에서도 작동해 이를 1행당 1문서, 1문서당 1토큰으로 변화시킬 수 있음

inaug_td <- tidy(inaug_dfm)
inaug_td

# bind_tf_idf()를 활용해 각 용어-연설 쌍의 tf-idf를 계산해 정량화 할 수 있음

inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

# 네 개의 선택된 취임식 연설 각각에서 tf-idf가 가장 큰 용어 

speeches <- c("1933-Roosevelt", "1861-Lincoln", 
              "1961-Kennedy", "2009-Obama") 


inaug_tf_idf %>% 
  filter(document %in% speeches) %>% 
  group_by(document) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, tf_idf, document)) %>% 
  ggplot(aes(term, tf_idf, fill = document)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ document, scales = "free") + 
  coord_flip() + 
  scale_x_reordered() + 
  labs(x = "", y = "tf-idf") 


# 이름에서 연도를 추출하고 해마다 전체 단어 수를 계산

library(tidyr)

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count)) 

year_term_counts %>%
  filter(term %in% c("america", "foreign", "union", 
                     "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")


# 정돈 텍스트 데이터를 행렬에 캐스팅하기 

# 문서-용어 행렬로 캐스팅

ap_td %>%
  cast_dtm(document, term, count)

# 테이블을 quanteda의 dfm에서 dfm 객체로 캐스팅

ap_td %>%
  cast_dfm(document, term, count)

# 희소 행렬

library(Matrix)

# matrix 객체로 캐스팅해 넣기

m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)

dim(m)

# 이 캐스팅 과정을 통해 dplyr 및 기타 정돈 도구를 사용해 읽고 선별하고 처리한 후 데이터를 머신러닝 응용프로그램용 
# 문서-용어 행렬로 변환할 수 있다. 

library(janeaustenr)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm 

# Corpus 객체를 메타데이터로 정돈하기 

data("acq")

acq

acq[[1]] # 첫 번째 문서 

acq_td <- tidy(acq)

acq_td

# 50개의 로이터 기사를 찾거나 가장 구체적인 기사를 찾음 

acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# 가장 흔한 단어들

View(acq_tokens)

acq_tokens %>%
  count(word, sort = TRUE)

#td-idf 

acq_tf_idf <- acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))


# 사례 연구: 금융 관련 기사 마이닝 (구글, 야후 서비스 종료)

# 기술 관련 주식과 관련된 최근 기사 검색

install.packages("tm.plugin.webmining")
library(tm.plugin.webmining)
library(tm)
library(purrr)
library(tidyr)
library(tidytext)
library(rJava)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook", 
             "Twitter", "IBM", "Yahoo", "Netfilx")

symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")

download_articles <- function(symbol) {
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
  }

stock_articles <-tibble(company = company, 
                        symbol = symbol) %>% 
  mutate(corpus = map(symbol, download_articles))

stock_articles

stock_tokens <- stock_articles %>%
  unnest(map(corpus, tidy) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp), word, id, heading)

stock_tokens

library(stringr)

stork_tf_idf <- stock_tokens %>%
  count(company, word) %>%
  filter(!str_detect(word, "\\d+")) %>%
  bind_tf_idf(word, company, n) %>%
  arrnage(-tf_idf)

# 정서 분석

stock_tokens %>%
  anti_join(stop_words, by = "word") %>%
  count(word, id, sort = "TRUE") %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(contribution = sum(n * sum)) %>%
  top_n(12, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution)) +
  geom_col() +
  coord_flip() +
  labs(y = "Frequency of word * AFINN score")

# 금융 정서 용어집

stock_tokens %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ sentiment, scale("free")) +
  ylab("Frequency of this word in the recent financial articles")

# 정서 관련 단어의 사용 횟 수 계산

stock_sentiment_count <- stock_tokens %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  count(sentiment, company) %>%
  spread(sentiment, n, fill = 0)

stock_sentiment_count

# 어떤 회사가 논쟁적이거나 불확실한 용어들이 들어 있는 뉴스를 가장 많이 내는지 분석 

stock_sentiment_count %>%
  mutate(score = (positive - negative) / (positive + negative)) %>%
  mutate(company = reorder(company, score)) %>%
  ggplot(aes(company, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Company",
       y = "Positivity score among 20 recent news articles")



# Chapter 6 토픽모델링 

# 잠재 디리클레 할당(LDA) 분석

library(topicmodels)

data("AssociatedPress")  

AssociatedPress  

# topicmodels 패키지의 LDA() 함수를 사용해 k=2로 설정하면 2 토픽 LDA 모델을 만들 수 있음

# 시드(seed) 값을 정해 모델의 결과를 예측할 수 있게 함

ap_lda <- LDA(AssociatedPress, k = 5, control = list(seed = 1234))

# 토픽이 두 개인 LDA_VEM 토픽 모델 
ap_lda  

# 단어-토픽 확률

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")

ap_topics

# 토픽 모델링 시각화

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() 

# 대안으로 토픽 1과 토픽 2 사이의 베타 값의 차가 가장 큰 용어들을 고려할 수 있음
# 이는 로그 비율을 기반으로 추정함

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread  

# 토픽 1, 2의 베타가 가장 큰 단어 

beta_spread %>% 
  group_by(direction = log_ratio > 0) %>% 
  top_n(10, abs(log_ratio)) %>% 
  ungroup() %>% 
  mutate(term = reorder(term, log_ratio)) %>% 
  ggplot(aes(term, log_ratio)) + 
  geom_col() + 
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") + 
  coord_flip() 

# 문서 토픽 확률, 감마는 토픽당 문서당 확률 

ap_documents <- tidy(ap_lda, matrix = "gamma")

ap_documents

# 해당 문서에서 가장 흔한 단어가 무엇인지 확인

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

# 대도서관 강도 

library(gutenbergr)
library(dplyr)
library(tidyr)
library(tidytext)

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

library(stringr)

# 각기 1개 장을 대표하는 문서들로 나눈다. 

reg <- regex("^chapter", ignore_case = TRUE)
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, reg))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter) # tidyr 패키지 열어아 작동함 

# 단어들을 분리한다.

by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text) # tidy_text 패키지를 열어아 작동함 

# 문서-단어 카운트를 알아낸다.

word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()
  
word_counts  

# 각 장의 LDA

# tidytext의 cast_dtm()을 이용해 1행당 1토큰 테이블을 DocumentTermMatrix로 캐스팅

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)
  
chapters_dtm

# LDA() 함수를 사용해 원하는 갯 수의 토픽을 만들 수 있음 

library(topicmodels)

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))

chapters_lda

# 단어 당 토픽 확률 조사

chapters_topics <- tidy(chapters_lda, matrix = "beta")

chapters_topics

# dplyr의 top_n()을 사용해 각 토픽 내에서 상위 다섯 개 용어를 찾을 수 있음

top_terms <- chapters_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

library(ggplot2)  

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

# 문서 당 분류

# 토픽 별 문서 당 확률인 감마를 검토하여 어떤 토픽이 각 문서와 관련되어 있는가 확인

chapters_gamma <- tidy(chapters_lda, matrix = "gamma") 

chapters_gamma

# 문서 당 토픽 당 확률을 시각화 

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

# 그림을 그리기 전에 토픽 1, 토픽 2, 등의 순서에 따라 제목(title)의 순서를 바꾼다. 

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

# 일부 장이 다른 토픽과 다소 연관되어 있는 것 처럼 보일 때 각 장과 가장 관련이 있는 토픽 확인

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications  

# 잘못 식별되는지 확인

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic) 

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

# 단어 별 할당 augment 

# 원본 문서-단어 쌍을 가져와서 각 문서에서 어떤 단어가 어떤 토픽에 할당되었는지 분석 

assignments <- augment(chapters_lda, data = chapters_dtm)

assignments

# assignments 테이블을 consensus라는 도서 이름과 결합해 어떤 단어가 잘못 분류되었는지 분석

assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic")) 

assignments  

library(scales)  

# LDA가 각 도서의 단어를 할당한 곳을 보여주는 혼동 행렬 
# 이 표의 각 행은 각 단어의 출현 사실을 나타내고 각 열은 배정된 도서를 나타냄 

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to", 
       y= "Book words came from",
       fill = "% of assignments")

# 가장 일반적으로 잘못된 단어는 무엇인가 

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))


# 잘못된 분류 단어

word_counts %>%
  filter(word == "flopson")

# 대체 LDA 구현


install.packages("mallet")
library(mallet)

# 1개 장당 1개 문자열을 사용해 벡터를 생성한다. 

collapsed <- by_chapter_word %>% 
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "`", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = ""))

# 불용어 넣을 빈 파일을 생성한다. 

file.create(empty_file <- tempfile()) 
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)  
mallet_model$loadDocuments(docs)  
mallet_model$train(100)

# 단어-토픽 쌍들

tidy(mallet_model)

# 문서-토픽 쌍들

tidy(mallet_model, matrix = "gamma")

# 'augment'에 대한 열의 이름이 'term'으로 되어야 함

term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)

# 사례 연구: 트위터 아카이브 비교 

# 사례 연구: NASA 메타데이터 마이닝 

library(jsonlite)

metadata <- fromJSON("https://data.nasa.gov/data.json")

names(metadata$dataset)  

class(metadata$dataset$title)

class(metadata$dataset$description)

class(metadata$dataset$keyword)

library(dplyr)

nasa_title <- data_frame(id = metadata$dataset$'@type',
                         title = metadata$dataset$title)
nasa_title <- random(nasa_title$id)

View(nasa_title)

head(nasa_title, 5)

nasa_desc <- data_frame(id = metadata$dataset$'@type',
                        desc = metadata$dataset$description)

nasa_desc %>%
  select(desc) %>%
  sample_n(5)

library(tidyr)

nasa_keyword <- data_frame(id = metadata$dataset$'@type',
                           keyword = metadata$dataset$keyword) %>% 
  unnest(keyword) # 중요어를 위한 정돈 데이터 프레임 

nasa_keyword 

library(tidytext)

nasa_title <- nasa_title %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) # 데이터 토큰

nasa_desc <- nasa_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words) # 데이터 토큰 

nasa_title

nasa_desc
  
nasa_title %>%
  count(word, sort = TRUE)

nasa_desc %>%
  count(word, sort = TRUE)

my_stopwords <- data_frame(word = c(as.character(1:10),
                                    "v1", "v03", "l2", "l3", "l4", "v5.2.0", 
                                    "v003", "v004", "v005", "v006", "v7")) # 불용어 목록 생성 

nasa_title <- nasa_title %>%
  anti_join(my_stopwords) # 불용어 제거 

nasa_desc <- nasa_desc %>%
  anti_join(my_stopwords) # 불용어 제거 

nasa_keyword %>%
  group_by(keyword) %>%
  count(sort = TRUE)

nasa_keyword <- nasa_keyword %>%
  mutate(keyword = toupper(keyword)) # 대문자 소문자로 변환 

# 단어 동시 발생과 상관

library(widyr)

nasa_title$id <-rnorm(n=230370, mean=0, sd=1)
nasa_title


title_word_pairs <- nasa_title %>%
  pairwise_count(word, id, sort =TRUE, upper = FALSE) # pairwise_count()를 사용해 각 단어 쌍이 제목 필드 또는 설명 필드에 

title_word_pairs

