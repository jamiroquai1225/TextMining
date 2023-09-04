install.packages("arulesViz")

library(arules)     # read.transactions()함수 제공
library(arulesViz)  # 시각화를 위한 패키지

# transactions 데이터 가져오기
data("Groceries")  # 식료품점 데이터 로딩

library(KoNLP)
library(arules)
library(igraph)
library(combinat)

rules <- X123$x

head(rules, 10)

tran <- Map(extractNoun, rules)

tran1 <- unique(tran)

tran2 <- sapp

head(tran, 10)

tran <- sapply(tran, unique)

tran <- sapply(tran, function(x) {Filter(function(y)
  
{nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)})

tran <- Filter(function(x) {length(x) >=2}, tran)

tran

names(tran) <- paste("Tr", 1:length(tran), sep="")

names(tran)

wordtran <- as(tran,"transactions")

wordtran

wordtab <- crossTable(wordtran)

wordtab

# 최대 길이 3이내로 규칙 생성
rules <- apriori(wordtran, parameter=list(supp=0.001, conf=0.80, maxlen=3))
inspect(rules) # 29개 규칙

# confidence(신뢰도) 기준 내림차순으로 규칙 정렬
rules <- sort(rules, decreasing=T, by="confidence")
inspect(head(rules)) 

library(arulesViz) # rules값 대상 그래프를 그리는 패키지
plot(rules, method="graph", control=list(type="items"))

View(Groceries)
