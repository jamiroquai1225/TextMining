library(wordcloud2)
library(KoNLP)
library(tidyverse)
library(reshape2)
library(wordcloud2)
library(rJava)
library(Rcpp)
library(topicmodels)
library(tidyr)

useNIADic() # KoNLP 패키지에 있는 형태소사전(NIADic) 사용 선언 


# 사전에 사용자 단어를 명사(ncn)로 추가 
mergeUserDic(data.frame(c("현대상선, 대한해운, 흥아해운"),
                        "ncn"))

#뉴스 기사의 제목을 따로 추출
TEXTFILE <- X123$x

View(TEXTFILE)

library(tidytext)

head(TEXTFILE, 10)

# 텍스트 파일을 읽고, 품사를 구분하고, tidytext 패키지에서 사용 가능한 tibble 형태로 저장
tbl_TEXTFILE<-TEXTFILE%>%
  SimplePos09%>% # 품사구분함수, SimplePos09()는 9개 품사로, SimplePos22()는 22개 품사로 구분 
  melt%>% # 전체 자료를 tibble 형태로 저장 
  as_tibble%>% # 전체 자료를 tibble 형태로 저장 
  select(3,1) # 실제 분석에 필요한 3열과 1열만 따로 저장 

View(tbl_TEXTFILE)


# 명사형 자료만 골라내어 카운트 
tbl_TEXTFILECOUNT0 <- tbl_TEXTFILE %>% # tbl_TEXTFILE 데이터 사용 
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% # 명사형(/N) 자료만 골라낸 후 /N 빼고 저장 
  na.omit %>% # 비어있는 값(NA) 제외 
  filter(str_length(noun)>=2) %>%  # '것', '수' 와 같이 별 의미가 없는 의존명사를 제외하기 위하여 한글자 단어는 제외
  count(noun, sort=TRUE)

# 빈도 수, 제외 단어 필터링 
tbl_TEXTFILECOUNT1<-tbl_TEXTFILECOUNT0%>%
  filter(n>=5)%>%filter(!noun%in%c("기자","억원","이날","네이버", "재배포", "무단",
                                   "다양한","네이버에서","바로가기","저작권자", "무단전재","경우","지난해"
                                   ,"연합뉴스","뉴시스","구독하기", "이번", "기존", "우리", "올해", "이상", 
                                   "이후", "전재", "기존", "뉴스", "가운데"))

# 200대 키워드만 선정 
tbl_TEXTFILECOUNT2<-tbl_TEXTFILECOUNT1[1:200,]

# 워드클라우드 그리기 
wordcloud2(tbl_TEXTFILECOUNT2,fontFamily = "malgun gothic", size = 0.5, minRotation = 0, maxRotation = 0, backgroundColor = "black", color = "random-light")
