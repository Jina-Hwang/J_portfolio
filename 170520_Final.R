setwd("C:/Users/choib/Desktop/R directory")

install.packages("httr")
install.packages("rvest")
install.packages("KoNLP") 
install.packages("dplyr")
install.packages("tm")
install.packages('lda')
install.packages('topicmodels')
install.packages('LDAvis')
install.packages('servr')
install.packages("wordcloud2")
install.packages("RColorBrewer")


useSejongDic()

library(KoNLP)  
library(tm) 
library(httr)
library(rvest)
library(dplyr)
library(tm)
library(lda)
library(stringr)
library(topicmodels)
library(LDAvis)
library(servr)
library(wordcloud2)

base_url='http://search.daum.net/search?nil_suggest=btn&w=news&DA=SBC&cluster=y&q=%EC%9D%B4%ED%99%94%EC%97%AC%EB%8C%80'

all.reviews = c()
for(i in 1:100) {
  print(i) 
  url = paste(base_url, i, sep="")
  r=GET(url)
  h=read_html(r)
  comment_area=html_nodes(h, '.wrap_cont')
  comments=html_nodes(comment_area, '.wrap_tit')
  reviews=html_text(comments)
  if(length(reviews) == 0) {break} 
  all.reviews = c(all.reviews, reviews)
  print(i)
}
all.reviews 

data1<-all.reviews 
data1

data2 <- sapply(data1, extractNoun, USE.NAMES=F) 

data3<-unlist(data2)
data3<-Filter(function(x){nchar(x)>=2},data3)


data3 <- gsub("[]]","",data3) # ] 삭제
data3 <- gsub("[[]","",data3) # [ 삭제
data3 <- gsub("[^[:alnum:][:space:]]", "", data3) # 글자, 숫자, 스페이스 외에 삭제
data3 <- gsub("[c(]","",data3) # c( 삭제
data3 <- gsub("문재","문재인",data3) # '문재'를 '문재인'으로 바꿈
data3 <- gsub("먼저다","",data3) 
data3 <- gsub("신촌골에","신촌골",data3)
data3 <- gsub("이대생이","이대생",data3)
data3 <- gsub("똑똑","",data3)
data3 <- gsub("11명으","11명",data3)
data3 <- gsub("생명으","생명",data3)
data3 <- gsub("신촌캠퍼스에","신촌캠퍼스",data3)
data3 <- gsub("평창올림","평창올림픽",data3)
data3 <- gsub("신촌캠퍼","신촌캠퍼스",data3)
data3 <- gsub("신촌캠퍼스스","신촌캠퍼스",data3)
data3 <- gsub("서울환경영화","서울환경영화제",data3)
data3 <- gsub("자궁경","자궁경부암",data3)
data3 <- gsub("자궁경부","자궁경부암",data3)
data3 <- gsub("한국교","한국교회",data3)
data3 <- gsub("래미안편한세","래미안편한세상",data3)
data3 <- gsub("[^가-힣]","",data3) # 한 글자로 된 텍스트 제거
data3

data3 <- str_trim(data3) # str_trim()으로 텍스트 앞 뒤 공백 제거
nouns <- sapply(data3, extractNoun, USE.NAMES = F) # extractNoun으로 의미있는 단어만 추출
unlist_nouns <- unlist(nouns) #unlist를 통해 데이터 구조를 바꿈
filtered <- Filter(function(x){nchar(x) >2 & nchar(x) <=8},unlist_nouns) # 2이상 8이하의 단어로 추림
wordcount <- table(filtered) # wordcloud를 사용하기 위해 table로 변환해줌


### 워드클라우드2 실행
#wordcloud2(data=wordcount, minSize = 4, shuffle=F)

# 워드클라우드2 (비둘기)
figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(data=wordcount, figPath = figPath, size = 1.5,color = "skyblue")

# 워드클라우드2 (letterCloud)
#letterCloud(data=wordcount, word = "Ewha", size = 10)

# 워드클라우드2 (letterCloud)
#letterCloud(data=wordcount, word = "R", size = 10)

# 워드클라우드2
#wordcloud2(data=wordcount,minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
           #rotateRatio = 1)

# 워드클라우드2
#wordcloud2(data=wordcount, color = "random-light", backgroundColor = "grey")

# 워드클라우드2 (letterCloud)
#letterCloud(data=wordcount, word = "R", color='random-light' , fontFamily = "微软雅黑",backgroundColor="black")
#letterCloud(data=wordcount, word = "PEACE", color="white",fontFamily = "微软雅黑", backgroundColor="pink")


### tdm 만들기

doc.list <- list(data3)

my.corpus <- Corpus(VectorSource(doc.list))

# 한글 2글자 이상만 추출하기, 다른 옵션도 어떻게 적용되는지 관찰
# tdm <- TermDocumentMatrix(my.corpus, control=list(removePunctuation=T, removeNumbers=T, wordLengths=c(2,6), weighting=weightBin))
tdm <- TermDocumentMatrix(my.corpus, control=list(removePunctuation=T, removeNumbers=T, wordLengths=c(3,6), weighting=weightBin))

m <-as.matrix(tdm)
m

# 간격 조절하여, 빈공간 메꾸기.

write(unlist(data3),"이화여대.txt")
write.csv(data3,"13조이화여대.csv")
data4<-read.table("이화여대.txt")

nrow(data4) # 변수에 데이터가 몇 건이 있는지 조회
data4
View(as.data.frame(data4))


wordgraph<-table(data4) # 단어별 글자 수 테이블로 변환
head(sort(wordcount,decreasing=T),20) # 내림차순으로 20개 정렬
a<-head(sort(wordgraph,decreasing=T),20) # 'a'에 내림차순 20개 정렬 저자
a



### 파이차트만들기
pie(a) #파이차트
pie(a,col=rainbow(10),radius=1.5) #파이차트(크기 및 색상지정)
pct<-round(a/sum(a)*100,1) #파이차트의 변수 별 퍼센트
names(a)  # (a의 변수 이름들)
lab<-paste(names(a),"\n",pct,"%") #변수이름과 퍼센트를 'lab'에 곳에 저장
pie(a,main="Top 20 keywords of Ewha Univ",col=rainbow(10),cex=1,labels=lab)
#파이차트의 제목은  main으로 지정, col은 무지개색 10가지로 지정, 라벨은 'lab'에 저장했던것 처럼 이름과 퍼센트로 적음)
par(new=T)

recommand<-head(sort(wordcount,decreasing=T),15) #'recommand'라는 곳에 wordcount 내림정렬 15개를 저장
recommand


### 바차트만들기
bp<-barplot(a,main="Top 15 keywords",col=rainbow(10),cex.names=1.2,las=2,ylim=c(0,120))
#'bp'라는 곳에 바차트 저장 (제목="Top 15 keywords", 색상=무지개10가지색, y축=0부터120까지 )
pct<-round(a/sum(a)*20,1)
text(x=bp,y=a*1.05,labels=paste(a,"건"),col="black",cex=0.7) #라벨에 건수 추가입력
text(x=bp,y=a*0.10,labels=paste("(",pct,"%",")"),col="black",cex=0.7) #라벨에 퍼센트 추가입력

wordlist<-read.table("이화여대.txt")
str(wordlist)
head(wordlist, 10)
nrow(wordlist) 

data5<-table(wordlist)
data5

write.csv(data5,"이화여대다음.csv")
head(sort(data5, decreasing=T),30)


#A topic model for Social reviews(소셜리뷰를 위한 토픽모델)

#토픽 모델을 피팅하기 전에 텍스트를 토큰화해야한다. 
#이데이터 세트는 이미 매우 깨끗하기 때문에 구두점과 일부 일반적인 중지 단어 만 제거합니다. 


# tokenize on space and output as a list (공백으로 토큰화하고 리스트로 출력)

# read in some stopwords:
#Return various kinds of stopwords with support for different languages.

library(tm)

doc.list <- strsplit(data3, "[[:space:]]+")

doc.list

### compute the table of terms: (terms를 테이블 화하여 계산)
term.table <- table(unlist(doc.list))
term.table

term.table <- sort(term.table, decreasing = TRUE)
term.table

### remove terms that are stop words or occur fewer than 5 times:
#(5번 반복 미만의 단어나 stop word 는 삭제하시오.)


stop_words = c('http','www','img','border','color','style','padding','table','font','thi','inch','ha','width','height')
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)
vocab

# 5단계: 토픽모델 적합
corpusLDA <- lexicalize(vocab)   
# lda 토픽분석을 수행하는 lda.collapsed.gibbs.sampler() 함수를 적용하기 위해 corpus 형태로 저장 

ldaModel <- lda.collapsed.gibbs.sampler(corpusLDA$documents, K=3, vocab=corpusLDA$vocab, burnin=999, num.iterations=1000, alpha=0.05, eta=0.01)
# K: 토픽 수, alpha, eta : lda 토픽분석 과정에 필요한 하이퍼패러미터, burnin 으로 설정된 반복 (iteration) 수는 전체 반복 수에 포함되지 않음

str(ldaModel)
# lda 수행 결과를 저장한 ldaModel 변수에는 assignments(indicating the topic assignment for each word in each document), topics(the number of times a word was assigned to a topic) 등의 정보가 저장됨

# 6단계: 결과 보기 및 시각화
ldaModel$topics  
# 각 단어가 토픽에 할당된 횟수, K*V 행렬 (K:토픽 수, V: vocab 에 포함된 단어 수)

ldaModel$assignments   
# 문서 안에 포함된 단어가 어떤 토픽에 할당되었는지를 나타내는 벡터로 이루어진 리스트, 
# 리스트의 길이: D (D: 문서의 총 수)

ldaModel$document_sums   
# 각 문서에 포함된 단어들이 각 토픽에 할당된 횟수, K*D 행렬 (K:토픽 수, D: 문서의 총 수)

top.words <- top.topic.words(ldaModel$topics, 5, by.score=TRUE) # 토픽별로 상위 5개의 단어를 추출

print(top.words)    # 앞에서 설정한 토픽 수가 3이므로 세 개의 토픽에 대해 상위 5개 단어 출력


corpusLDA$vocab <- gsub("[c][(]", "", corpusLDA$vocab)
corpusLDA$vocab <- gsub("\"", "", corpusLDA$vocab)
corpusLDA$vocab <- gsub(")", "", corpusLDA$vocab)
corpusLDA$vocab <- gsub(",", "", corpusLDA$vocab)
# 출력된 단어에 c( , ) \ 등이 붙는 것이 싫다면 위 코드를 실행하고 아래는 똑같이 반복
ldaModel <- lda.collapsed.gibbs.sampler(corpusLDA$documents,K=3,vocab=corpusLDA$vocab,burnin=999, num.iterations=1000,alpha=0.05,eta=0.01)
top.words <- top.topic.words(ldaModel$topics, 5, by.score=TRUE)




### now put the documents into the format required by the lda package:
#(lda 패키지에 필요한 형식으로 문서를 넣으십시오.)

get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)



### Compute some statistics related to the data set:
#데이터 세트와 관련된 일부 통계를 계산

D <- length(documents)  # number of documents (다큐먼트의 수계산)
W <- length(vocab)  # number of terms in the vocab(용어의 어휘 수 계산)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens in the data (데이터의 총 토큰수)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus (코퍼스안의  terms 빈도수)

#다음으로, 주제 - 기간 분포 ($ \ eta $ = 0.02)와 문서 - 주제 분포 
#($ \ alpha $ = 0.02)에 대해 상대적으로 분산 된 3개의 주제로 주제 모델을 설정하고, 
#축소 된  샘플러 5,000 회 반복 실행.

### Model tuning parameters:
K <- 3     # number of topics(토픽수)
G <- 5000    # number of iterations(반복횟수)
alpha <- 0.02    
eta <- 0.02


### Fit the model:MCMC and model tuning parameters:

#Set.seed 설명
#의사 랜덤 넘버 생성을 위해 R에서 set.seed ()를 사용한다. 
#또한 set.seed (123)와 같은 숫자를 사용하면 결과를 재현 할 수 있음을 알 수 있습니다.. 
#몇 가지 기능을 가지고 일부는 set.seed (1) 또는 set.seed (300) 또는 set.seed (12345)를 사용합니다.
#예를 들어, 내가 작업중인 책에서 의사 결정 트리에 대한 교육 세트를 만들 때 set.seed (12345)를 사용합니다. 
#그런 다음 다른 장에서 set.seed (300)를 사용하여 임의의 포리스트를 만듭니다.
#한 눈에 보기에도 중복되는 난수가 없는데, 우연히 중복된 난수가 나올 가능성은 20억 분의 1 이하입니다.
#이런경우 , 난수를 생성하고 다시 그값의 난수를 불러오고 싶을때, set seed(#)사용.

library(lda)
set.seed(357) #랜덤넘버 357 넣어줌 (Set.seed 런덤 넘버 제너레이션 툴)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # time elapsed for calculating,# about 24 minutes on laptop


#적합 모델을 LDAvis로 시각화


theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))  # apply(): function operation to each row(1) or cloumn(2)
# matrix, with each row containing the probability distribution over topics for a document, with as many rows as there are documents 
# in the corpus, and as many columns as there are topics in the model
theta

phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
# matrix, with each row containing the distribution over terms for a topic, with as many rows as there are topics in the model, 
# and as many columns as there are terms in the vocabulary
phi


Ewha <- list(phi = phi,
                      theta = theta,
                      doc.length = doc.length,
                      vocab = vocab,
                      term.frequency = term.frequency)


##이미 문서 당 토큰 수와 전체 코퍼스에서의 용어 빈도를 계산했습니다. 
## $ \ phi $, $ \ theta $ 및 vocab과 함께 LDAvis 패키지에 포함 된 데이터 객체 SocialReviews로 목록에 저장합니다.


options(encoding = 'UTF-8')      # for processing Korean
# options(): set and examine a variety of global options which affect the way in which R computes and displays its results


library(LDAvis)


##이제 LDAvis에서 createJSON () 함수를 호출 할 준비가되었습니다. 
##이 함수는 시각화를 채우는 데 사용되는 JSON 객체를 나타내는 문자열을 반환합니다. 
##createJSON () 함수는 주제 빈도, 주제 간 거리 및 주제를 2 차원 평면에 투영하여 서로의 
##유사성을 나타냅니다. 또한 각 주제에 대해 용어의 순위가 결정되는 방식을 제어하는 튜닝 
##매개 변수 $ 0 \ leq \ lambda \ leq 1 $의 값 격자를 반복합니다. 여기서 용어는 관련성이 
##감소하여 나열되며 여기서 용어 $ w 주제 $ t $는 $ \ lambda \ times p (w \ mid t) + (1 - \ lambda) \ times p (w \ mid t) / p (w) $로 정의됩니다. 
##$ \ lambda $의 값이 1에 가까울수록 주어진 주제 내에서 자주 관련성이 높은 순위가 부여되는 
##반면, 0에 가깝게 $ \ lambda $의 값은 주제 내에서 독점 용어와의 관련성 순위가 높습니다. 
##각 주제에 대해 가장 관련성 높은 상위 용어들 중에서 순위가 매겨진 모든 용어 집합은 
##createJSON () 함수에 의해 사전 계산되어 JSON 객체의 일부로 D3을 사용하여 대화식으로 
##시각화되도록 브라우저로 전송됩니다.

###serVis () 함수는 json을 가져 와서 다양한 방식으로 결과를 제공 할 수 있습니다. 
###여기서 우리는 json을 'vis'디렉토리 내의 파일에 씁니다 
#(페이지를 렌더링하는 데 필요한 다른 HTML 및 JavaScript와 함께). 여기서 결과를 볼 수 있습니다.

### create the JSON object to feed the visualization:
json <- createJSON(phi = Ewha$phi, 
                   theta = Ewha$theta, 
                   doc.length = Ewha$doc.length, 
                   vocab = Ewha$vocab, 
                   term.frequency = Ewha$term.frequency, encoding='UTF-8')

serVis(json, out.dir = 'vis', open.browser = TRUE)
# json: character string output from createJSON, out.dir: directory to store html/js/json files- 'vis' directory created in working directory, open.browser: Should R open a browser?

data5<-table(wordlist)

wordlist<-read.table("이화여대.txt")
str(wordlist)
head(wordlist, 10)
nrow(wordlist) 


data5<-table(wordlist)

write.csv(data5,"이화여대Final.csv")
head(sort(data5, decreasing=T),30)
