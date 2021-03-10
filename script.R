library(tm)
library(readr)
library(ggplot2)
Train <- read_csv("Train.csv")
#View(Train)
str(Train)
attach(Train)
summary(Train)

Train$Label<-as.factor(Train$Label)
colnames(Train)

na_count1 <-sapply(Train, function(y) sum(length(which(is.na(y)))))
na_count1 <-data.frame(na_count1)
na_count1


table(Label)

prop.table(table(Label))
Train$Text_Length<-nchar(Train$Text)

hist(Train$Text_Length, col = rainbow(7))
ggplot(Train, aes(x=Text_Length, fill=Label))+
  geom_histogram()+
  stat_bin(bins = 30)+
  facet_wrap(~Label)

Text_Corpus<-Corpus(VectorSource(Train$Text))
print(Text_Corpus)
inspect(Text_Corpus[1:3])

Text_Corpus_Clean<-tm_map(Text_Corpus, tolower)

Text_Corpus_Clean<-tm_map(Text_Corpus_Clean, removeNumbers)

Text_Corpus_Clean<-tm_map(Text_Corpus_Clean, removePunctuation)

Text_Corpus_Clean<-tm_map(Text_Corpus_Clean, removeWords, stopwords())

Text_Corpus_Clean<-tm_map(Text_Corpus_Clean,stripWhitespace)

#?removeWords

inspect(Text_Corpus_Clean[1:2])

Text_dtm<-DocumentTermMatrix(Text_Corpus_Clean)

Text_dtm2<-DocumentTermMatrix(Text_Corpus_Clean, control = list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=TRUE,
  removePunctuation=TRUE,
  stripWhitespace=TRUE,
  stemDocument=TRUE
  ))

inspect(Text_dtm[1:10,10:15])


Sports_Cloud<-which(Train$Label=="SPORTS")
Transport_Cloud<-which(Train$Label=="TRANSPORT")
Wild_Env_Cloud<-which(Train$Label=="WILDLIFE/ENVIRONMENT")
RELATIONSHIPS_Cloud<-which(Train$Label=="RELATIONSHIPS")
RELIGION_Cloud<-which(Train$Label=="RELIGION")
SOCIAL_Cloud<-which(Train$Label=="SOCIAL")
SOCIAL_ISSUES_Cloud<-which(Train$Label=="SOCIAL ISSUES")
LOCALCHIEFS_Cloud<-which(Train$Label=="LOCALCHIEFS")
MUSIC_Cloud<-which(Train$Label=="MUSIC")
OPINION_ESSAY_Cloud<-which(Train$Label=="OPINION/ESSAY")
POLITICS_Cloud<-which(Train$Label=="POLITICS")
FARMING_Cloud<-which(Train$Label=="FARMING")
FLOODING_Cloud<-which(Train$Label=="FLOODING")
HEALTH_Cloud<-which(Train$Label=="HEALTH")
LAW_ORDER_Cloud<-which(Train$Label=="LAW/ORDER")
EDUCATION_Cloud<-which(Train$Label=="EDUCATION")
ECONOMY_Cloud<-which(Train$Label=="ECONOMY")
CULTURE_Cloud<-which(Train$Label=="CULTURE")
ARTS_CRAFTS_Cloud<-which(Train$Label=="ARTS AND CRAFTS")
WITCHCRAFT_Cloud<-which(Train$Label=="WITCHCRAFT")

library(wordcloud)

wordcloud(Text_Corpus_Clean[Sports_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[Transport_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[Wild_Env_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[RELATIONSHIPS_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[RELIGION_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[SOCIAL_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[SOCIAL_ISSUES_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[LOCALCHIEFS_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[MUSIC_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[OPINION_ESSAY_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[POLITICS_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[FARMING_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[FLOODING_Cloud], min.freq = 20)
wordcloud(Text_Corpus_Clean[HEALTH_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[LAW_ORDER_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[EDUCATION_Cloud], min.freq = 20)
wordcloud(Text_Corpus_Clean[ECONOMY_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[CULTURE_Cloud], min.freq = 40)
wordcloud(Text_Corpus_Clean[ARTS_CRAFTS_Cloud], min.freq = 10)
wordcloud(Text_Corpus_Clean[WITCHCRAFT_Cloud], min.freq = 10)
