setwd("D:/ANALITYKA GOSPODARCZA/Magisterka/Semestr I/Data mining/Zadanie 1/teksty")
wd <- "D:/ANALITYKA GOSPODARCZA/Magisterka/Semestr I/Data mining/Zadanie 1/teksty"
dir(wd)

library(tm)

docs <- Corpus(DirSource(wd))

docs
DocumentTermMatrix(docs)


writeLines(as.character(docs[[1]]))

getTransformations()


docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))


for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("–", " ", docs[[j]])
  docs[[j]] <- gsub("’", " ", docs[[j]])
  docs[[j]] <- gsub("“", " ", docs[[j]])
  docs[[j]] <- gsub("…", " ", docs[[j]])
  docs[[j]] <- gsub("‘", " ", docs[[j]])
  docs[[j]] <- gsub(")", " ", docs[[j]])
  docs[[j]] <- gsub("”", " ", docs[[j]])
}


writeLines(as.character(docs[[1]]))


docs <- tm_map(docs, tolower)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))


length(stopwords("english"))
stopwords("english")


docs <- tm_map(docs, removeWords, stopwords("English"))
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))




StW<-read.table("D:/ANALITYKA GOSPODARCZA/Magisterka/Semestr I/Data mining/Stopwords/StopWords.txt")
StW

StWW<-as.character(StW$V1)
StWW


docs <- tm_map(docs, removeWords, StWW)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))


docs <- tm_map(docs, stripWhitespace)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))


library(SnowballC)


for (j in seq(docs)) {
  docs[[j]]<-stemDocument(docs[[j]], language = "english")
}
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))


dtm <- DocumentTermMatrix(docs)
dtm

inspect(dtm)

tdm <- t(dtm)

tdm <- TermDocumentMatrix(docs)
tdm 


inspect(tdm[140:145,1:2])


dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20),bounds = list(global = c(2,Inf))))
dtmr



#filenames <- list.files(getwd(),pattern="*.txt")
#filenames <-c(filenames)
#rownames(dtmr )<-filenames

dtmr1 = removeSparseTerms(dtmr, 0.70) 
dtmr1



doc_length <- as.data.frame(rowSums(as.matrix(dtm)))
doc_length
max_length<-max(doc_length)
max_length
min_length<-min(doc_length)
min_length
aver_length<-mean(rowSums(as.matrix(dtm)))
aver_length



nn<-rowSums(as.matrix(dtm))
nn
dtm_Norm<-dtm/nn


m0 <- as.matrix(dtm)
write.csv(m0, file="D:/ANALITYKA GOSPODARCZA/Magisterka/Semestr I/Data mining/Stopwords/DocumentTermMatrix.csv")
m1<-as.data.frame(as.matrix(dtm_Norm))
write.csv(m1, file="D:/ANALITYKA GOSPODARCZA/Magisterka/Semestr I/Data mining/Stopwords/DocumentTermMatrixNorm.csv")
m2 <- as.matrix(dtmr)
write.csv(m2, file="D:/ANALITYKA GOSPODARCZA/Magisterka/Semestr I/Data mining/Stopwords/DocumentTermMatrix_1.csv")
m3 <- as.matrix(dtmr1)
write.csv(m3, file="D:/ANALITYKA GOSPODARCZA/Magisterka/Semestr I/Data mining/Stopwords/SparseDocumentTermMatrix.csv")


freqr <- colSums(as.matrix(dtm))
length(freqr)
freq <- sort(freqr, decreasing=TRUE)
head(freq, 14)


findFreqTerms(dtmr,lowfreq=80)

findAssocs(dtmr,"decent",0.6)

freqr <- colSums(as.matrix(dtmr))
length(freqr)
freq <- sort(freqr, decreasing=TRUE)
mk<-min(head(freq, 30))
mk
wf=data.frame(word=names(freq),freq=freq)
library(ggplot2)


p <- ggplot(subset(wf, freq>1), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


p <- ggplot(subset(wf, freq>mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

library(wordcloud)
set.seed(42)
wordcloud(names(freq),freq, min.freq=70)

#File/Save as/ Jpeg


set.seed(142)
wordcloud(names(freq), freq, max.words=100)


wordcloud(names(freq), freq, min.freq=70,colors=brewer.pal(6, "Dark2"))


set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)



#####################################################################
docs_1 <- VCorpus(DirSource(wd))
docs_1
docs_1<- tm_map(docs_1,removePunctuation)
docs_1<- tm_map(docs_1, removeNumbers)
for (j in seq(docs_1)) {
  docs_1 [[j]] <- gsub("/", " ", docs_1[[j]])
  docs_1 [[j]] <- gsub("@", " ", docs_1[[j]])
  docs_1 [[j]] <- gsub("–", " ", docs_1[[j]])
  docs_1 [[j]] <- gsub("’", " ", docs_1[[j]])
  docs_1 [[j]] <- gsub("“", " ", docs_1[[j]])
  docs_1 [[j]] <- gsub("…", " ", docs_1[[j]])
  docs_1 [[j]] <- gsub("‘", " ", docs_1[[j]])
  docs_1 [[j]] <- gsub(")", " ", docs_1[[j]])
  docs_1 [[j]] <- gsub("”", " ", docs_1[[j]])
}
docs_1<- tm_map(docs_1, tolower)
docs_1<- tm_map(docs_1, removeWords, stopwords("English"))
StW<-read.table("D:/ANALITYKA GOSPODARCZA/Magisterka/Semestr I/Data mining/Stopwords/StopWords.txt")
StWW<-as.character(StW$V1)
StWW
docs_1<- tm_map(docs_1, removeWords, StWW)
docs_1<- tm_map(docs_1, stripWhitespace)
for (j in seq(docs_1)) {
  docs_1[[j]]<-stemDocument(docs_1[[j]], language = "english")
}
docs_1<- tm_map(docs_1, PlainTextDocument) #this line is required here!
docs_1


NgramTokenizer = function(x) {
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "),
         use.names = FALSE)
}
dtm_n <- DocumentTermMatrix(docs_1, control = list(tokenize = NgramTokenizer))
dtm_n



freq_n <- sort(colSums(as.matrix(dtm_n)), decreasing=TRUE)
head(freq_n, 15)
mk<-min(head(freq_n, 15))
tail(freq_n, 15)
m<-as.matrix(dtm_n)
write.csv(m, file="N_DocumentTermMatrix.csv")


wf=data.frame(word=names(freq_n),freq=freq_n)
wf
p <- ggplot(subset(wf, freq>=mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")+ ggtitle("Histogram of Bigrams for Opinions") +labs(x="Bi
-grams",y="Frequency")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1, size=16))
p















