
smallsample<-which(Train[,4]%in% c(40:49))
docs<-Train[smallsample,1]
class(docs)

library(tm)

doc_corpus<-VCorpus(VectorSource(docs))
doc_corpus<-tm_map(doc_corpus, tolower)
stopwords("english")[-(121:144)]
## remove stopwords
#doc_corpus<-tm_map(doc_corpus, removeWords, stopwords("english"))
your_own_words_list<-c("rt","lt","fb","dx","was","were",stopwords("english")[-(121:144)])
doc_corpus<-tm_map(doc_corpus, removeWords, your_own_words_list)


## extra space
doc_corpus<-tm_map(doc_corpus, stripWhitespace)
doc_corpus<-tm_map(doc_corpus, removePunctuation)
doc_corpus<-tm_map(doc_corpus, PlainTextDocument)
doc_corpus[[1]]$content

## stemlized
library(SnowballC)
doc_corpus<-tm_map(doc_corpus, stemDocument)
doc_corpus[[1]]$content


n<-length(smallsample)
wl<-list()
for(i in 1:n){#
  words<-strsplit(as.character(Train[i,1])," ")
  words<-tolower(words[[1]])
  words<-wordStem(as.vector(words))
  for(j in 1:length(words)){

      wrd<-words[j]
      wrd<-gsub("^[0-9]*[yomf]*$","",wrd,perl=TRUE,ignore.case = T)

      if(nchar(wrd)>1){
        wl[[wrd]]<-c(wl[[wrd]],i) # for every word create a list with location list
      }

    }


}

nms<-names(wl)
p<-length(nms)
freqs<-sapply(wl,length)
names(freqs)<-nms
sort(freqs,decreasing = T)[1:100]


library(wordcloud2)
wcdata<-list(word = nms,freq=freqs)
names(wcdata) <- c("word", "freq")
wcdata$freq <- (wcdata$freq)^(2/3)
wordcloud2(wcdata)
