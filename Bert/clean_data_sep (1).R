### data preprocessing for bert
### sometimes two words are scanned togheter like "fallon" rather than "fall on"
### thus we will try to correct it 

#docs<-Train[smallsample,1]
table(Train[,4])
### delete samples that rarely occur
aaa <- c(10,20,21,22,40,29,30,45,49,50,54,56,59,61,65,67,69,74,79)
aaa <- setdiff(unique(Train[,4]), aaa)
aaa
smallsample <- which(Train[,4] %in% aaa )
length(smallsample)
set.seed(1)
smallsample <- sample(smallsample, length(smallsample))
docs <- Train[smallsample, 1]
class(docs)

library(tm)

doc_corpus <- VCorpus(VectorSource(docs))
doc_corpus <- tm_map(doc_corpus, tolower)
stopwords("english")[-(121:144)]
## remove stopwords
## we need prepostion here, which is important to our objective.
your_own_words_list <- c("rt","lt","fb","dx","was","were", stopwords("english")[-(121:144)])
doc_corpus <- tm_map(doc_corpus, removeWords, your_own_words_list)


## extra space
doc_corpus <- tm_map(doc_corpus, stripWhitespace)
doc_corpus <- tm_map(doc_corpus, removePunctuation)
doc_corpus <- tm_map(doc_corpus, PlainTextDocument)
doc_corpus[[1]]$content

## stemlized
library(SnowballC)
doc_corpus <- tm_map(doc_corpus, stemDocument)
doc_corpus[[1]]$content


library(hunspell)
sink(file = "~/Desktop/bert/data_spell.txt")
max_len <- 0
n <- length(smallsample)

### output the file for google colab to read
for(i in 1:n){#
  words <- strsplit(as.character(doc_corpus[[i]]$content)," ")
  flag=0
  
  w <- rep(0,length(words[[1]]))
  for(j in 1:length(words[[1]])){
      wrd <- words[[1]][j]
      wrd <- gsub("^[0-9]*[yomf]*$", "", wrd, perl=TRUE, ignore.case = T)
      if(nchar(wrd)<=1){
        next
      }
      if(nchar(wrd)>=5 ){
        bad <- hunspell(wrd)
        if(length(bad[[1]])>0){
          w[j] <- hunspell_suggest(bad[[1]])[[1]][1]
        }else{
          w[j] <- wrd
        }
      }else{
        w[j] <- wrd
      }
  }
  w <- w[which(w!=0)]
  max_len <- max(max_len, length(w))
  cat(w)
  cat("\n")
}

write.csv(Train[smallsample,4], file = "~/Desktop/bert/label_spell.txt", row.names = FALSE)
#######################

