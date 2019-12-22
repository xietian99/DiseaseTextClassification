### data preprocessing for bert

table(Train[,4])
aaa <- c(10,20,21,22,40,29,30,45,49,50,54,56,59,61,65,67,69,74,79)
aaa <- setdiff(unique(Train[,4]),aaa)
aaa
smallsample <- which(Train[,4] %in% aaa )
length(smallsample)

smallsample <- sample(smallsample, length(smallsample))
docs <- Train[smallsample,1]
class(docs)

library(tm)

doc_corpus <- VCorpus(VectorSource(docs))
doc_corpus <- tm_map(doc_corpus, tolower)
stopwords("english")
## remove stopwords
## we need prepostion here, which is important to our objective.
your_own_words_list <- c("rt","lt","fb","dx","was","were",stopwords("english")[-(121:144)])
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
check_error <- rep(0,n)
for(i in 1:n){#
  words <- strsplit(as.character(doc_corpus[[i]]$content)," ")
  flag=0
  
  w <- rep(0,length(words[[1]]))
  for(j in 1:length(words[[1]])){
    wrd <- words[[1]][j]
    wrd <- gsub("^[0-9]*[yomf]*$", "", wrd, perl = TRUE, ignore.case = T)
    if(nchar(wrd) > 1){
      w[j] <- wrd
    }
    
  }
  w <- w[which(w!=0)]
  words <- paste(w, collapse = " ")
  bad <- hunspell(words)
  check_error[i] <- length(bad[[1]])
}

sink()


write.csv(Train[smallsample,4], file = "~/Desktop/bert/label_spell.txt", row.names = FALSE)
