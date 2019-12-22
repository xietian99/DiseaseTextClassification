setwd("~/Desktop/pilotproject")
# install.packages("readr")
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("ROCR")
# install.packages("glmnet")

library(readr)
library(tm)
library(SnowballC)
library(ROCR)
Train <- read.csv("CDC_Text_ClassificationChallenge_TrainData.csv")
head(Train)

n<-dim(Train)[1]

summary(Train$age)
Train$age[which(Train$age==0)]<-as.integer(mean(Train$age))
Train$age[which(Train$age==5)]<-as.integer(mean(Train$age))


# wl<-list()
# for(i in 1:n){#
#   words<-strsplit(as.character(Train[i,1])," ")
#   words<-tolower(words[[1]])
#   words<-wordStem(as.vector(words))
#   for(j in 1:length(words)){
#     wrd<-words[j]
#     wrd<-gsub("^[0-9]*[yomf]*$","",wrd,perl=TRUE,ignore.case = T)
#     if(nchar(wrd)>1){
#       wl[[wrd]]<-c(wl[[wrd]],i) # for every word create a list with location list
#     }
#   }
# }
# 
# save(wl,file="~/wordlist_stem.Rda")
#load("/Users/wangchang/Desktop/pilotproject/wordlist.Rda")
load("/Users/wangchang/Desktop/pilotproject/wordlist_stem.Rda")

nms<-names(wl)
p<-length(nms)
sn<-sort(nms)
freqs<-sapply(wl,length) 
#for every element use length() ,and result is c([[i]] length)
wlOrder<-wl[order(freqs)]
index<-(length(wlOrder)-50):length(wlOrder)
#index<-(1:100)
top10<-wlOrder[index]

#no meaning

#maybe meaning

### choose words whose freqs>=5
wl_1<-wl[which(freqs>=5)]
p1<-length(names(wl_1))
for (ii in 1:p1) {
  wl_1[[ii]]<-unique(wl_1[[ii]])
}
freqs_1<-sapply(wl_1,length) 
word_sum<-sum(freqs_1)
nms1<-names(wl_1)


#### generate 0 & 1 output (n by 48 matrix)
inj_name<-unique(Train[,4])
disease_factor<-vector()
disease_vector<-rep(0,n)
for (i in 1:length(inj_name)) {
  a<-rep(0,n)
  a[which(Train[,4]==inj_name[i])]<-1
  disease_vector<-disease_vector+a*i
  disease_factor<-cbind(disease_factor,as.vector(a))
}

sample_vector<-apply(disease_factor,2,sum)

#####  Generate sparse matrix
##########################################
i<-rep(1:(word_sum))
j<-rep(1:word_sum)
tmp<-0
for (ii in 1:p1) {
  a<-wl_1[[ii]]
  n0<-length(a)
  i[tmp+1:n0]<-a
  j[tmp+1:n0]<-rep(ii,n0)
  tmp<-tmp+n0
}
x =rep(1,length(i))

i<-c(i,1:n,1:n)
j<-c(j,rep(p1+1,n),rep(p1+2,n))
x <-c(x,Train$sex,Train$age)
library(Matrix)
#create a sparse matrix
dims = c(n,p1+2)


M_dgc = sparseMatrix(i=i,j=j,x=x,dims=dims) 
M_dgt = as(M_dgc,"TsparseMatrix")

M_pn = sparseMatrix(i=j,j=i,x=x,dims=rev(dims)) 
M_pn = as(M_pn,"TsparseMatrix")
###################################################


### Do anova test to move out insignificant feature
########
A<-matrix(0,30,p1+2)
Sw<-0
Sb<-0
for(ii in 1:29){
  a<-which(Train$event==inj_name[ii])
  n0<-length(a)
  p0<-colMeans(M_dgt[a,])
  A[ii,]<-p0  # 1 by p
}

a<-which(Train$event==inj_name[33])
n0<-length(a)
p0<-colMeans(M_dgt[a,])
A[30,]<-p0  # 1 by p

Sw<-colSums( sample_vector[c(1:29,33)]*A*(1-A)     )/(n-30)
mu<- colMeans(M_dgt)
Sb<- colSums ( sample_vector[c(1:29,33)]*(A- matrix(rep(mu,each=30),30,p1+2) )^2     )   /(30-1)

Fscore<-Sb/Sw
length(which(Fscore<=5))
M_dgt<-M_dgt[,which(Fscore>5)]
p1<-dim(M_dgt)[2]


#"fell","lac","cut","finger","hand","knife","lacer","head"

#sex 400
#Age 177
#######



# k fold
###################
set.seed(1)
k<-8
testsize<-floor(sample_vector/k)
include_sample<-vector()

for( ii in 1:29){
  a<-sample(which(Train$event==inj_name[ii]),testsize[ii]*k)
  dim(a)<-c(k,testsize[ii])
  include_sample<-cbind(include_sample,a)
}

ii<-33
a<-sample(which(Train$event==inj_name[ii]),testsize[ii]*k)
dim(a)<-c(k,testsize[ii])
include_sample<-cbind(include_sample,a)




#################

### LDA for fold 1
##################
label<-rep(1:30,testsize[c(1:29,33)])
mu_l<-list()
Sw<-0
include_sample0<-include_sample[1,]
for (ii in 1:30) {
  tmp<-which(label==ii)
  M<-M_dgt[ include_sample0[tmp],]
  mu<-colMeans(M)  # mu length p
  M<-t(M)-mu  # M_{n by p} => M_{p by n}
  Sw<- Sw + M%*%t(M)
  mu_l[[ii]]<-mu
  
}
Sb<-0
mu<-colMeans( M_dgt[include_sample0,])
for (ii in 1:30) {
  Sb<-Sb+ sum(label==ii)*outer(mu_l[[ii]]-mu,mu_l[[ii]]-mu)
}

svd0<-svd(solve(Sw+diag(1,p1))%*%Sb)
#### only first 29 eignevectors

M_dimred<-as.matrix(M_dgt%*%svd0$v[,1:29])

# M_1<-M_dimred[1:(testsize[1]+testsize[2]),]
# y_1<-rep(0:1,testsize[1:2])

M_1<-M_dimred[include_sample0,]
y_1<-disease_vector[include_sample0]

M_2<-M_dgt[1:(testsize[1]+testsize[2]),]
y_2<-rep(1:2,testsize[1:2])
#save(M_dimred,file="M_dimreduction.Rda")
load("M_dimreduction.Rda")
##################


fivemodel<-list()
accuracy<-matrix(0,3,k)

library(glmnet)
for(ii in 1:k){
  ### 19177 by 2131 == 0.788 (test)
  ### 
  include_sample0<-include_sample[ii,]
  #train_big<- M_dgt[  include_sample0 ,] 
  train_big<- M_dimred[  include_sample0 ,] 
  train_y<-disease_factor[include_sample0,c(1:29,33)]
  grid= exp(seq(-8,-4,length=10))
  #cvfit=cv.glmnet(train_big, train_y, family="multinomial",lambda = grid, parallel = TRUE, type.multinomial = "grouped")
  cvfit=cv.glmnet(train_big,disease_vector[include_sample0] , lambda = grid,
                  family="multinomial",parallel = TRUE,type.measure = "class",type.multinomial = "grouped")
  
  log(cvfit$lambda) 
  plot(cvfit)
  cvfit$cvm
  cvfit$lambda.min#0.005604669
  cvfit$lambda.1se#0.01235903
  result<-predict(cvfit, newx = train_big, s = "lambda.1se", type = "class")
  accuracy[1,ii]<-mean(result==disease_vector[include_sample0])
  
  test_big<- M_dgt[  -include_sample0 ,] 
  result<-predict(cvfit, newx = test_big, s = "lambda.1se", type = "class")
  accuracy[2,ii]<-mean(result==disease_vector[-include_sample0])
  
  ### all model 
  result<-predict(cvfit, newx = M_dgt, s = "lambda.1se", type = "class")
  accuracy[3,ii]<-mean(result==disease_vector)
  
  fivemodel[[ii]]<-cvfit
  #save(cvfit,file="~/6smallmodel1.Rda")
  #load(file="~/smallmodel1.Rda")
  #save(include_sample,file="~/include_sample_8.Rda")
}

#save(fivemodel,file="~/fivemodel_2.Rda")
#load(file="~/include_sample_8.Rda")
test_sample<-include_sample[k,]
result<-vector()
for(i in 1:(k-1)){
  cvfit<-fivemodel[[i]]
  test_eight<- M_dgt[  test_sample ,1:6120] 
  result<-cbind(result,predict(cvfit, newx = test_eight, s = "lambda.1se", type = "class"))
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

prediction0<-apply(result, 1, getmode)

mean(prediction0==disease_vector[test_sample])

floor_vector<-function(x){
  floor(x/10)
}
mean(prediction0==disease_vector[test_sample])

pred<-inj_name[prediction0]
truevalue<-inj_name[disease_vector[test_sample]]
mean(floor_vector(pred)==floor_vector(truevalue) )





confuse<-matrix(0,48,48)
truevalue<-disease_vector[test_sample]
for(i in 1:48){
  a<-which(truevalue==i)
  l<-length(a)
  for(j in 1:48){
    confuse[i,j]<-mean(prediction0[a]==j)
  }
}


# 1: 2,4,5,10,16,19,27
# 2: 1, 10
# 3: 1,7,10
# 4: 1, 19
# 5: 14
# 7: 5,14
# 9: 1,24
# 10: 1,2
# 11: 1,9
# 12: 1,4,5,7,14  accuracy = 0

# 14: 5,7
# 16: 1,4,19
# 19: 1,4
# 23: 1,2,5,7,10,23
# 24: nearly 0
# 27: 1
k<-27
plot(confuse[k,-k])
which(confuse[k,]>0.02)
confuse[k,]
#### multinomial



#fit = glmnet(x, y, family = "multinomial",lambda=grid,type.multinomial = "grouped")# group lasso rather than pure lasso


test_value<-disease_vector[test_sample]

vec_pred<-vector()
for (ii in 1:29) {
  pred <- prediction( as.integer(prediction0==ii), as.integer(test_value==ii)  )
  perf <- performance(pred,"f")
  vec_pred<-c(vec_pred,perf@y.values[[1]][2])
}
pred <- prediction( as.integer(prediction0==30), as.integer(test_value==33)  )
perf <- performance(pred,"f")
vec_pred<-c(vec_pred,perf@y.values[[1]][2])

vec_pred<-c(vec_pred,rep(0,18))
sum(vec_pred*sample_vector[1:48])/sum(sample_vector[1:48])
#cbind(apply(disease_factor,2,sum),vec_pred,trainsize)[1:39,]

result[cumsum(testsize)[1]:cumsum(testsize)[2]]
result[cumsum(testsize)[10]:cumsum(testsize)[11]]
result[cumsum(testsize)[11]:cumsum(testsize)[12]]

## 12 is usually ragearded as 4 & 14 !!!!!

