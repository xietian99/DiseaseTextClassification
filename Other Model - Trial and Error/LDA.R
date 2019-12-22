# p0<-1000
# a<-matrix(rnorm(p0^2,0,1),p0,p0)
# svd0<-svd(a)


LDAmodel<-list()
accuracy<-matrix(0,3,k)

ii<-1
include_sample0<-include_sample[ii,]
train_big<- M_dgt[  include_sample0 ,] 
train_y<-disease_factor[include_sample0,c(1:29,33)]
sample_number<-include_sample0
label<-rep(1:30,testsize[c(1:29,33)])



n0<-length(unique(label))

feature_number<-sample(1:p1,p1)
feature_number<-matrix(feature_number[1:2130],2,1065)

for(jj in 1:2){
  mu_l<-list()
  Sw<-0

  for (ii in 1:n0) {
    feature_number0<-feature_number[jj,]
  
    tmp<-which(label==ii)
    M<-M_dgt[ include_sample0[tmp],feature_number0]
    mu<-colMeans(M)  # mu length p
    M<-t(M)-mu  # M_{n by p} => M_{p by n}
    Sw<- Sw + M%*%t(M)
    mu_l[[ii]]<-mu
  
  }
  Sb<-0
  mu<-colMeans( M_dgt[include_sample0,feature_number0])
  for (ii in 1:n0) {
    Sb<-Sb+ sum(label==ii)*outer(mu_l[[ii]]-mu,mu_l[[ii]]-mu)
  }

  svd0<-svd(solve(Sw+diag(1,1065))%*%Sb)
}

###############
mu_l<-list()
Sw<-0

for (ii in 1:n0) {
  feature_number0<-feature_number[jj,]
  
  tmp<-which(label==ii)
  M<-M_dgt[ include_sample0[tmp],]
  mu<-colMeans(M)  # mu length p
  M<-t(M)-mu  # M_{n by p} => M_{p by n}
  Sw<- Sw + M%*%t(M)
  mu_l[[ii]]<-mu
  
}
Sb<-0
mu<-colMeans( M_dgt[include_sample0,])
for (ii in 1:n0) {
  Sb<-Sb+ sum(label==ii)*outer(mu_l[[ii]]-mu,mu_l[[ii]]-mu)
}

svd0<-svd(solve(Sw+diag(1,p1))%*%Sb)
#### only first 30 eignevectors

M_dimred<-as.matrix(M_dgt%*%svd0$v[,1:30])







library(glmnet)
for(ii in 1:k){
  include_sample0<-include_sample[ii,]
  train_big<- M_dimred[  include_sample0 ,] 
  train_y<-disease_factor[include_sample0,c(1:29,33)]

  cvfit=cv.glmnet(train_big, train_y, family="multinomial", type.measure = "class",
                  parallel = TRUE)
  log(cvfit$lambda) 
  plot(cvfit)
  cvfit$cvm
  cvfit$lambda.min#0.005604669
  cvfit$lambda.1se#0.01235903
  result<-predict(cvfit, newx = train_big, s = "lambda.1se", type = "class")
  accuracy[1,ii]<-mean(result==disease_vector[include_sample0])
  
  test_big<- M_dimred[  -include_sample0 ,] 
  result<-predict(cvfit, newx = test_big, s = "lambda.1se", type = "class")
  accuracy[2,ii]<-mean(result==disease_vector[-include_sample0])
  
  ### all model 
  result<-predict(cvfit, newx = M_dimred, s = "lambda.1se", type = "class")
  accuracy[3,ii]<-mean(result==disease_vector)
  
  coef(cvfit, s="lambda.1se")
 a<- predict(cvfit, newx = train_big, s = "lambda.1se",type="response")
  a[1:3,,1]
  LDAmodel[[ii]]<-cvfit
 
}








a<-M_dgt[which(Train$event==inj_name[1]),]

svd0<-svd(a[1:1000,])
svd0$d


library(dimRed)
library(lle)
library(loe)
library(RSpectra)

dat <- loadDataSet("3D S Curve", n = 500)
## directy use the S4 class:
lle <- LLE()
emb <- lle@fun(dat, lle@stdpars)
## using embed():
emb2 <- embed(dat, "LLE", knn = 45)
plot(emb, type = "2vars")
plot(emb2, type = "2vars")

## kpca
dat <- loadDataSet("3D S Curve")
## use the S4 class directly:
kpca <- kPCA()
emb <- kpca@fun(dat, kpca@stdpars)
## simpler, use embed():
emb2 <- embed(dat, "kPCA")
plot(emb, type = "2vars")
## End(Not run)

dat <- loadDataSet("3D S Curve")
leim <- LaplacianEigenmaps()
emb <- leim@fun(dat, leim@stdpars)
plot(emb@data@data)



Fscore<-rep(0,6120)

j<-6122

  A<-matrix(0,3,30)
  Sw<-0
  Sb<-0
  for(ii in 1:29){
    a<-which(Train$event==inj_name[ii])
    n0<-length(a)
    p0<-mean(M_dgt[a,j])
    A[1,ii]<-p0
    A[2,ii]<-n0
    A[3,ii]<-var(M_dgt[a,j])
  }
  
  a<-which(Train$event==inj_name[33])
  n0<-length(a)
  p0<-mean(M_dgt[a,j])
  A[1,30]<-p0
  A[2,30]<-n0
  A[3,30]<-var(M_dgt[a,j])
  
  Sw<-sum( A[2,]*A[3,]    )/(n-30)
  mu<-mean(M_dgt[,j])
  Sb<-sum( A[2,]*(A[1,]-mu)^2     )/(30-1)

Sb/Sw

  
A[2,]*A[1,]
A[2,]
Fscore[1:10]

p_i<-A[1,]/(A[1,]+A[2,])




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
  
  #sex 400
  #Age 177