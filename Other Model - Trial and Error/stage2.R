


##glmnet
############
a<-c(0,testsize)
a<-cumsum(a)
chosen_feature<-(a[5]+1):a[6]
#class2_1<-as.vector( include_sample[1:7,1:testsize[1]] ) 
class2_1<-as.vector( include_sample[1:7,chosen_feature] ) 



plot(Train[class2_1,4])

 # b<-which(Train[class2_1,4] ==30)
 # include_sample0<-class2_1[-b]
include_sample0<-class2_1
train_big<- M_dgt1[  include_sample0 ,] 

cvfit=cv.glmnet(train_big,Train[include_sample0,4] ,family="multinomial",parallel = TRUE,type.measure = "class",type.multinomial = "grouped")

log(cvfit$lambda) 
plot(cvfit)
cvfit$cvm
cvfit$lambda.min#0.005604669
cvfit$lambda.1se#0.01235903
result<-predict(cvfit, newx = train_big, s = "lambda.1se", type = "class")
mean(result==Train[include_sample0,4])
table(result,Train[include_sample0,4])

include_sample1<-as.vector(include_sample[8,chosen_feature] )

test_big<- M_dgt1[  include_sample1 ,] 

result<-predict(cvfit, newx = test_big, s = "lambda.1se", type = "class")
mean(result==Train[include_sample1,4])
table(result,Train[include_sample1,4])

cvfit5<-cvfit

save(cvfit1,file="~/stage1_1.Rda")
save(cvfit2,file="~/stage1_2.Rda")
save(cvfit3,file="~/stage1_3.Rda")
save(cvfit4,file="~/stage1_4.Rda")
save(cvfit5,file="~/stage1_5.Rda")
save(cvfit6,file="~/stage1_6.Rda")
save(cvfit7,file="~/stage1_7.Rda")
save(cvfit8,file="~/stage1_8.Rda")

stage2model<-list()

#save(stage2model,file="~/stage2model.Rda")

load(file="~/stage1_7.Rda")

stage2model[[5]]<-cvfit5
load("~/stage1.Rda")
##########
#0.905 0.866  0.915 0.899 0.978  (only 1:4) 0.80  0.89



include_sample1<-(include_sample[8,] )

test_big<- M_dgt1[  include_sample1 ,] 
load(file="~/stage1.Rda")
result<-predict(cvfit, newx = test_big, s = "lambda.1se", type = "class")

tb1<-table(result, floor(Train[include_sample1,4]/10),dnn = c('Predicted','Actual'))
mean(result== floor(Train[include_sample1,4]/10))

nt<-length(result)
result<-as.numeric(result)
for (ii in 1:7) {
  tmp<-which(result==ii)
  result[tmp] <- predict(stage2model[[ii]], newx = test_big[tmp,], s = "lambda.1se", type = "class")
}

mean(result==Train[include_sample1,4])

tb <- table(result,Train[include_sample1,4])
