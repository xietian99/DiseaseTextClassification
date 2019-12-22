library(randomForest)
set.seed(666)

include_sample0<-include_sample[k,]
M_test<-M_dimred[include_sample0,]
y_test<-disease_vector[include_sample0]
P<-vector()

for(ii in 1: (k-1)){
  include_sample0<-include_sample[ii,]
  M_1<-M_dimred[include_sample0,]
  y_1<-disease_vector[include_sample0]
  
  ez.forest <- randomForest(as.matrix(M_1),as.factor(y_1),
                            na.action = na.roughfix,    #变量缺失值替换成对应列的中位数
                            importance = TRUE,type="classification",ntree = 1000)  #生成森林
  #ez.forest
  
  #importance(ez.forest,type=2)   #变量重要性
  forest.pred <- predict(ez.forest,M_test)
  P<-rbind(P,forest.pred )
  # forest.perf <- table(y_1,forest.pred,
  #                      dnn = c('Actual','Predicted'))
  # forest.perf
  
  #forest.pred <- predict(ez.forest,M_dimred)
  # mean(forest.pred==y_test)
  # mean(forest.pred==disease_vector)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean(apply(P, 2, getmode)==y_test)
mean(P[1,]==y_test)
mean(P[2,]==y_test)
mean(P[3,]==y_test)
mean(P[4,]==y_test)
mean(P[5,]==y_test)





library(e1071)   

#ss<-c(1:8000,25000:33000)

x<-M_dimred[include_sample0,]
y<-disease_vector[include_sample0]
svm1 <- svm(x,as.factor(y), 
            method="C-classification", kernal="radial", 
            gamma=0.1, cost=10)
prediction <- predict(svm1, M_dimred)
mean(prediction==disease_vector)

prediction <- predict(svm1, x)
mean(prediction==y)