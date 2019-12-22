### this file is for cluster 
### load data before running this


library(Matrix)
load(file = "train_big.Rda")
load(file = "train_y.Rda")
load(file = "test_big.Rda")
load(file = "test_y.Rda")
load(file = "disease_vector.Rda")
load(file = "include_sample_test.Rda")


dim(train_big)
dim(train_y)



Logit <- function( X , y,lambda){
  #X <- train_big
  #y <- train_y
  #X <- cbind(rep(1,dim(train_big)[1]),X)
  n <- dim(X)[1]
  p <- dim(X)[2]
  k <- dim(y)[2]
  m_nosadam <- 0
  v_nosadam <- 1
  p <- dim(X)[2]
  x <- matrix(1e-6, p, k-1)
  alpha_nosadam <- 5e-2
  beta_1 <- 0.9
  #times <- max_ite
  times <- 10000
  gamma <- 1e-2
  B <- cumsum((1:(1 + times))^(-gamma))
  loss <- rep(0, times)
  testloss <- rep(0, times)
  error <- rep(0,times)
  # lambda<-1e-1
  i <- 0
  flag = 0
  while (flag < times) {
    i <- i+1
    flag <- flag+1
    beta_2 <- B[i]/B[i+1] #0.99 #
    
    gradient <- Gradient(X,x,y) 
    tmp <- sqrt(rowSums(gradient^2)) > lambda
    
    gradient <- gradient - (lambda * x/ sqrt(1e-6 + rowSums(x^2)) ) * (flag > 10)
    
    m_nosadam <- m_nosadam * beta_1 + (1-beta_1) * gradient
    v_nosadam <- v_nosadam * beta_2 + (1-beta_2) *gradient^2
    
    # lasso
    # tmp <- abs(gradient) > lambda
    # x <- tmp * ( x + alpha_nosadam / sqrt(i) * m_nosadam / sqrt(v_nosadam)      )
    
    #group lasso
    
    x <- tmp * ( x + alpha_nosadam/ sqrt(i)  * m_nosadam / sqrt(v_nosadam)      ) #
    
    #x <-  ( x + alpha_nosadam / sqrt(i) * m_nosadam / sqrt(v_nosadam)      )
    
    n <- dim(test_big)[1]
    Beta <- cbind(rep(0, n),x)
    P0 <- exp(cbind(rep(1, n),test_big)%*%Beta)
    pred <- apply(P0, 1, which.max)
    
    if(i%%20 == 0){
      loss[i] <- LOSS(X, x, y)
      testloss[i] <- mean(disease_vector[include_sample_test] == pred)
    }
    
    
    
    
    
    
    # if(i>=10 ){
    #   error[i]<-var(loss[(i-9):i])
    #   if(!is.na(error[i]) && error[i]<1e-3){
    #     break
    #   }
    # }
  }
  
  #loss
  #plot(loss)
  # P0<-cbind(1-rowSums(P),P)
  # pred<- apply(P0, 1, which.max)
  # mean(disease_vector[include_sample0]==pred)
  # train_acc_nosadam<-mean(pred==y)
  result<-list()
  result$beta<-x
  result$loss<-loss[loss!=0]
  #result$Train_Acc<-train_acc_nosadam
  result$testloss<-testloss[testloss!=0]
  return(result)
}



# X<-train_big
# y<-train_y

Gradient<-function(X,x,y){
  P0<- exp(X%*%x) # n by k-1
  P<<-P0/(rowSums(P0)+1)
  return( t(X)%*% ( y[,-1]-P ) ) #p by k-1
}


LOSS<-function(X,x,y){
  P1<-log(P)
  P1<-cbind(log(1-rowSums(P)),P1)
  sum( -P1*y   )
}


n<-dim(train_big)[1]

result<-Logit(cbind(rep(1,n),train_big),train_y,lambda = exp(3))
result$loss
result$testloss
