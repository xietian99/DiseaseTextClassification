library(keras)
library(tidyverse)
library(tensorflow)

# load data input
load(file="./Sequential NN/NNdata.rda")

# specify model features
max_features <- 35000
maxlen <- 6122                                                                 
batch_size <-15
embedding_dims <- 50
filters <- 32
kernel_size <- c(3)
hidden_dims <- 300
epochs <- 7

# build sequential model
model <- keras_model_sequential() %>% 

  layer_dense(hidden_dims) %>%
  layer_dropout(0.3) %>%
  layer_activation("relu") %>%
  layer_dense(units = 48, activation = 'softmax') %>% 
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

# prepare training data and testing data
x <- as.matrix(M_dgt) 
y <- disease_factor

train_x <- x[1:100000,]
train_y <- y[1:100000,]

test_x <- x[100001:153956,]
test_y <- y[100001:153956,]

# train model
history <- model %>%
  fit(
    train_x,
    train_y,
    batch_size = batch_size,
    epochs = epochs,
    # validation_split = 0.3
  )

# evaluate model performance
result <- model %>% evaluate(test_x, test_y)

# load training outcome
load(file="NNmodel.rda")
result # testing accuracy = 0.8323264
history # trianing accuracy = 0.8937
