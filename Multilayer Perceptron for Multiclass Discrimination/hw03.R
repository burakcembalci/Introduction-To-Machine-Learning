  setwd("C:/Users/burak/Desktop/Burak/KU/Comp421/comp_indr421_521_elec443_543_fall2018_hw03")
  safelog <- function(x) {
    return (log(x + 1e-100))
  }
  #reading data
  imgs<-read.csv("hw03_data_set_images.csv",header= FALSE)
  labels<-read.csv("hw03_data_set_labels.csv", header=FALSE)
  
  #splitting the dataset as 25 train 14 test for A images
  Aimgs_train<-imgs[1:25,]
  Aimgs_test<-imgs[26:39,]
  #splitting the dataset as 25 train 14 test for B images
  Bimgs_train<-imgs[40:64,]
  Bimgs_test<-imgs[65:78,]
  #splitting the dataset as 25 train 14 test for C images
  Cimgs_train<-imgs[79:103,]
  Cimgs_test<-imgs[104:117,]
  #splitting the dataset as 25 train 14 test for D images
  Dimgs_train<-imgs[118:142,]
  Dimgs_test<-imgs[143:156,]
  #splitting the dataset as 25 train 14 test for E images
  Eimgs_train<-imgs[157:181,]
  Eimgs_test<-imgs[182:195,]
  #row binding the test sets to form a one test set
  X_test<-rbind.data.frame(Aimgs_test,Bimgs_test,Cimgs_test,Dimgs_test,Eimgs_test)
  X_test<-as.matrix(X_test)
  y_truth_test<-c(labels[26:39,],labels[65:78,],labels[104:117,],labels[143:156,],labels[182:195,])
  #row binding the train sets to form a one train set
  X<-rbind(Aimgs_train,Bimgs_train,Cimgs_train,Dimgs_train,Eimgs_train)
  X<-as.matrix(X)
  y_truth<-c(labels[1:25,],labels[40:64,],labels[79:103,],labels[118:142,],labels[157:181,])
  K <- max(y_truth)
  N <- length(y_truth)
  Y_truth <- matrix(0, N, K)
  Y_truth[cbind(1:N, y_truth)] <- 1
  
  K_test <- max(y_truth_test)
  N_test <- length(y_truth_test)
  Y_truth_test <- matrix(0, N_test, K_test)
  Y_truth_test[cbind(1:N_test, y_truth_test)] <- 1
  
  N <- length(y_truth)
  D <- ncol(X)
  
  sigmoid <- function(a) {
    return (1 / (1 + exp(-a)))
  }
  softmax <- function(X, v) {
    scores <- cbind(1,X) %*% v
    scores <- exp(scores - matrix(apply(scores, MARGIN = 2, FUN = max), nrow = nrow(scores), ncol = ncol(scores), byrow = FALSE))
    scores <- scores / matrix(rowSums(scores), nrow(scores), ncol(scores), byrow = FALSE)
    return (scores)
  }
  #params
  eta <- 0.005
  epsilon <- 1e-3
  H <- 20
  max_iteration <- 200
  set.seed(521)
  
  
  # randomly initalize W and v

  W <- matrix(runif((D + 1) * H, min = -0.01, max = 0.01), D + 1, H)
  v <- matrix(runif(5*H+5 , min = -0.01, max = 0.01),H+1,5)
  
    Z <- sigmoid(cbind(1,X) %*% W)
  y_predicted <- softmax(Z , v)
  objective_values <- -sum(Y_truth*safelog(y_predicted))
  
  iteration <- 1
  while (1) {
    v <- v + eta *t(t((Y_truth- y_predicted))%*%(cbind(1,Z)))
    W<-W+eta* t((t((Y_truth - y_predicted) %*% t(v[2:21,]) * (Z * (1 - Z))) %*% cbind(1, X)))
   # calculate hidden nodes
    Z <- sigmoid(cbind(1, X) %*% W)
    # calculate output node
    y_predicted <- softmax(Z , v)
    objective_values <- c(objective_values,-sum(Y_truth*safelog(y_predicted)))
    if (abs(objective_values[iteration + 1] - objective_values[iteration]) < epsilon | iteration >= max_iteration) {
      break
    }
    
    iteration <- iteration + 1
  }
  # plot objective function during iterations
  plot(1:(iteration + 1), objective_values,
       type = "l", lwd = 2, las = 1,
       xlab = "Iteration", ylab = "Error")
  
  # calculate confusion matrix
  Y_predicted <- apply(y_predicted, 1 , which.max)
  confusion_matrix <- table(Y_predicted, y_truth)
  print(confusion_matrix)
  
  Z_test<-sigmoid(cbind(1, X_test) %*% W)
  y_test_predicted <- softmax(Z_test , v)
  Y_predicted_test <- apply(y_test_predicted, 1, which.max)
  confusion_matrix_test <- table(Y_predicted_test, y_truth_test)
  print(confusion_matrix_test)