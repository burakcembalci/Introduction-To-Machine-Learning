setwd("C:/Users/burak/Desktop/Burak/KU/Comp421/comp_indr421_521_elec443_543_fall2018_hw02")
safelog <- function(x) {
  return (log(x + 1e-100))
}
#reading data
imgs<-read.csv("hw02_data_set_images.csv",header= FALSE)
labels<-read.csv("hw02_data_set_labels.csv", header=FALSE)

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
class(X)
#getting the labels for train set
y_truth<-c(labels[1:25,],labels[40:64,],labels[79:103,],labels[118:142,],labels[157:181,])
# define K and N
K<-max(y_truth)
N<-length(y_truth)
#one-of-K-encoding
Y_truth <- matrix(0, N, K)
Y_truth[cbind(1:N, y_truth)] <- 1
Y_truth<- as.matrix(Y_truth)
# define the sigmoid function
sigmoid <- function(X, W, w0) {
 # scores <- cbind(as.matrix(X), 1) %*% rbind(W, w0)
 # scores <- exp(scores - matrix(apply(scores, MARGIN = 2, FUN = max), nrow = nrow(scores), ncol = ncol(scores), byrow = FALSE))
 # scores <- scores / matrix(rowSums(scores), nrow(scores), ncol(scores), byrow = FALSE)
 scores<- (1 / (1 + exp(-(X %*% W + w0))))

  return (scores)
}

# define the gradient functions
gradient_W <- function(X, Y_truth, Y_predicted) {
 # return (-sapply(X = 1:ncol(Y_truth), function(c) colSums(matrix(Y_truth[,c] - Y_predicted[,c], nrow = nrow(X), ncol = ncol(X), byrow = FALSE) * X)))
    return (-sapply(X = 1:ncol(Y_truth), function(c) colSums(matrix((Y_truth[,c] - Y_predicted[,c])*Y_predicted[,c]*(1-Y_predicted[,c]), nrow = nrow(X), ncol = ncol(X), byrow = FALSE) * X)))
  }

gradient_w0 <- function(Y_truth, Y_predicted) {
 # return (-colSums(Y_truth - Y_predicted))
  return (-colSums((Y_truth - Y_predicted)*Y_predicted*(1-Y_predicted)))
}

# set learning parameters
eta <- 0.01
epsilon <- 1e-3

# randomly initalize W and w0
set.seed(521)
W <- matrix(runif(ncol(X) * K, min = -0.01, max = 0.01), ncol(X), K)
w0 <- runif(K, min = -0.01, max = 0.01)

# learn W and w0 using gradient descent
iteration <- 1
objective_values <- c()
while (1) {
  Y_predicted <- sigmoid(X, W, w0)
  
  objective_values <- c(objective_values, 0.5*sum((Y_truth-Y_predicted)^2))
  
  W_old <- W
  w0_old <- w0
  
  W <- W - eta * gradient_W(X, Y_truth, Y_predicted)
  w0 <- w0 - eta * gradient_w0(Y_truth, Y_predicted)
  
  if (sqrt(sum((w0 - w0_old)^2) + sum((W - W_old)^2)) < epsilon) {
    break
  }
  
  iteration <- iteration + 1
}
print(W)
print(w0)
# plot objective function during iterations
plot(1:iteration, objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

# calculate confusion matrix
y_predicted <- apply(Y_predicted, 1, which.max)
confusion_matrix <- table(y_predicted, y_truth)
print(confusion_matrix)

# define K and N
K<-max(y_truth_test)
N<-length(y_truth_test)
#one-of-K-encoding
Y_truth_test <- matrix(0, N, K)
Y_truth_test[cbind(1:N, y_truth_test)] <- 1
# learn W and w0 using gradient descent
iteration <- 1
objective_values <- c()
while (1) {
  Y_predicted_test <- sigmoid(X_test, W, w0)
  
  objective_values <- c(objective_values, 0.5*sum((Y_truth-Y_predicted)^2))
  
  W_old <- W
  w0_old <- w0
  
  W <- W - eta * gradient_W(X, Y_truth, Y_predicted)
  w0 <- w0 - eta * gradient_w0(Y_truth, Y_predicted)
  
  if (sqrt(sum((w0 - w0_old)^2) + sum((W - W_old)^2)) < epsilon) {
    break
  }
  
  iteration <- iteration + 1
}
# calculate confusion matrix
y_predicted_test <- apply(Y_predicted_test, 1, which.max)
confusion_matrix_test <- table(y_predicted_test, y_truth_test)
print(confusion_matrix_test)
