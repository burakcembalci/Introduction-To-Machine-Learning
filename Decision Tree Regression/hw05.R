setwd("C:/Users/burak/Desktop/Burak/KU/Comp421/comp_indr421_521_elec443_543_fall2018_hw05")
dataset<- read.csv("hw05_data_set.csv")
X<-dataset$x
y<-dataset$y
X_train<-X[0:100]
X_test<-X[101:133]
y_train<-y[0:100]
y_test<-y[101:133]
N_train <- length(y_train)
N_test <- length(y_test)
K<-max(y)
data_interval <- seq(from = 0, to = 60, by = 0.01)


# create necessary data structures
node_indices <- list()
is_terminal <- c()
need_split <- c()

node_splits <- c()
node_mean <- c()
p <- 10
# put all training instances into the root node
node_indices <- list(1:N_train)
is_terminal <- c(FALSE)
need_split <- c(TRUE)
# learning algorithm
while (1) {
  # find nodes that need splitting
  split_nodes <- which(need_split)
  # check whether we reach all terminal nodes
  if (length(split_nodes) == 0) {
    break
  }
  # find best split positions for all nodes
  for (split_node in split_nodes) {
    data_indices <- node_indices[[split_node]]
    need_split[split_node] <- FALSE
    node_mean[[split_node]] <- mean(y_train[data_indices])
    # check whether node is pure
    if (length(y_train[data_indices]) <= p) {
      is_terminal[split_node] <- TRUE
    } else {
      is_terminal[split_node] <- FALSE
      
      best_scores <- 0
      best_splits <- 0
       
        unique_values <- sort(unique(X_train[data_indices]))
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(X_train[data_indices] < split_positions[s])]
          right_indices <- data_indices[which(X_train[data_indices] >= split_positions[s])]
          split_scores[s] <- sum((mean(y_train[left_indices])-y_train[left_indices])^2)+
            sum((mean(y_train[right_indices])-y_train[right_indices])^2)
        
        best_scores <- min(split_scores)
        best_splits <- split_positions[which.min(split_scores)]
        }
        
      # decide where to split on which feature
      node_splits[split_node] <- best_splits
      
      # create left node using the selected split
      left_indices <- data_indices[which(X_train[data_indices] <= best_splits)]
      node_indices[[2 * split_node]] <- left_indices
      is_terminal[2 * split_node] <- FALSE
      need_split[2 * split_node] <- TRUE
      
      # create left node using the selected split
      right_indices <- data_indices[which(X_train[data_indices] > best_splits)]
      node_indices[[2 * split_node + 1]] <- right_indices
      is_terminal[2 * split_node + 1] <- FALSE
      need_split[2 * split_node + 1] <- TRUE
    }
  }
}

y_predicted <- rep(0, N_test)
for (i in 1:N_test) {
  index <- 1
  while (1) {
    if (is_terminal[index] == TRUE) {
      y_predicted[i] <- node_mean[[index]]
      break
    } else {
      if (X_test[i] <= node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
}

line <- rep(0, length(data_interval))
for (i in 1:length(data_interval)) {
  index <- 1
  while (1) {
    if (is_terminal[index] == TRUE) {
      line[i] <- node_mean[[index]]
      break
    } else {
      if (data_interval[i] <= node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
}

plot(X_train,y_train, type = "p", pch = 19, col = c("blue"),
     ylab = "y", xlab = "x", las = 1)
points(X_test,y_test, type = "p", pch = 19, col = c("red"))
lines(data_interval,line, type = "l", lwd = 2, col = "black")

RMSE<-sqrt(sum((y_predicted-y_test)^2)/33)
sprintf("RMSE is  %f when P is %f ",RMSE,p)

RMSE_list<-c()
for(j in 1:20){
  
  node_indices <- list()
  is_terminal <- c()
  need_split <- c()
  
  node_splits <- c()
  node_mean <- c()
  p <-j
  # put all training instances into the root node
  node_indices <- list(1:N_train)
  is_terminal <- c(FALSE)
  need_split <- c(TRUE)
  # learning algorithm
  while (1) {
    # find nodes that need splitting
    split_nodes <- which(need_split)
    # check whether we reach all terminal nodes
    if (length(split_nodes) == 0) {
      break
    }
    # find best split positions for all nodes
    for (split_node in split_nodes) {
      data_indices <- node_indices[[split_node]]
      need_split[split_node] <- FALSE
      node_mean[[split_node]] <- mean(y_train[data_indices])
      # check whether node is pure
      if (length(y_train[data_indices]) <= p) {
        is_terminal[split_node] <- TRUE
      } else {
        is_terminal[split_node] <- FALSE
        
        best_scores <- 0
        best_splits <- 0
        
        unique_values <- sort(unique(X_train[data_indices]))
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(X_train[data_indices] < split_positions[s])]
          right_indices <- data_indices[which(X_train[data_indices] >= split_positions[s])]
          split_scores[s] <- sum((mean(y_train[left_indices])-y_train[left_indices])^2)+
            sum((mean(y_train[right_indices])-y_train[right_indices])^2)
          
          best_scores <- min(split_scores)
          best_splits <- split_positions[which.min(split_scores)]
        }
        
        # decide where to split on which feature
        node_splits[split_node] <- best_splits
        
        # create left node using the selected split
        left_indices <- data_indices[which(X_train[data_indices] <= best_splits)]
        node_indices[[2 * split_node]] <- left_indices
        is_terminal[2 * split_node] <- FALSE
        need_split[2 * split_node] <- TRUE
        
        # create left node using the selected split
        right_indices <- data_indices[which(X_train[data_indices] > best_splits)]
        node_indices[[2 * split_node + 1]] <- right_indices
        is_terminal[2 * split_node + 1] <- FALSE
        need_split[2 * split_node + 1] <- TRUE
      }
    }
  }
  
  y_predicted <- rep(0, N_test)
  for (i in 1:N_test) {
    index <- 1
    while (1) {
      if (is_terminal[index] == TRUE|| is.na(node_splits[index])) {
        y_predicted[i] <- node_mean[[index]]
        break
      } else {
        if (X_test[i] <= node_splits[index]) {
          index <- index * 2
        } else {
          index <- index * 2 + 1
        }
      }
    }
  }
  
  
  
  
    
RMSE<-sqrt(sum((y_predicted-y_test)^2/33))
RMSE
  RMSE_list<-c(RMSE_list,RMSE)
}
RMSE_list
plot(RMSE_list)
plot(RMSE_list, type="o", col="blue",xlab = 'P',ylab= 'RMSE' )


