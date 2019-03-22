library(MASS)
setwd("C:/Users/burak/Desktop/Burak/KU/Comp421/comp_indr421_521_elec443_543_fall2018_hw06")
set.seed(521)
X1 <- mvrnorm(50, c(+2.5, +2.5), matrix(c(.8, -0.6, -0.6, .8), nrow=2,ncol= 2))
X2 <- mvrnorm(50,c(-2.5, +2.5), matrix(c(.8, 0.6, 0.6, .8), nrow=2, ncol=2))
X3 <- mvrnorm(50, c(-2.5, -2.5), matrix(c(.8, -0.6, -0.6, .8), nrow=2, ncol=2))
X4 <- mvrnorm(50,c(+2.5, -2.5), matrix(c(.8, 0.6, 0.6, .8), nrow=2, ncol=2))
X5 <- mvrnorm(100, c(0, 0), matrix(c(1.6, 0, 0, 1.6),nrow =  2, ncol=2))

X <- rbind(X1, X2, X3, X4, X5)
centroids<-NULL
assignments<-NULL
  plot(X,xlab='x1',ylab = 'x2', xlim=c(-6,6), ylim=c(-6,6),frame.plot = TRUE , ann = TRUE, col = "black", pch = 19)
N <- 300
K <- 5
for(i in 1:2){
if (is.null(centroids) == TRUE) {
  centroids <- X[sample(1:N, K),]
}else {
  for (k in 1:K) {
    centroids[k,] <- colMeans(X[assignments == k,])
  }  
}
centroids <<- centroids




D <- as.matrix(dist(rbind(centroids, X), method = "euclidean"))
D <- D[1:nrow(centroids), (nrow(centroids) + 1):(nrow(centroids) + nrow(X))]
assignments <<- sapply(1:ncol(D), function(i) {which.min(D[,i])})
}
means <<- centroids 

prior1 <-sum(assignments==1)/300
prior2 <-sum(assignments==2)/300
prior3 <-sum(assignments==3)/300
prior4 <-sum(assignments==4)/300
prior5 <-sum(assignments==5)/300
priors<-c(prior1,prior2,prior3,prior4,prior5)
cov1=cov(X[assignments==1,])
cov2=cov(X[assignments==2,])
cov3=cov(X[assignments==3,])
cov4=cov(X[assignments==4,])
cov5=cov(X[assignments==5,])
covs=array(c(cov1,cov2,cov3,cov4,cov5))
gaus <- function(cov,mean,j){
  sapply(1:N, function (i) {
    (1/(2*pi * det(cov)^0.5))  * exp (-0.5 * t( X[i,] - mean[j,]) %*% ginv(cov) %*% ((X[i,]-mean[j,])))
  })
}


gaussian1<- gaus(cov1,means,1)
gaussian2<- gaus(cov2,means,2)
gaussian3<- gaus(cov3,means,3)
gaussian4<- gaus(cov4,means,4)
gaussian5<- gaus(cov5,means,5)
gausm<-matrix(c(gaussian1,gaussian2,gaussian3,gaussian4,gaussian5),ncol = 5)
H <-matrix(0,nrow = 300,ncol=5)
covs 
for(i in 1:100){
    for(k in 1:K){
      H[,k] <- gausm[,k]*priors[k] / rowSums(sapply(1:K,function(y){
        gausm[,y]*priors[y]}))
      means[k,]<-colSums(H[,k]*X)/sum(H[,k])
     
      temp_covs<-matrix(rowSums(sapply(1:N,function(y){
        H[y,k]*((X[y,]-means[k,])) %*% t((X[y,]-means[k,]))
      })),ncol=2)/sum(H[,k])
      if(i==100&&k==1){
        covf1= temp_covs
      }
      if(i==100&&k==2){
        covf2= temp_covs
      }
      if(i==100&&k==3){
        covf3= temp_covs
      }
      if(i==100&&k==4){
        covf4= temp_covs
      }
      if(i==100&&k==5){
        covf5= temp_covs
      }
      priors[k]<- sum(H[,k])/300
      gausm[,k]<- sapply(1:N, function (i) {
        (1/(2*pi * det(temp_covs)^0.5))  * exp (-0.5 * t( X[i,] - means[k,]) %*% ginv(temp_covs) %*% ((X[i,]-means[k,])))
      })
      
    }
}

assignments<-sapply(1:N,function(i){
  which.max(H[i,])
})
print(means)
colors <- c("#ff7f00","#e31a1c","#33a02c","#1f78b4", "#6a3d9a")
plot(X[,1], X[,2], col = colors[assignments], xlim = c(-6, 6), ylim = c(-6, 6), xlab = "x1", ylab = "x2", pch = 19)

x1_interval <- seq(from = -6, to = +6, by = 0.06)
x2_interval <- seq(from = -6, to = +6, by = 0.06)
x1_grid <- matrix(x1_interval, nrow = length(x1_interval), ncol = length(x1_interval), byrow = FALSE)
x2_grid <- matrix(x2_interval, nrow = length(x2_interval), ncol = length(x2_interval), byrow = TRUE)


colors <- c("#ff7f00","#e31a1c","#33a02c","#1f78b4", "#6a3d9a")
plot(X[,1], X[,2], col = colors[assignments], xlim = c(-6, 6), ylim = c(-6, 6), xlab = "x1", ylab = "x2", pch = 19)

x1_interval <- seq(from = -6, to = +6, by = 0.06)
x2_interval <- seq(from = -6, to = +6, by = 0.06)
x1_grid <- matrix(x1_interval, nrow = length(x1_interval), ncol = length(x1_interval), byrow = FALSE)
x2_grid <- matrix(x2_interval, nrow = length(x2_interval), ncol = length(x2_interval), byrow = TRUE)

f <- function(x1, x2) {
  (1/(2*pi * det(covf1)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - means[1,]), ncol=2) %*% ginv(covf1) %*% t(((matrix((c(x1,x2)-means[1,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)

f <- function(x1, x2) {
  (1/(2*pi * det(covf2)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - means[2,]), ncol=2) %*% ginv(covf2) %*% t(((matrix((c(x1,x2)-means[2,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)

f <- function(x1, x2) {
  (1/(2*pi * det(covf3)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - means[3,]), ncol=2) %*% ginv(covf3) %*% t(((matrix((c(x1,x2)-means[3,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)

f <- function(x1, x2) {
  (1/(2*pi * det(covf4)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - means[4,]), ncol=2) %*% ginv(covf4) %*% t(((matrix((c(x1,x2)-means[4,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)

f <- function(x1, x2) {
  (1/(2*pi * det(covf5)^0.5))  * exp (-0.5 * matrix((c(x1,x2) - means[5,]), ncol=2) %*% ginv(covf5) %*% t(((matrix((c(x1,x2)-means[5,]),ncol=2)))))}
discriminant_values <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))
contour(x1_interval, x2_interval, discriminant_values, levels = c(0.05), add = TRUE, lwd = 2, drawlabels = FALSE)
