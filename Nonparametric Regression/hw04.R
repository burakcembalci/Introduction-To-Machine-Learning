setwd("C:/Users/burak/Desktop/Burak/KU/Comp421/comp_indr421_521_elec443_543_fall2018_hw04")
X=read.csv("hw04_data_set.csv")
x=X$x
y=X$y
x_train= X$x[1:100]
y_train= X$y[1:100]
x_test=X$x[101:133]
y_test=X$y[101:133]
N <- length(x_train)
minimum_value <- 0
maximum_value <- 60
data_interval <- seq(from = 0, to = 60, by = 0.01)
#Q3
bin_width<-3
left_borders <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)
g_reg<- sapply(1:length(left_borders), function(b) {sum((left_borders[b] < x_train & x_train <= right_borders[b])*y_train) / (sum(left_borders[b] < x_train & x_train <= right_borders[b]))})
plot(x_train,y_train, type = "p", pch = 19, col = c("blue"),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test,y_test, type = "p", pch = 19, col = c("red"))
for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(g_reg[b], g_reg[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(g_reg[b], g_reg[b + 1]), lwd = 2, col = "black") 
  }
}
#Q4
g_reg<- sapply(floor(x_test/3)+1, function(b) {sum((left_borders[b] < x_train & x_train <= right_borders[b])*y_train) / (sum(left_borders[b] < x_train & x_train <= right_borders[b]))})
RMSE_reg<-sqrt(sum((g_reg-y_test)^2)/33)
RMSE_reg
#Q5
bin_width <-3
g_mean <- sapply(data_interval, function(x) {sum(((x - 0.5 * bin_width) < x_train & x_train <= (x + 0.5 * bin_width))*y_train) /sum((x - 0.5 * bin_width) < x_train & x_train <= (x + 0.5 * bin_width))})
plot(x_train,y_train, type = "p", pch = 19, col = "blue",
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test,y_test, type = "p", pch = 19, col = c("red"))
lines(data_interval, g_mean, type = "l", lwd = 2, col = "black")
#Q6
g_mean <- sapply(x_test, function(x) {sum(((x - 0.5 * bin_width) < x_train & x_train <= (x + 0.5 * bin_width))*y_train) /sum((x - 0.5 * bin_width) < x_train & x_train <= (x + 0.5 * bin_width))})
RMSE_mean<-sqrt(sum((g_mean-y_test)^2)/33)
RMSE_mean
#Q7
bin_width <-1
g_kern <- sapply(data_interval, function(x) {sum((1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))*y_train) /(sum(1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))) })
plot(x_train, y_train, type = "p", pch = 19, col ="blue",
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test,y_test, type = "p", pch = 19, col = c("red"))
lines(data_interval, g_kern, type = "l", lwd = 2, col = "black")
#Q8
g_kern <- sapply(x_test, function(x) {sum((1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))*y_train) /(sum(1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))) })
RMSE_kern<-sqrt(sum((g_kern-y_test)^2)/33)
RMSE_kern

