#lsm関数
lsm <- function(d) {
  if (ncol(d)!=2) stop("単回帰だけ！")
  n <- nrow(d)
  x_mean <- mean(d[,1])
  y_mean <- mean(d[,2])
  x_deviation <- d[,1] - matrix(x_mean,nrow=n_row,ncol=1)
  y_deviation <- d[,2] - matrix(y_mean,nrow=n_row,ncol=1)
  a <- colSums(x_deviation*y_deviation)/colSums(x_deviation^2)
  b <- y_mean - a * x_mean
  FUNC <- function(x) {a * x + b } 
  ESS <- sum((d[,2] - FUNC(d[,1]))^2)
  R2 <- 1 - (ESS/colSums(y_deviation^2))
  plot(d[,1],d[,2],xlab="x",ylab="y")
  plot(FUNC,xlim=c(min(d[,1]), max(d[,1])),add=TRUE,type="l")
  return(list(a=a, b=b, R2=R2))
  }


#デモ
demo <- matrix( c( 
  1,  21,
  5,  138,
  3,  75,
  6.5, 135,
  10,  257,
  18,  399,
  7,  120,
  2,  45
),  ncol=2, byrow=TRUE)  

lsm(demo)
