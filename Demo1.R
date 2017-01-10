n <- 400 #no ppl
m <- 400 #no product
lambda <- 8 # average product view per ppl
k <- 3 # latent factor

set.seed(2017)

beta <- matrix(rnorm(n*k),nr=n,nc=k)
x <- matrix(rnorm(m*k),nr=m,nc=k)
logitmat<-x%*%t(beta)

pmat <- exp(logitmat)/(1+exp(logitmat))
purchasemat <- (matrix(runif(m*n),nr=m,nc=n)<pmat) * 1
#datamat <- apply(purchasemat,2,function(x){x[-sample(1:m,size=rpois(1,lambda))]<-NA;return(x)})
datamat <- purchasemat


beta0 <- matrix(rnorm(n*k),nr=n,nc=k)
x0 <- matrix(rnorm(m*k),nr=m,nc=k)

learnr<-0.0005
shrink<-0.100

VAL<-NULL

for(i in 1:100000){
  diff<-(x0%*%t(beta0) - datamat)
  diff[is.na(diff)]<-0
  x1 <- x0 - learnr*(diff%*%beta0 + shrink*x0)
  beta1 <- beta0 - learnr*(t(diff)%*%x0 + shrink*beta0)
  x0<-x1
  beta0<-beta1
  #VAL<-c(VAL,sum(diff^2,na.rm = TRUE))
  print(sum(diff^2,na.rm = TRUE))
}


for(i in 1:100000){
  diff<--(2*datamat-1)*(x0%*%t(beta0))
  diff<-exp(diff)/(1+exp(diff))
  diff<--(2*datamat-1)*diff
  diff[is.na(datamat)]<-0
  x1 <- x0 - learnr*(diff%*%beta0 + shrink*x0)
  beta1 <- beta0 - learnr*(t(diff)%*%x0 + shrink*beta0)
  x0<-x1
  beta0<-beta1
  #VAL<-c(VAL,sum(diff^2,na.rm = TRUE))
  logitmat0<-x0%*%t(beta0)
  pmat0 <- exp(logitmat0)/(1+exp(logitmat0))
  ll<-sum(log(pmat0[(!is.na(datamat))&datamat==1]))+sum(log((1-pmat0)[(!is.na(datamat))&datamat==0]))
  print(ll)
}
