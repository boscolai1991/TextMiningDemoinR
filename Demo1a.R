df0<-read.csv("sampleUserData.csv",stringsAsFactors=F)

userList<-unique(df0$userId)
itemList<-unique(df0$itemId)

n <- length(userList) #no ppl
m <- length(itemList) #no product
k <- 1 # latent factor

datamat <- matrix(NA, nr=m, nc=n)

for(i in 1:m){
  for(j in 1:n){
    acts<-df0$action[(df0$userId==userList[j])&(df0$itemId==itemList[i])]
    if(length(acts)!=0){
      if("click"%in%acts){datamat[i,j]<-1}
      if("favourite"%in%acts){datamat[i,j]<-2}
      if("purchased"%in%acts){datamat[i,j]<-3}
    }
  }
}

beta0 <- matrix(rnorm(n*k),nr=n,nc=k)
x0 <- matrix(rnorm(m*k),nr=m,nc=k)


learnr<-0.0005
shrink<-0.100

VAL<-NULL

for(i in 1:10000){
  diff<-(x0%*%t(beta0) - datamat)
  diff[is.na(diff)]<-0
  x1 <- x0 - learnr*(diff%*%beta0 + shrink*x0)
  beta1 <- beta0 - learnr*(t(diff)%*%x0 + shrink*beta0)
  x0<-x1
  beta0<-beta1
  #VAL<-c(VAL,sum(diff^2,na.rm = TRUE))
  print(sum(diff^2,na.rm = TRUE))
}

# prediction
predictmat <- x0%*%t(beta0)
resmat <- round(predictmat)
resmat[round(predictmat)<1]<-""
resmat[round(predictmat)==1]<-"click"
resmat[round(predictmat)==2]<-"favourite"
resmat[round(predictmat)>=3]<-"purchased"
print(resmat)
