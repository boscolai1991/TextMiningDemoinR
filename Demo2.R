df0<-read.csv("sampleProductData.csv",stringsAsFactors = F)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim2 <- function (x) {y<-strsplit(x," ")[[1]]; y<-y[y!=""]; paste(y,collapse=" ")}

#install.packages("stringr")
#install.packages("stringdist")
library(stringr)
library(stringdist)


titlevec<-tolower(df0[,1])
titlevec2<-sapply(str_extract_all(titlevec, "[A-Za-z0-9-']+"),function(x){paste(x,collapse=" ")})
titlevec2<-gsub(" - ","",titlevec2)
df0$titlePro<-titlevec2
rm(titlevec,titlevec2)

# 2) Discounting Characters from Brands and etcs

df0$titleMinusBrand<-mapply(function(titletxt,brand){trim(gsub(tolower(brand),"",titletxt))},df0$titlePro,df0$brand)

# Tokenization

titleToken<-strsplit(df0$titleMinusBrand," ")
titleToken<-lapply(titleToken,function(x){x[x!=""]})

# 3) Typo Correction
titleBigram<-lapply(titleToken,function(token){
  if(length(token)<=1){
    return(NULL)
  } else {
      return(paste(token[2:length(token)-1],token[2:length(token)],sep=""))
  }
})
titleBigramwSpace<-lapply(titleToken,function(token){
  if(length(token)<=1){
    return(NULL)
  } else {
    return(paste(token[2:length(token)-1],token[2:length(token)],sep=" "))
  }
})

print(cbind(unlist(titleBigramwSpace),unlist(titleBigram))[unlist(titleBigram)%in%unlist(titleToken),])
#stringdistMat<-outer(unlist(titleToken),unlist(titleToken),function(x,y){stringdist(x,y)})
#outer(unlist(titleToken),unlist(titleToken),function(x,y){paste(x,y,sep=",")})[(stringdistMat==1)&(upper.tri(stringdistMat))]

# Popular Brand
for(i in unique(df0$productGroup)){
  cat("Category: ",i," - \n",sep="")
  brand<-df0$brand[df0$productGroup==i]
  tab<-sort(table(brand[brand!=""]),decreasing=T)
  print(tab[tab>1])
  cat("\n\n")
}

# Keywording
for(i in unique(df0$productGroup)){
  cat("Category: ",i," - \n",sep="")
  itemno<-(1:nrow(df0))[df0$productGroup==i]
  txt<-NULL
  for(j in itemno){
    txttmp<-titleToken[[j]]
    txttmp<-unique(txttmp)
    txttmp<-txttmp[txttmp!=""]
    txt<-c(txt,txttmp)
  }
  tab<-sort(table(txt),decreasing=T)
  print(tab[tab>1])
  cat("\n\n")
}
