df0<-read.csv("sampleProductData.csv")

#install.packages("stringr")
library(stringr)


titlevec<-tolower(df0[,1])
titlevec2<-sapply(str_extract_all(titlevec, "[A-Za-z0-9-']+"),function(x){paste(x,collapse=" ")})
titlevec2<-gsub(" - ","",titlevec2)

#
# Pending Task:
# 1) Keywording for each category
# 2) Discounting Characters from Brands and etcs
# 3) Typo Correction
