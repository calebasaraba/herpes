#set working directory
setwd("E:/Herpes")

#importing herpes identity matrix
platypus<-read.csv("IDRC Herpes Identity Matrix.csv")

#cleaning data up 
rownames(platypus)<-platypus[,1]
platypus<-platypus[,-1]

identical(rownames(platypus), colnames(platypus))

#cleaning up the row and column names to suit initial analysis
dinosaur<-rownames(platypus)
dinosaur<-substr(x = dinosaur,start = 7, stop = 33)
dinosaur<-ifelse(substring(dinosaur, 1, 1) == "_", substring(dinosaur,2),dinosaur)
dinosaur<-ifelse(substring(dinosaur, 10, nchar(dinosaur)) == "Alignment consensu" 
             | substring(dinosaur, 10, nchar(dinosaur)) == "Alignment consens",
             substring(dinosaur,1,8), dinosaur)
dinosaur<-ifelse(nchar(dinosaur) == 8, paste(dinosaur,".1", sep = ""), dinosaur)
dinosaur<-ifelse(substring(dinosaur,19,19) == "1", paste(substring(dinosaur,1,8), ".1", sep = ""), dinosaur)
dinosaur<-ifelse(substring(dinosaur,19,19) == "2", paste(substring(dinosaur,1,8), ".2", sep = ""), dinosaur)
dinosaur<-ifelse(substring(dinosaur,19,19) == "3", paste(substring(dinosaur,1,8), ".3", sep = ""), dinosaur)
dinosaur<-ifelse(substring(dinosaur,19,19) == "4", paste(substring(dinosaur,1,8), ".4", sep = ""), dinosaur)
dinosaur<-ifelse(substring(dinosaur,19,19) == "5", paste(substring(dinosaur,1,8), ".5", sep = ""), dinosaur)
dinosaur<-ifelse(substring(dinosaur, 10,13) == "Only", paste(substring(dinosaur,1,8),".1", sep = ""), dinosaur)
dinosaur<-ifelse(substring(dinosaur, 10,11) == "Al", paste(substring(dinosaur,1,8),".1", sep = ""), dinosaur)
dinosaur

#assigning cleaned names to matrix
rownames(platypus)<-dinosaur
colnames(platypus)<-dinosaur

#assigning a final name
idMat<-platypus


#changing from a matrix to a list of all comparisons
Prime<-rep(NA, (nrow(idMat)-1)*ncol(idMat)/2)
Comparison<-rep(NA, (nrow(idMat)-1)*ncol(idMat)/2)
Similarity<-rep(NA, (nrow(idMat)-1)*ncol(idMat)/2)
Sample1<-rep(NA, (nrow(idMat)-1)*ncol(idMat)/2)
Sample2<-rep(NA, (nrow(idMat)-1)*ncol(idMat)/2)

x <- 1
for (i in 1:(nrow(idMat)-1)){
  for (j in (i+1):ncol(idMat)){
    Prime[x] <- rownames(idMat)[i]
    Comparison[x] <- colnames(idMat)[j]
    Similarity[x] <- idMat[i,j]
    x <- x+1
  }
}
Sample1<-(substring(Prime,1,8))
Sample2<-(substring(Comparison,1,8))

aslaug<-data.frame(Prime, Comparison, Similarity, Sample1, Sample2)

#assigning a final name
idList<-aslaug


#importing sample details
library(xlsx)
stone<-read.xlsx("Sample details.xlsx",1)

details<-stone

#combining sample details with sample names from the matrix  
sampleNames<-rownames(idMat)
sampleNames<-substring(sampleNames,1,8)
sampleNames<-data.frame(sampleNames)
sampleNames<-unique(sampleNames)

descriptive<-merge(sampleNames,details,by.x="sampleNames", by.y ="CII.ID")

combined<-merge(idList,details, by.x = "Sample1", by.y = "CII.ID")
combined<-combined[,1:6]
colnames(combined)[6]<-"Animal1"
combined<-merge(combined,details, by.x = "Sample2", by.y = "CII.ID")
combined<-combined[,1:7]
colnames(combined)[7]<-"Animal2"