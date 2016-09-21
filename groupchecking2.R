library(tidyverse)

idMat <- data.matrix(idMat)

over97 <- idMat >= 97
potential<-which(over97, arr.ind = TRUE)
first<- potential %>%
  filter(col==1)


findall<-function(col, data){
  a<-c()
  a<-c(data[which(data[,2]==col),1])
  a
}

tlist<-list()
a<-findall(1,potential)
b<-c(1)
for(i in a){
  b<-findall(i,potential)
  a<-a[-i]
}

tlist<-list()
for(i in (1:length(potential))){
  a<-findall(i,potential)
  tlist[[length(tlist)+1]]<-a
}


a<-c(1)
a<-c(a,findall(1,potential))

cvals<-unique(potential[,2])
head(cvals)

tlist<-list()
for(i in cvals){
  a<-findall(i,potential)
  tlist[[length(tlist)+1]]<-a
}

tlist

######################################## NEW 9/20

scyther <- taxalist

arcanine <- list()

reduceT <- function(x, input){
  b<-c()
  for(i in 1:length(input)){
    a<-intersect(x, input[[i]])
    if(length(a)>0){
      if(identical(x, a)){
        print("Identical")
        b<-c(b,x)
        b<-unique(b)
      }else{
      print(paste('Taxa',i,":"))  
      print(x)
      b<- c(b,x, input[[i]])
      b<-unique(b)
      print(paste('New Taxa:'))
      print(b)
      }
    }
  }
  return(b)
}

arcanine<-list()
for(i in 1:length(scyther)){
  g <- reduceT(scyther[[i]], scyther)
  arcanine[[length(arcanine)+1]]<-g
}

blastoise<-list()
for(i in 1:length(arcanine)){
  g <- reduceT(arcanine[[i]], arcanine)
  blastoise[[length(blastoise)+1]]<-g
}


blastoise<-lapply(blastoise, sort)
blastoise<-unique(blastoise)



x <- 0
for(i in blastoise){
  x <- x + length(i)
}
x








