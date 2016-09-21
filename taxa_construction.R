idMat <- data.matrix(idMat)

#97
loweroriginal <- idMat
#loweroriginal[upper.tri(loweroriginal)]<-0
diag(loweroriginal)<-0
over97<-loweroriginal>=97
potential<-which(over97, arr.ind = TRUE)
columnvals<-c(potential[,2])
columnvals<-unique(columnvals)
taxalist<-list()
for (j in columnvals){
  sametaxa<-c(potential[which(potential[,2]==j),1])
  finaltaxa<-c(j)
  for(i in sametaxa){
    finaltaxa[length(finaltaxa)+1]<-i
    nextlevel<-c(potential[which(potential[,2]==i),1])
    sametaxa<-c(nextlevel,sametaxa)
    sametaxa<-unique(sametaxa)
  }
  if(j==1){
    taxalist[[length(taxalist)+1]]<-finaltaxa
  }
  else if(!is.element(finaltaxa,taxalist[[length(taxalist)]])[1]){
    taxalist[[length(taxalist)+1]]<-finaltaxa}
}

alltaxarows<-c()
for (i in taxalist){
  alltaxarows<-c(alltaxarows, i)
}
lonesomerows<-c()
for (i in (1:926)){
  if (i %in% alltaxarows){
  }else{
    lonesomerows <- c(lonesomerows, i)
    }
}
for (i in lonesomerows){
  taxalist[[length(taxalist)+1]]<-c(i)
}
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
scyther<-taxalist
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
taxalist<-blastoise
#record groups
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0("97taxa",i)
  taxanames[length(taxanames)+1]<-name
}
taxanames
olaf<-matrix(c(0), nrow=length(taxanames),ncol=926)
for(i in 1:length(taxalist)){
  x<-taxalist[[i]]
  for(j in 1:length(x)){
    y<-x[j]
    olaf[i,y]<-1
  }
}
olaf<-data.frame(olaf)
rownames(olaf)<-taxanames
colnames(olaf)<-colnames(loweroriginal)
taxa97<-olaf[,order(colnames(olaf))]
write.csv(taxa97,"taxa97.csv")

x <- 0
for(i in taxalist){
  x <- x + length(i)
}
x

#88
loweroriginal <- idMat
loweroriginal[upper.tri(loweroriginal)]<-0

over88<-loweroriginal>=88
potential<-which(over88, arr.ind = TRUE)
columnvals<-c(potential[,2])
columnvals<-unique(columnvals)
taxalist<-list()
for (j in columnvals){
  sametaxa<-c(potential[which(potential[,2]==j),1])
  finaltaxa<-c(j)
  for(i in sametaxa){
    finaltaxa[length(finaltaxa)+1]<-i
    nextlevel<-c(potential[which(potential[,2]==i),1])
    sametaxa<-c(nextlevel,sametaxa)
    sametaxa<-sametaxa[!sametaxa <= i]
    sametaxa<-unique(sametaxa)
  }
  if(j==1){
    taxalist[[length(taxalist)+1]]<-finaltaxa
  }
  else if(!is.element(finaltaxa,taxalist[[length(taxalist)]])[1]){
    taxalist[[length(taxalist)+1]]<-finaltaxa}
}

alltaxarows<-c()
for (i in taxalist){
  alltaxarows<-c(alltaxarows, i)
}
lonesomerows<-c()
for (i in (1:926)){
  if (i %in% alltaxarows){
  }else{
    lonesomerows <- c(lonesomerows, i)
  }
}
for (i in lonesomerows){
  taxalist[[length(taxalist)+1]]<-c(i)
}
#record groups
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0("88taxa",i)
  taxanames[length(taxanames)+1]<-name
}
taxanames
olaf<-matrix(c(0), nrow=length(taxanames),ncol=926)
for(i in 1:length(taxalist)){
  x<-taxalist[[i]]
  for(j in 1:length(x)){
    y<-x[j]
    olaf[i,y]<-1
  }
}
olaf<-data.frame(olaf)
rownames(olaf)<-taxanames
colnames(olaf)<-colnames(loweroriginal)
taxa88<-olaf[,order(colnames(olaf))]
write.table(taxa88,"taxa88.txt",sep=",")

x <- 0
for(i in taxalist){
  x <- x + length(i)
}
x


#81
loweroriginal <- idMat
loweroriginal[upper.tri(loweroriginal)]<-0

over81<-loweroriginal>=81
potential<-which(over81, arr.ind = TRUE)
columnvals<-c(potential[,2])
columnvals<-unique(columnvals)
taxalist<-list()
for (j in columnvals){
  sametaxa<-c(potential[which(potential[,2]==j),1])
  finaltaxa<-c(j)
  for(i in sametaxa){
    finaltaxa[length(finaltaxa)+1]<-i
    nextlevel<-c(potential[which(potential[,2]==i),1])
    sametaxa<-c(nextlevel,sametaxa)
    sametaxa<-sametaxa[!sametaxa <= i]
    sametaxa<-unique(sametaxa)
  }
  if(j==1){
    taxalist[[length(taxalist)+1]]<-finaltaxa
  }
  else if(!is.element(finaltaxa,taxalist[[length(taxalist)]])[1]){
    taxalist[[length(taxalist)+1]]<-finaltaxa}
}

alltaxarows<-c()
for (i in taxalist){
  alltaxarows<-c(alltaxarows, i)
}
lonesomerows<-c()
for (i in (1:926)){
  if (i %in% alltaxarows){
  }else{
    lonesomerows <- c(lonesomerows, i)
  }
}
for (i in lonesomerows){
  taxalist[[length(taxalist)+1]]<-c(i)
}
#record groups
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0("81taxa",i)
  taxanames[length(taxanames)+1]<-name
}
taxanames
olaf<-matrix(c(0), nrow=length(taxanames),ncol=926)
for(i in 1:length(taxalist)){
  x<-taxalist[[i]]
  for(j in 1:length(x)){
    y<-x[j]
    olaf[i,y]<-1
  }
}
olaf<-data.frame(olaf)
rownames(olaf)<-taxanames
colnames(olaf)<-colnames(loweroriginal)
taxa81<-olaf[,order(colnames(olaf))]
write.table(taxa81,"taxa81.txt",sep=",")

x <- 0
for(i in taxalist){
  x <- x + length(i)
}
x














#singletons
x <- 0
for(i in taxalist){
  x <- x + length(i)
}
x