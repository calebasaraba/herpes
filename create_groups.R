
create_groups <- function(idMat, cutoff){
loweroriginal <- idMat
diag(loweroriginal)<-0
over<-loweroriginal>=cutoff
potential<-which(over, arr.ind = TRUE)
columnvals<-c(potential[,2])
columnvals<-unique(columnvals)
taxalist<-list()
for(j in columnvals){
  sametaxa<-c(potential[which(potential[,2]==j),1])
  finaltaxa<-c(j)
  for(i in sametaxa){
    finaltaxa[length(finaltaxa)+1]<-i
    nextlevel<-c(potential[which(potential[,2]==i),1])
    sametaxa<-c(nextlevel,sametaxa)
    sametaxa<-unique(sametaxa)
  }
  if(j==columnvals[1]){
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
for (i in (1:length(colnames(idMat)))){
  if (i %in% alltaxarows){
  }else{
    lonesomerows <- c(lonesomerows, i)
  }
}
for (i in lonesomerows){
  taxalist[[length(taxalist)+1]]<-c(i)
}
#this is a reduction function that makes sure there is no overlap between groups
reduceT <- function(x, input){
  b<-c()
  for(i in 1:length(input)){
    a<-intersect(x, input[[i]])
    if(length(a)>0){
      if(identical(x, a)){
        b<-c(b,x)
        b<-unique(b)
      }else{
        b<- c(b,x, input[[i]])
        b<-unique(b)
      }
    }
  }
  return(b)
}

#this is first round of reduction, just in case there is overlap
scyther<-taxalist
arcanine<-list()
for(i in 1:length(scyther)){
  g <- reduceT(scyther[[i]], scyther)
  arcanine[[length(arcanine)+1]]<-g
}

# this reduces taxalists again
blastoise<-list()
for(i in 1:length(arcanine)){
  g <- reduceT(arcanine[[i]], arcanine)
  blastoise[[length(blastoise)+1]]<-g
}
blastoise<-lapply(blastoise, sort)
blastoise<-unique(blastoise)
taxalist<-blastoise
# this creates the taxanames you see together
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0(cutoff,"group",i)
  taxanames[length(taxanames)+1]<-name
}
# this creates the output dataframe
taxanames
olaf<-matrix(c(0), nrow=length(taxanames),ncol=length(colnames(idMat)))
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
final_groups<-olaf[,order(colnames(olaf))]
final_groups
results <- list(taxalist, final_groups)
}