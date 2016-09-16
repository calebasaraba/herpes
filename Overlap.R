#95
over96<-loweroriginal>=95
potential<-which(over96, arr.ind = TRUE)
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

#record groups
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0("95taxa",i)
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
taxa96<-olaf[,order(colnames(olaf))]
taxa96r<-matrix(c(0),nrow=length(taxanames),ncol=length(sampleNames))
rownames(taxa96r)<-taxanames
colnames(taxa96r)<-sampleNames
for(i in 1:length(sampleNames)){
  x<-sampleNames[i]
  xmat<-taxa96[,substring(colnames(taxa96),1,8)==x]
  smat<-rep(0,length(taxa96[,1]))
  if(is.null(dim(xmat))){
    taxa96r[,i]<-xmat
    next
  }
  for(j in 1:length(xmat)){
    smat<-smat+xmat[,j]
  }
  for(k in 1:length(smat)){
    if(smat[k]>0){smat[k]<-1}
  }
  taxa96r[,i]<-smat
}
taxa96r<-data.frame(taxa96r)
taxa96r<-taxa96r[,order(colnames(taxa96r))]

write.table(taxa96r,"taxa95.txt",sep=" ")
otaxa95<-taxa96r

#96
over96<-loweroriginal>=96
potential<-which(over96, arr.ind = TRUE)
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

#record groups
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0("96taxa",i)
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
taxa96<-olaf[,order(colnames(olaf))]
taxa96r<-matrix(c(0),nrow=length(taxanames),ncol=length(sampleNames))
rownames(taxa96r)<-taxanames
colnames(taxa96r)<-sampleNames
for(i in 1:length(sampleNames)){
  x<-sampleNames[i]
  xmat<-taxa96[,substring(colnames(taxa96),1,8)==x]
  smat<-rep(0,length(taxa96[,1]))
  if(is.null(dim(xmat))){
    taxa96r[,i]<-xmat
    next
  }
  for(j in 1:length(xmat)){
    smat<-smat+xmat[,j]
  }
  for(k in 1:length(smat)){
    if(smat[k]>0){smat[k]<-1}
  }
  taxa96r[,i]<-smat
}
taxa96r<-data.frame(taxa96r)
taxa96r<-taxa96r[,order(colnames(taxa96r))]
write.table(taxa96r,"taxa96.txt",sep=" ")
otaxa96<-taxa96r

#97
over96<-loweroriginal>=97
potential<-which(over96, arr.ind = TRUE)
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
taxa96<-olaf[,order(colnames(olaf))]
taxa96r<-matrix(c(0),nrow=length(taxanames),ncol=length(sampleNames))
rownames(taxa96r)<-taxanames
colnames(taxa96r)<-sampleNames
for(i in 1:length(sampleNames)){
  x<-sampleNames[i]
  xmat<-taxa96[,substring(colnames(taxa96),1,8)==x]
  smat<-rep(0,length(taxa96[,1]))
  if(is.null(dim(xmat))){
    taxa96r[,i]<-xmat
    next
  }
  for(j in 1:length(xmat)){
    smat<-smat+xmat[,j]
  }
  for(k in 1:length(smat)){
    if(smat[k]>0){smat[k]<-1}
  }
  taxa96r[,i]<-smat
}
taxa97<-taxa96
taxa96r<-data.frame(taxa96r)
taxa96r<-taxa96r[,order(colnames(taxa96r))]
write.table(taxa96r,"taxa97.txt",sep=" ")
otaxa97<-taxa96r

#98
over96<-loweroriginal>=98
potential<-which(over96, arr.ind = TRUE)
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

#record groups
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0("98taxa",i)
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
taxa96<-olaf[,order(colnames(olaf))]
taxa96r<-matrix(c(0),nrow=length(taxanames),ncol=length(sampleNames))
rownames(taxa96r)<-taxanames
colnames(taxa96r)<-sampleNames
for(i in 1:length(sampleNames)){
  x<-sampleNames[i]
  xmat<-taxa96[,substring(colnames(taxa96),1,8)==x]
  smat<-rep(0,length(taxa96[,1]))
  if(is.null(dim(xmat))){
    taxa96r[,i]<-xmat
    next
  }
  for(j in 1:length(xmat)){
    smat<-smat+xmat[,j]
  }
  for(k in 1:length(smat)){
    if(smat[k]>0){smat[k]<-1}
  }
  taxa96r[,i]<-smat
}
taxa96r<-data.frame(taxa96r)
taxa96r<-taxa96r[,order(colnames(taxa96r))]
write.table(taxa96r,"taxa98.txt",sep=" ")
otaxa98<-taxa96r

#86
over96<-loweroriginal>=86
potential<-which(over96, arr.ind = TRUE)
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

#record groups
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0("86taxa",i)
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
taxa96<-olaf[,order(colnames(olaf))]
taxa96r<-matrix(c(0),nrow=length(taxanames),ncol=length(sampleNames))
rownames(taxa96r)<-taxanames
colnames(taxa96r)<-sampleNames
for(i in 1:length(sampleNames)){
  x<-sampleNames[i]
  xmat<-taxa96[,substring(colnames(taxa96),1,8)==x]
  smat<-rep(0,length(taxa96[,1]))
  if(is.null(dim(xmat))){
    taxa96r[,i]<-xmat
    next
  }
  for(j in 1:length(xmat)){
    smat<-smat+xmat[,j]
  }
  for(k in 1:length(smat)){
    if(smat[k]>0){smat[k]<-1}
  }
  taxa96r[,i]<-smat
}
taxa96r<-data.frame(taxa96r)
taxa96r<-taxa96r[,order(colnames(taxa96r))]
write.table(taxa96r,"taxa86.txt",sep=" ")
otaxa86<-taxa96r

#87
over96<-loweroriginal>=87
potential<-which(over96, arr.ind = TRUE)
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

#record groups
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0("87taxa",i)
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
taxa96<-olaf[,order(colnames(olaf))]
taxa96r<-matrix(c(0),nrow=length(taxanames),ncol=length(sampleNames))
rownames(taxa96r)<-taxanames
colnames(taxa96r)<-sampleNames
for(i in 1:length(sampleNames)){
  x<-sampleNames[i]
  xmat<-taxa96[,substring(colnames(taxa96),1,8)==x]
  smat<-rep(0,length(taxa96[,1]))
  if(is.null(dim(xmat))){
    taxa96r[,i]<-xmat
    next
  }
  for(j in 1:length(xmat)){
    smat<-smat+xmat[,j]
  }
  for(k in 1:length(smat)){
    if(smat[k]>0){smat[k]<-1}
  }
  taxa96r[,i]<-smat
}
taxa96r<-data.frame(taxa96r)
taxa96r<-taxa96r[,order(colnames(taxa96r))]
write.table(taxa96r,"taxa87.txt",sep=" ")
otaxa87<-taxa96r

#88
over96<-loweroriginal>=88
potential<-which(over96, arr.ind = TRUE)
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
taxa96<-olaf[,order(colnames(olaf))]
taxa96r<-matrix(c(0),nrow=length(taxanames),ncol=length(sampleNames))
rownames(taxa96r)<-taxanames
colnames(taxa96r)<-sampleNames
for(i in 1:length(sampleNames)){
  x<-sampleNames[i]
  xmat<-taxa96[,substring(colnames(taxa96),1,8)==x]
  smat<-rep(0,length(taxa96[,1]))
  if(is.null(dim(xmat))){
    taxa96r[,i]<-xmat
    next
  }
  for(j in 1:length(xmat)){
    smat<-smat+xmat[,j]
  }
  for(k in 1:length(smat)){
    if(smat[k]>0){smat[k]<-1}
  }
  taxa96r[,i]<-smat
}
taxa88<-taxa96
taxa96r<-data.frame(taxa96r)
taxa96r<-taxa96r[,order(colnames(taxa96r))]
write.table(taxa96r,"taxa88.txt",sep=" ")
otaxa88<-taxa96r

#80
over96<-loweroriginal>=80
potential<-which(over96, arr.ind = TRUE)
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

#record groups
taxanames<-c()
for(i in 1:length(taxalist)){
  name<-paste0("80taxa",i)
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
taxa96<-olaf[,order(colnames(olaf))]
taxa96r<-matrix(c(0),nrow=length(taxanames),ncol=length(sampleNames))
rownames(taxa96r)<-taxanames
colnames(taxa96r)<-sampleNames


for(i in 1:length(sampleNames)){
  x<-sampleNames[i]
  xmat<-taxa96[,substring(colnames(taxa96),1,8)==x]
  smat<-rep(0,length(taxa96[,1]))
  if(is.null(dim(xmat))){
    taxa96r[,i]<-xmat
    next
  }
  for(j in 1:length(xmat)){
    smat<-smat+xmat[,j]
  }
  for(k in 1:length(smat)){
    if(smat[k]>0){smat[k]<-1}
  }
  taxa96r[,i]<-smat
}
taxa96r<-data.frame(taxa96r)
taxa96r<-taxa96r[,order(colnames(taxa96r))]

write.table(taxa96r,"taxa80.txt",sep=" ")
otaxa80<-taxa96r

#81
over96<-loweroriginal>=81
potential<-which(over96, arr.ind = TRUE)
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
taxa96<-olaf[,order(colnames(olaf))]
taxa96r<-matrix(c(0),nrow=length(taxanames),ncol=length(sampleNames))
rownames(taxa96r)<-taxanames
colnames(taxa96r)<-sampleNames
for(i in 1:length(sampleNames)){
  x<-sampleNames[i]
  xmat<-taxa96[,substring(colnames(taxa96),1,8)==x]
  smat<-rep(0,length(taxa96[,1]))
  if(is.null(dim(xmat))){
    taxa96r[,i]<-xmat
    next
  }
  for(j in 1:length(xmat)){
    smat<-smat+xmat[,j]
  }
  for(k in 1:length(smat)){
    if(smat[k]>0){smat[k]<-1}
  }
  taxa96r[,i]<-smat
}
taxa81<-taxa96
taxa96r<-data.frame(taxa96r)
taxa96r<-taxa96r[,order(colnames(taxa96r))]

write.table(taxa96r,"taxa81.txt",sep=" ")
otaxa81<-taxa96r