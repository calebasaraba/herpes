loweroriginal
sum(loweroriginal[3,] > 97)

zeros <- 0
for (i in 2:926){
  if ((sum(loweroriginal[i,] > 81)) == 0){
    zeros <- zeros + 1
  }
}

row.names

blarg <- list()
for (i in (1:926)){
  if(sum(taxa97[,i]) > 1){
    blarg[[length(blarg)+1]]<-i
  }
}


test <- taxalist
for(i in (1:length(taxalist))){
  for(j in (2:length(taxalist))){
    a<-intersect(taxalist[[i]], taxalist[[j]])
    if(length(a) > 0){
      taxalist[[i]]<-c(taxalist[[i]], taxalist[[j]])
      taxalist[[i]]<-unique(taxalist[[i]])
      taxalist<-taxalist[-j]
    }
  }
}

taxalist <- lapply(taxalist, unique)

check_overlap <- function(group,group_list){
  overlaps<-c()
  for(i in (1:length(group_list))){
    a <- length(intersect(group_list[[i]], group))
    overlaps <- c(overlaps, a)
  }
  print(overlaps)
  
}


group <- taxalist[[1]]
n <- c()
for(i in (1:length(taxalist))){
  a <- length(intersect(group, taxalist[[i]]))
  if(a > 0){
    n <- c(n,taxalist[[i]],taxalist[[1]])
  }else{
    n <- c(n,taxalist[[1]])
  }
}

a <- c(taxalist[[1]])

check_overlap(taxalist[[9]], taxalist)



for(i in taxalist){
  for(j in taxalist){
    if(length(intersect(taxalist[[i]],taxalist[[j]]))>0){
      taxalist[[i]]<-c(taxalist[[i]], taxalist[[j]])
    }
  }
}

a <- c(1, 5, 4)
b <- c(2, 22, 3)

length(intersect(a,b)) == 0


blarg <- list()
for (i in (1:926)){
  if(sum(taxa88[,i]) > 1){
    blarg[[length(blarg)+1]]<-i
  }
}

blarg <- list()
for (i in (1:926)){
  if(sum(taxa81[,i]) > 1){
    blarg[[length(blarg)+1]]<-i
  }
}

taxalist[[1]]
taxalist[1]

temp <- taxalist

for (i in temp){
  for(j in temp){
    if(length(intersect(i,j))>0){
      print(length(intersect(i,j)))
      i<-c(i,j)
    }
  }
}
temp<-unique(temp)

