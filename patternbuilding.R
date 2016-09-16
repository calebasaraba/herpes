library(cooccur)

otaxa88co<-cooccur(otaxa88)

prob.table(otaxa88co)

plot(otaxa88co)

plot(otaxa97co)

mat<-otaxa97co$results

otaxa97coR<-otaxa97co
otaxa97coR$results<-mat[mat[,'p_lt']<0.0001 | mat[,'p_gt']<0.0001 ,]

plot(otaxa97coR)

#Reduced 88
mat<-otaxa88co$results

otaxa88coR<-otaxa88co
otaxa88coR$results<-mat[mat[,'p_lt']<0.0001 | mat[,'p_gt']<0.0001 ,]

plot(otaxa88coR)

#Reduced 81
mat<-otaxa81co$results

otaxa81coR<-otaxa81co
otaxa81coR$results<-mat[mat[,'p_lt']<0.0001 | mat[,'p_gt']<0.0001 ,]

plot(otaxa81coR)

#GETTING ACTUAL SAMPLES
#positive relationships 97
t9714names<-colnames(taxa97[,taxa97['97taxa14',]==1])
t9713names<-colnames(taxa97[,taxa97['97taxa13',]==1])

t979names<-colnames(taxa97[,taxa97['97taxa9',]==1])
t978names<-colnames(taxa97[,taxa97['97taxa8',]==1])

t9731names<-colnames(taxa97[,taxa97['97taxa31',]==1])
t9711names<-colnames(taxa97[,taxa97['97taxa11',]==1])

prel97<-list(t9714names,t9713names,t979names,t978names,t9731names,t9711names)

#positive relationships 88
t8820names<-colnames(taxa88[,taxa88['88taxa20',]==1])
t888names<-colnames(taxa88[,taxa88['88taxa8',]==1])

t8811names<-colnames(taxa88[,taxa88['88taxa11',]==1])

t887names<-colnames(taxa88[,taxa88['88taxa7',]==1])
t886names<-colnames(taxa88[,taxa88['88taxa6',]==1])

prel88<-list(t8820names,t888names,t8811names,t887names,t886names)

#positive relationships 81
t8119names<-colnames(taxa81[,taxa81['81taxa19',]==1])
t817names<-colnames(taxa81[,taxa81['81taxa7',]==1])

t8110names<-colnames(taxa81[,taxa81['81taxa10',]==1])

t8117names<-colnames(taxa81[,taxa81['81taxa17',]==1])
t816names<-colnames(taxa81[,taxa81['81taxa6',]==1])

prel81<-list(t8119names,t817names,t8110names,t8117names,t816names)

for(i in 1:length(prel97)){
  for(j in 1:length(prel88)){
  if(setequal(prel97[[i]],prel88[[j]])){
    print("Two equal Taxa:")
    print(i)
    print(j)
    }
  } 
}

for(i in 1:length(prel88)){
  for(j in 1:length(prel81)){
    if(setequal(prel88[[i]],prel81[[j]])){
      print("Two equal Taxa:")
      print(i)
      print(j)
    }
  } 
}

setdiff(t8811names,t9714names)
setdiff(t9714names,t8811names)

is.element(c(t9714names,t9713names),t8811names)
is.element(t9714names,t8811names)
is.element(t9713names,t8811names)

is.element(t887names,t8117names)
is.element(t886names,t8117names)

length(t816names)-sum(is.element(t886names,t816names))

for(i in 1:length(prel97)){
  for(j in 1:length(prel88)){
    if(sum(is.element(prel97[[i]],prel88[[j]]))>0){
      print("X is a subset of Y:")
      print(i)
      print(j)
      print("length of X:")
      print(length(prel97[[i]]))
      print("length of Y:")
      print(length(prel88[[j]]))
    }
  }
}

for(i in 1:length(prel88)){
  for(j in 1:length(prel81)){
    if(sum(is.element(prel88[[i]],prel81[[j]]))>0){
      print("X is a subset of Y:")
      print(i)
      print(j)
      print("length of X:")
      print(length(prel88[[i]]))
      print("length of Y:")
      print(length(prel81[[j]]))
    }
  }
}

is.element(t9714names,t888names)
is.element(t9713names,t888names)


setdiff(c(t9714names,t9713names,t9711names),t888names)

setdiff(t888names,c(t9714names,t9713names,t9711names))


for(i in 1:length(prel88)){
  for(j in 1:length(prel81)){
    if(sum(is.element(prel88[[i]],prel81[[j]]))>0){
      print("X is a subset of Y:")
      print(i)
      print(j)
    }
  }
}

length(t816names)-sum(is.element(t887names,t816names))
length(t816names)
length(t887names)

for(i in 1:length(prel97)){
  for(j in 1:length(prel81)){
    if(sum(is.element(prel97[[i]],prel81[[j]]))>0){
      print("X is a subset of Y:")
      print(i)
      print(j)
    }
  }
}

sum(is.element(t886names,t816names))


#catch negative relationships
#97
plot(otaxa97coR)
plot(otaxa88coR)
plot(otaxa81coR)

#97
t9741names<-colnames(taxa97[,taxa97['97taxa41',]==1])
t979names<-colnames(taxa97[,taxa97['97taxa9',]==1])
t9711names<-colnames(taxa97[,taxa97['97taxa11',]==1])
t9743names<-colnames(taxa97[,taxa97['97taxa43',]==1])
t978names<-colnames(taxa97[,taxa97['97taxa8',]==1])
t9734names<-colnames(taxa97[,taxa97['97taxa34',]==1])
nrel97<-list(t9741names,t979names,t9711names,t9743names,t978names,t9734names)
nrel97index<-c(41,9,11,43,8,34)

#88
t8823names<-colnames(taxa88[,taxa88['88taxa23',]==1])
t887names<-colnames(taxa88[,taxa88['88taxa7',]==1])
t886names<-colnames(taxa88[,taxa88['88taxa6',]==1])
t888names<-colnames(taxa88[,taxa88['88taxa8',]==1])
t8831names<-colnames(taxa88[,taxa88['88taxa31',]==1])
t8830names<-colnames(taxa88[,taxa88['88taxa30',]==1])
nrel88<-list(t8823names,t887names,t886names,t888names, t8831names, t8830names)
nrel88index<-c(23,7,6,8,31,30)

#81
t8121names<-colnames(taxa81[,taxa81['81taxa21',]==1])
t816names<-colnames(taxa81[,taxa81['81taxa6',]==1])
t817names<-colnames(taxa81[,taxa81['81taxa7',]==1])
t8129names<-colnames(taxa81[,taxa81['81taxa29',]==1])
t8128names<-colnames(taxa81[,taxa81['81taxa28',]==1])
nrel81<-list(t8121names,t816names,t817names,t8129names,t8128names)
nrel81index<-c(21,6,7,29,28)

for(i in 1:length(nrel97)){
  for(j in 1:length(nrel88)){
    if(setequal(nrel97[[i]],nrel88[[j]])){
      print("Two equal Taxa:")
      print(nrel97index[i])
      print(nrel88index[j])
    }
  } 
}

setequal(t978names,t886names)
setequal(t9734names,t8823names)



for(i in 1:length(nrel88)){
  for(j in 1:length(nrel81)){
    if(setequal(nrel88[[i]],nrel81[[j]])){
      print("Two equal Taxa:")
      print(nrel88index[i])
      print(nrel81index[j])
    }
  } 
}

setequal(t8823names,t8121names)
setequal(t888names,t817names)
setequal(t8831names,t8129names)
setequal(t8830names,t8128names)


for(i in 1:length(nrel97)){
  for(j in 1:length(nrel81)){
    if(setequal(nrel97[[i]],nrel81[[j]])){
      print("Two equal Taxa:")
      print(nrel97index[i])
      print(nrel81index[j])
    }
  } 
}

plot(otaxa88coR)
prob.table(otaxa88co)

setequal(t888names,t8129names)


for(i in 1:length(nrel97)){
  for(j in 1:length(nrel88)){
    if(sum(is.element(nrel97[[i]],nrel88[[j]]))>0){
      print("X is a subset of Y:")
      print(nrel97index[i])
      print(nrel88index[j])
      print("length of X:")
      print(length(nrel97[[i]]))
      print("length of Y:")
      print(length(nrel88[[j]]))
    }
  }
}

for(i in 1:length(nrel88)){
  for(j in 1:length(nrel81)){
    if(sum(is.element(nrel88[[i]],nrel81[[j]]))>0){
      print("X is a subset of Y:")
      print(nrel88index[i])
      print(nrel81index[j])
      print("length of X:")
      print(length(nrel88[[i]]))
      print("length of Y:")
      print(length(nrel81[[j]]))
    }
  }
}

