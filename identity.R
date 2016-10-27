library(doBy)

#function which finds the maximum observed similarity within 'data' sample after setting a cutoff for viral
#identity 'cutoff' and then runs 'sims' number of simulations where similarity scores have been randomly mixed
cSimSample<-function(data, cutoff, sims){
  b<-data
  b$Similarity<-replace(b$Similarity, b$Similarity >=cutoff, 0)
  c<-subset(b, substring(b$Sample1,1,8)==substring(b$Sample2, 1,8))
  d<-summaryBy(Similarity ~Sample1, data=c, FUN=max)
  maxObs<-max(d$Similarity.max)
  results<-rep(NA,sims)
  i<-1
  while (i <= sims){
    bRandom<-transform(b, Similarity = sample(Similarity))
    bRandomReduced<-bRandom[substring(b$Sample1,1,8)==substring(b$Sample2, 1,8),]
    cSim<-summaryBy(Similarity ~Sample1, data=bRandomReduced, FUN=max)
    results[i]<-max(cSim$Similarity.max)
    i<-i+1
  }
  list(maxObs,results)
}

sample100 <- cSimSample(idList, 100, 1000)
sample99 <- cSimSample(idList, 99, 1000)
sample98 <- cSimSample(idList, 98, 1000)
sample97<-cSimSample(idList,97,1000)
sample96<-cSimSample(idList,96,1000)
sample95<-cSimSample(idList,95,1000)


######################################################################
sample100<-cSimSample(combined,100,1000)
sample99<-cSimSample(combined,99,1000)
sample98<-cSimSample(combined,98,1000)
sample97<-cSimSample(combined,97,1000)
sample96<-cSimSample(combined,96,1000)
sample95<-cSimSample(combined,95,1000)


#Now does the same thing except for hosts, not sample
cSimHost<-function(data, cutoff, sims){
  b<-data
  b$Similarity<-replace(b$Similarity, b$Similarity >=cutoff, 0)
  c<-subset(b, b$Animal1==b$Animal2)
  d<-summaryBy(Similarity ~Animal1, data=c, FUN=max)
  maxObs<-max(d$Similarity.max)
  
  results<-rep(NA,sims)
  i<-1
  while (i <= sims){
    bRandom<-transform(b, Similarity = sample(Similarity))
    bRandomReduced<-bRandom[bRandom$Animal1==bRandom$Animal2,]
    cSim<-summaryBy(Similarity ~Animal1, data=bRandomReduced, FUN=max)
    results[i]<-max(cSim$Similarity.max)
    i<-i+1
  }
  list(maxObs,results)
}

host100<-cSimHost(combined,100,1000)
host99<-cSimHost(combined,99,1000)
host98<-cSimHost(combined,98,1000)
host97<-cSimHost(combined,97,1000)
host96<-cSimHost(combined,96,1000)
host95<-cSimHost(combined,95,1000)


#determining statistical 'significance' (Table 2)
statSig<-function(data,siglevel){
  apple<-quantile(data[[2]],c(siglevel/2,1-siglevel/2))
  list(data[[1]],apple,data[[1]] < apple[1], data[[1]] > apple[2])
}

#cutoff 100
statSig(sample100,.05)
statSig(host100,.05)

#99
statSig(sample99,.05)
statSig(host99,.05)

#98
statSig(sample98,.05)
statSig(host98,.05)

#97
statSig(sample97,.05)
statSig(host97,.05)

#96
statSig(sample96,.05)
statSig(host96,.05)

#95
statSig(sample95,.05)
statSig(host95,.05)

