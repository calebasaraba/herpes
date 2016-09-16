library(doBy)

originalSort<-original[order(rownames(original)), order(colnames(original))]

Prime<-rep(NA, nrow(originalSort)*ncol(originalSort))
Comparison<-rep(NA, nrow(originalSort)*ncol(originalSort))
Similarity<-rep(NA, nrow(originalSort)*ncol(originalSort))
Sample<-rep(NA, nrow(originalSort)*ncol(originalSort))

aslaug<-data.frame(Prime, Comparison, Similarity, Sample)


x <- 1
for (i in 1:(nrow(originalSort)-1)){
  for (j in (i+1):ncol(originalSort)){
    Prime[x] <- rownames(originalSort)[i]
    Comparison[x] <- colnames(originalSort)[j]
    Similarity[x] <- originalSort[i,j]
    x <- x+1
  }
}
Sample<-(substring(Prime,1,8))

basaraba<-merge(aslaug,details, by.x = "Sample", by.y = "CII.ID")

basarabaSimulated<- transform(basaraba, Similarity = sample(Similarity))
caleSimulated<-subset(basarabaSimulated, substring(basarabaSimulated$Prime,1,8)==substring(basarabaSimulated$Comparison, 1,8))
foo<-summaryBy(Similarity ~Sample, data=caleSimulated, FUN=max)


cale<-subset(basaraba, substring(basaraba$Prime,1,8)==substring(basaraba$Comparison, 1,8))



#dealing with 100.0's
caleSimulated$Similarity<-replace(caleSimulated$Similarity, caleSimulated$Similarity == 100.0, 0)


foo<-summaryBy(Similarity ~Sample, data=cale, FUN=max)

maximum<-max(foo$Similarity.max)
maximum


#set up basaraba
basaraba<-merge(aslaug,details, by.x = "Sample", by.y = "CII.ID")
#Number of sims
results<-rep(NA, 1000)
#Setting 100.0's to zero
basaraba$Similarity<-replace(basaraba$Similarity, basaraba$Similarity == 100.0, 0)
i<-1
while (i <= 1000){
  dfRandom<-transform(basaraba, Similarity = sample(Similarity))
  dfRandomReduced<-dfRandom[substring(dfRandom$Prime,1,8)==substring(dfRandom$Comparison, 1,8),]
  foo<-summaryBy(Similarity ~Sample, data=dfRandomReduced, FUN=max)
  results[i]<-max(foo$Similarity.max)
  i<-i+1
}
hist(results)


#set up basaraba
basaraba<-merge(aslaug,details, by.x = "Sample", by.y = "CII.ID")
##sensitivity > 95.0 to zero
results2<-rep(NA, 1000)
#Setting 100.0's to zero
basaraba$Similarity<-replace(basaraba$Similarity, basaraba$Similarity > 95.0, 0)
i<-1
while (i <= 1000){
  dfRandom<-transform(basaraba, Similarity = sample(Similarity))
  dfRandomReduced<-subset(dfRandom, substring(dfRandom$Prime,1,8)==substring(dfRandom$Comparison, 1,8))
  foo<-summaryBy(Similarity ~Sample, data=dfRandomReduced, FUN=max)
  results2[i]<-max(foo$Similarity.max)
  i<-i+1
}


#now do it based on animal, not sample
basic<-merge(aslaug,details, by.x = "Sample", by.y = "CII.ID")
#Setting 100.0's to zero

basic$Similarity<-replace(basic$Similarity, basic$Similarity ==100.0, 0)
basicFoo<-summaryBy(Similarity ~Original.Animal.ID, data=basic, FUN=max)
hostMax<-max(basicFoo$Similarity.max)

resultsHost<-rep(NA, 1000)
i<-1
while (i <= 1000){
  dfRandom<-transform(basic, Similarity = sample(Similarity))
  dfRandomReduced<-subset(dfRandom, substring(dfRandom$Prime,1,8)==substring(dfRandom$Comparison, 1,8))
  foo<-summaryBy(Similarity ~Sample, data=dfRandomReduced, FUN=max)
  resultsHost[i]<-max(foo$Similarity.max)
  i<-i+1
}
resultsHost

hist(dfRandomReduced$Similarity)



#T-test? comparison between 


#HISTOGRAM OF SIMILARITIES - 100 not removed

par(mgp=c(2,1,0))
basic$Similarity<-replace(basic$Similarity, basic$Similarity ==100.0, 0)
hist(basic$Similarity, breaks=seq(0,100, 0.5), xlab = "Percentage Similarity", ylab= "Frequency of Similarity", col="gray", main="Similarity Frequencies", xlim=c(25,100), xaxt="n")
axis(1, at=seq(0,100,1),tick = TRUE, labels=FALSE)
axis(1, at=seq(20,100,10), lwd.ticks = 2)



#Only removing 100s
b100<-merge(aslaug,details, by.x = "Sample", by.y = "CII.ID")
b100$Similarity<-replace(b100$Similarity, b100$Similarity ==100.0, 0)
c100<-subset(b100, substring(b100$Prime,1,8)==substring(b100$Comparison, 1,8))
d100<-summaryBy(Similarity ~Sample, data=c100, FUN=max)
max100<-max(d100$Similarity.max)
max100



results100<-rep(NA,1000)
i<-1
while (i <= 1000){
  dfRandom<-transform(b100, Similarity = sample(Similarity))
  dfRandomReduced<-dfRandom[substring(dfRandom$Prime,1,8)==substring(dfRandom$Comparison, 1,8),]
  foo<-summaryBy(Similarity ~Sample, data=dfRandomReduced, FUN=max)
  results100[i]<-max(foo$Similarity.max)
  i<-i+1
}
simmax100<-max(results100)

results99<-rep(NA,1000)
i<-1
while (i <= 1000){
  dfRandom<-transform(b99, Similarity = sample(Similarity))
  dfRandomReduced<-dfRandom[substring(dfRandom$Prime,1,8)==substring(dfRandom$Comparison, 1,8),]
  foo<-summaryBy(Similarity ~Sample, data=dfRandomReduced, FUN=max)
  results99[i]<-max(foo$Similarity.max)
  i<-i+1
}
simmax99<-max(results99)
simmax99

cAnalyze<-function(cutoff){
  b<-merge(aslaug,details, by.x = "Sample", by.y = "CII.ID")
  b$Similarity<-replace(b$Similarity, b$Similarity >=cutoff, 0)
  c<-subset(b, substring(b$Prime,1,8)==substring(b$Comparison, 1,8))
  d<-summaryBy(Similarity ~Sample, data=c, FUN=max)
  maxObs<-max(d$Similarity.max)
  
  results<-rep(NA,1000)
  i<-1
  while (i <= 1000){
    bRandom<-transform(b, Similarity = sample(Similarity))
    bRandomReduced<-bRandom[substring(bRandom$Prime,1,8)==substring(bRandom$Comparison, 1,8),]
    cSim<-summaryBy(Similarity ~Sample, data=bRandomReduced, FUN=max)
    results[i]<-max(cSim$Similarity.max)
    i<-i+1
  }
  list(maxObs,results)
}

test100<-cAnalyze(100)
test99<-cAnalyze(99)
test98<-cAnalyze(98)
test97<-cAnalyze(97)
test96<-cAnalyze(96)
test95<-cAnalyze(95)
test94<-cAnalyze(94)
test93<-cAnalyze(93)
test92<-cAnalyze(92)
test91<-cAnalyze(91)
test90<-cAnalyze(90)


h = hist(test100[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="No Cutoff", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 97.2")
points(test100[[1]],0, col="red", lwd=2)
truehist(test100[[2]])

h = hist(test99[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="99% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 97.2")
points(test99[[1]],0, col="red", lwd=2)

h = hist(test98[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="98% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 97.2")
points(test98[[1]],0, col="red", lwd=2)

h = hist(test97[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="97% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 96.5")
points(test97[[1]],0, col="red", lwd=2)
test97[[1]]
h$breaks
h$density


h = hist(test96[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="96% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 95.5")
points(test96[[1]],0, col="red", lwd=2)
test96[[1]]
h$breaks
h$density

h = hist(test92[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="92% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 91.9")
points(test92[[1]],0, col="red", lwd=2)
test92[[1]]
h$breaks
h$density

h = hist(test91[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="91% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 90.9")
points(test91[[1]],0, col="red", lwd=2)
test91[[1]]
h$breaks
h$density

h = hist(test90[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="90% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 88.6")
points(test90[[1]],0, col="red", lwd=2)
test90[[1]]
h$breaks
h$density


h = hist(test95[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="95% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 94.8")
points(test95[[1]],0, col="red", lwd=2)
test95[[1]]
h$breaks
h$density

cAnalyzeHost<-function(cutoff){
  b<-merge(aslaug,details, by.x = "Sample", by.y = "CII.ID")
  b$Similarity<-replace(b$Similarity, b$Similarity >=cutoff, 0)
  c<-subset(b, substring(b$Prime,1,8)==substring(b$Comparison, 1,8))
  d<-summaryBy(Similarity ~Original.Animal.ID, data=c, FUN=max)
  maxObs<-max(d$Similarity.max)
  
  results<-rep(NA,1000)
  i<-1
  while (i <= 1000){
    bRandom<-transform(b, Similarity = sample(Similarity))
    bRandomReduced<-bRandom[substring(bRandom$Prime,1,8)==substring(bRandom$Comparison, 1,8),]
    cSim<-summaryBy(Similarity ~Original.Animal.ID, data=bRandomReduced, FUN=max)
    results[i]<-max(cSim$Similarity.max)
    i<-i+1
  }
  list(maxObs,results)
}


host100<-cAnalyzeHost(100)
host99<-cAnalyzeHost(99)
host98<-cAnalyzeHost(98)
host97<-cAnalyzeHost(97)
host96<-cAnalyzeHost(96)
host95<-cAnalyzeHost(95)
host94<-cAnalyzeHost(94)
host93<-cAnalyzeHost(93)
host92<-cAnalyzeHost(92)
host91<-cAnalyzeHost(91)
host90<-cAnalyzeHost(90)

h = hist(host100[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="No Cutoff", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 97.2")
points(host100[[1]],0, col="red", lwd=2)
host100[[1]]
h$breaks
h$density

h = hist(host99[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="99% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 97.2")
points(host99[[1]],0, col="red", lwd=2)
host99[[1]]
h$breaks
h$density

h = hist(host98[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="98% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 97.2")
points(host98[[1]],0, col="red", lwd=2)
host98[[1]]
h$breaks
h$density

h = hist(host97[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="97% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 96.5")
points(host97[[1]],0, col="red", lwd=2)
host97[[1]]
h$breaks
h$density

h = hist(host96[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="96% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 95.5")
points(host96[[1]],0, col="red", lwd=2)
host96[[1]]
h$breaks
h$density

h = hist(host95[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="95% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 94.8")
points(host95[[1]],0, col="red", lwd=2)
host95[[1]]
h$breaks
h$density


h = hist(host94[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="94% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 93.6")
points(host94[[1]],0, col="red", lwd=2)
host94[[1]]
h$breaks
h$density

h = hist(host93[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="93% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 92.7")
points(host93[[1]],0, col="red", lwd=2)
host93[[1]]
h$breaks
h$density

h = hist(host92[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="92% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 91.9")
points(host92[[1]],0, col="red", lwd=2)
host92[[1]]
h$breaks
h$density

h = hist(host91[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="91% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 90.9")
points(host91[[1]],0, col="red", lwd=2)
host91[[1]]
h$breaks
h$density

h = hist(host90[[2]], axes=FALSE)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, ylab="% of Simulation Max Value", main="90% Considered Identical", xlab="% Genetic Similarity")
mtext("Maximum Observed Similarity = 88.6")
points(host90[[1]],0, col="red", lwd=2)
host90[[1]]
h$breaks
h$density

#Define new taxa
taxa1<-basaraba
taxa1<-taxa1[order(taxa1$Similarity, decreasing=TRUE),]
taxa1$Similarity<-replace(taxa1$Similarity, taxa1$Similarity == 100.0, 0)

z<-1
for (prime in taxa1$Prime){
  z<-z+1
  
}
?order
taxa<-data.frame(taxa1$Prime)


alpha = list()
for (i in 1:length(taxa1$Prime)){
  if (taxa1$Prime[i] >=96.0){
    
  }
  
}

diag(originalSort)<-1


for(j in 1:(length(originalSort[1,])-1)){
for(i in (j+1):(length(originalSort[1,])-1)){
if (originalSort[j,i] >= 96.0){
  alpha[i][[length(alpha)+1]]<-list(rownames(originalSort)[j], colnames(originalSort)[i], originalSort[j,i])
}
}
}
alpha=="PBZ-0801.1"

alpha[[1]]

first<-(alpha[[1]][[1]],alpha[[length(alpha)]][[1]])

alpha[[length(alpha)]][[1]]
alpha[[1]][[1]]


colnames(originalSort)<-rownames(originalSort)

colnames(originalSort[1,2])
originalSort[1,1] >= 96

beta<-list(rownames(originalSort)[1], colnames(originalSort)[2], originalSort[1,2])


HC<-hclust(as.dist(originalSort))

plot(HC)


##TAXA IDENTIFICATION



loweroriginal<-original
loweroriginal[upper.tri(loweroriginal)]<-0

##96.0
truth<-loweroriginal>=96

truthvectors<-which(truth, arr.ind=TRUE)
row.names(truthvectors)<-NULL


columnvals<-truthvectors[,'col']
columnvals<-unique(columnvals)
rowvals<-truthvectors[,'row']
brokenIndices<-list()

for(x in columnvals){
  writeLines(c("Working on column:",x))
  #print("working on column:")
  #print(x)
  test<-c(truthvectors[which(truthvectors[,'col']==x),1])
  #print("rows for this column")
  #print(test)
  for (i in test){
    #print("check this column")
    #print(i)
    testknife<-test[!test <= i] 
    #print("against these rows")
    #print(testknife)
    for (j in testknife){
      #print("row")
      #print(j)
      #print("index:")
      a<-which(truthvectors[,'col']==i & truthvectors[,'row']==j)
      #print(a)
      if (length(a)==0){
      writeLines(c("BROKEN", i,j))
      indices<-c(j,i)
      brokenIndices[[length(brokenIndices)+1]]<-indices
      }
    }
  }
}
print(brokenIndices)

brokenIndices<-unique(brokenIndices)

for (i in brokenIndices){
  row<-i[1]
  col<-i[2]
  print(original[row,col])
  
}

brokenIndices96<-brokenIndices

##95.0
truth<-loweroriginal>=95

truthvectors<-which(truth, arr.ind=TRUE)
row.names(truthvectors)<-NULL


columnvals<-truthvectors[,'col']
columnvals<-unique(columnvals)
rowvals<-truthvectors[,'row']
brokenIndices<-list()

for(x in columnvals){
  writeLines(c("Working on column:",x))
  test<-c(truthvectors[which(truthvectors[,'col']==x),1])
  for (i in test){
    testknife<-test[!test <= i] 
    for (j in testknife){
      a<-which(truthvectors[,'col']==i & truthvectors[,'row']==j)
      if (length(a)==0){
        writeLines(c("BROKEN", i,j))
        indices<-c(j,i)
        brokenIndices[[length(brokenIndices)+1]]<-indices
      }
    }
  }
}

brokenIndices<-unique(brokenIndices)
print(brokenIndices)

for (i in brokenIndices){
  row<-i[1]
  col<-i[2]
  print(original[row,col])
}

brokenIndices95<-brokenIndices

##97.0
truth<-loweroriginal>=97

truthvectors<-which(truth, arr.ind=TRUE)
row.names(truthvectors)<-NULL


columnvals<-truthvectors[,'col']
columnvals<-unique(columnvals)
rowvals<-truthvectors[,'row']
brokenIndices<-list()

for(x in columnvals){
  writeLines(c("Working on column:",x))
  test<-c(truthvectors[which(truthvectors[,'col']==x),1])
  for (i in test){
    testknife<-test[!test <= i] 
    for (j in testknife){
      a<-which(truthvectors[,'col']==i & truthvectors[,'row']==j)
      if (length(a)==0){
        writeLines(c("BROKEN", i,j))
        indices<-c(j,i)
        brokenIndices[[length(brokenIndices)+1]]<-indices
      }
    }
  }
}

brokenIndices<-unique(brokenIndices)
print(brokenIndices)

for (i in brokenIndices){
  row<-i[1]
  col<-i[2]
  print(original[row,col])
}

brokenIndices97<-brokenIndices

##85.0
truth<-loweroriginal>=85

truthvectors<-which(truth, arr.ind=TRUE)
row.names(truthvectors)<-NULL


columnvals<-truthvectors[,'col']
columnvals<-unique(columnvals)
rowvals<-truthvectors[,'row']
brokenIndices<-list()

for(x in columnvals){
  writeLines(c("Working on column:",x))
  test<-c(truthvectors[which(truthvectors[,'col']==x),1])
  for (i in test){
    testknife<-test[!test <= i] 
    for (j in testknife){
      a<-which(truthvectors[,'col']==i & truthvectors[,'row']==j)
      if (length(a)==0){
        writeLines(c("BROKEN", i,j))
        indices<-c(j,i)
        brokenIndices[[length(brokenIndices)+1]]<-indices
      }
    }
  }
}

brokenIndices<-unique(brokenIndices)
print(brokenIndices)

for (i in brokenIndices){
  row<-i[1]
  col<-i[2]
  print(original[row,col])
}

brokenIndices85<-brokenIndices

##86.0
truth<-loweroriginal>=86

truthvectors<-which(truth, arr.ind=TRUE)
row.names(truthvectors)<-NULL


columnvals<-truthvectors[,'col']
columnvals<-unique(columnvals)
rowvals<-truthvectors[,'row']
brokenIndices<-list()

for(x in columnvals){
  writeLines(c("Working on column:",x))
  test<-c(truthvectors[which(truthvectors[,'col']==x),1])
  for (i in test){
    testknife<-test[!test <= i] 
    for (j in testknife){
      a<-which(truthvectors[,'col']==i & truthvectors[,'row']==j)
      if (length(a)==0){
        writeLines(c("BROKEN", i,j))
        indices<-c(j,i)
        brokenIndices[[length(brokenIndices)+1]]<-indices
      }
    }
  }
}

brokenIndices<-unique(brokenIndices)
print(brokenIndices)

for (i in brokenIndices){
  row<-i[1]
  col<-i[2]
  print(original[row,col])
}

brokenIndices86<-brokenIndices

##87.0
truth<-loweroriginal>=87

truthvectors<-which(truth, arr.ind=TRUE)
row.names(truthvectors)<-NULL


columnvals<-truthvectors[,'col']
columnvals<-unique(columnvals)
rowvals<-truthvectors[,'row']
brokenIndices<-list()

for(x in columnvals){
  writeLines(c("Working on column:",x))
  test<-c(truthvectors[which(truthvectors[,'col']==x),1])
  for (i in test){
    testknife<-test[!test <= i] 
    for (j in testknife){
      a<-which(truthvectors[,'col']==i & truthvectors[,'row']==j)
      if (length(a)==0){
        writeLines(c("BROKEN", i,j))
        indices<-c(j,i)
        brokenIndices[[length(brokenIndices)+1]]<-indices
      }
    }
  }
}

brokenIndices<-unique(brokenIndices)
print(brokenIndices)

for (i in brokenIndices){
  row<-i[1]
  col<-i[2]
  print(original[row,col])
}

brokenIndices87<-brokenIndices

##80.0
truth<-loweroriginal>=80

truthvectors<-which(truth, arr.ind=TRUE)
row.names(truthvectors)<-NULL


columnvals<-truthvectors[,'col']
columnvals<-unique(columnvals)
rowvals<-truthvectors[,'row']
brokenIndices<-list()

for(x in columnvals){
  writeLines(c("Working on column:",x))
  test<-c(truthvectors[which(truthvectors[,'col']==x),1])
  for (i in test){
    testknife<-test[!test <= i] 
    for (j in testknife){
      a<-which(truthvectors[,'col']==i & truthvectors[,'row']==j)
      if (length(a)==0){
        writeLines(c("BROKEN", i,j))
        indices<-c(j,i)
        brokenIndices[[length(brokenIndices)+1]]<-indices
      }
    }
  }
}

brokenIndices<-unique(brokenIndices)
print(brokenIndices)

for (i in brokenIndices){
  row<-i[1]
  col<-i[2]
  print(original[row,col])
}

brokenIndices80<-brokenIndices

##81.0
truth<-loweroriginal>=81

truthvectors<-which(truth, arr.ind=TRUE)
row.names(truthvectors)<-NULL


columnvals<-truthvectors[,'col']
columnvals<-unique(columnvals)
rowvals<-truthvectors[,'row']
brokenIndices<-list()

for(x in columnvals){
  writeLines(c("Working on column:",x))
  test<-c(truthvectors[which(truthvectors[,'col']==x),1])
  for (i in test){
    testknife<-test[!test <= i] 
    for (j in testknife){
      a<-which(truthvectors[,'col']==i & truthvectors[,'row']==j)
      if (length(a)==0){
        writeLines(c("BROKEN", i,j))
        indices<-c(j,i)
        brokenIndices[[length(brokenIndices)+1]]<-indices
      }
    }
  }
}

brokenIndices<-unique(brokenIndices)
print(brokenIndices)

for (i in brokenIndices){
  row<-i[1]
  col<-i[2]
  print(original[row,col])
}

brokenIndices81<-brokenIndices

##82.0
truth<-loweroriginal>=82

truthvectors<-which(truth, arr.ind=TRUE)
row.names(truthvectors)<-NULL


columnvals<-truthvectors[,'col']
columnvals<-unique(columnvals)
rowvals<-truthvectors[,'row']
brokenIndices<-list()

for(x in columnvals){
  writeLines(c("Working on column:",x))
  test<-c(truthvectors[which(truthvectors[,'col']==x),1])
  for (i in test){
    testknife<-test[!test <= i] 
    for (j in testknife){
      a<-which(truthvectors[,'col']==i & truthvectors[,'row']==j)
      if (length(a)==0){
        writeLines(c("BROKEN", i,j))
        indices<-c(j,i)
        brokenIndices[[length(brokenIndices)+1]]<-indices
      }
    }
  }
}

brokenIndices<-unique(brokenIndices)
print(brokenIndices)

for (i in brokenIndices){
  row<-i[1]
  col<-i[2]
  print(original[row,col])
}

brokenIndices82<-brokenIndices



