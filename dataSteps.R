rownames(platypus)<-substr(x = rownames(platypus),start = 7, stop = 33)
colnames(platypus)<-rownames(platypus)

dinosaur<-colnames(platypus)

test<-dinosaur

ifelse(substring(dinosaur, 1, 1) == "_", substring(dinosaur,2),dinosaur)


test<-ifelse(substring(dinosaur, 1, 1) == "_", substring(dinosaur,2),dinosaur)
test<-ifelse(substring(test, 10, nchar(test)) == "Alignment consensu" 
             | substring(test, 10, nchar(test)) == "Alignment consens",
             substring(test,1,8), test)

test<-ifelse(nchar(test) == 8, paste(test,".1", sep = ""), test)
test<-ifelse(substring(test,19,19) == "1", paste(substring(test,1,8), ".1", sep = ""), test)
test<-ifelse(substring(test,19,19) == "2", paste(substring(test,1,8), ".2", sep = ""), test)
test<-ifelse(substring(test,19,19) == "3", paste(substring(test,1,8), ".3", sep = ""), test)
test<-ifelse(substring(test,19,19) == "4", paste(substring(test,1,8), ".4", sep = ""), test)
test<-ifelse(substring(test,19,19) == "5", paste(substring(test,1,8), ".5", sep = ""), test)
test<-ifelse(substring(test, 10,13) == "Only", paste(substring(test,1,8),".1", sep = ""), test)
test<-ifelse(substring(test, 10,11) == "Al", paste(substring(test,1,8),".1", sep = ""), test)
test
rownames(platypus)<-test
colnames(platypus)<-test




#
#
#
#
#
#
#

ifelse(substring(rownames(platypus),10,10) == 1, aardvark$count<-1, aardvark$count<aardvark$count)

aardvark$count<-ifelse(substring(rownames(platypus),10,10) == 1, 1, 0)
aardvark$count<-ifelse(substring(rownames(platypus),10,10) == 2, 2, aardvark$count)
aardvark$count<-ifelse(substring(rownames(platypus),10,10) == 3, 3, aardvark$count)
aardvark$count<-ifelse(substring(rownames(platypus),10,10) == 4, 4, aardvark$count)
aardvark$count<-ifelse(substring(rownames(platypus),10,10) == 5, 5, aardvark$count)
aardvark$sample<-substring(rownames(aardvark),1,8)

index<-match(aardvark$sample, details$CII.ID)
index


together<-merge(aardvark,details, by.x = "sample", by.y = "CII.ID")


sample<-aardvardk

original<-sample

sample <- original[sample(nrow(original)),]

sample <-original[sample(nrow(original)),sample(ncol(original))]

##sorting try
doublesorted$sample<-substring(rownames(doublesorted),1,8)
doublesorted<-merge(doublesorted,details, by.x = "sample", by.y = "CII.ID")
justone<-doublesorted[which(doublesorted$sample == 'PBZ-0801'), substring(colnames(doublesorted),1,8)== 'PBZ-0801']

onesample<-doublesorted[which(doublesorted$sample == 'PBZ-0803'), substring(colnames(doublesorted),1,8)== 'PBZ-0803']


doublesorted2<-original[sample(nrow(original)),sample(ncol(original))]
doublesorted2<-doublesorted2[order(rownames(doublesorted2)), order(colnames(doublesorted2))]
doublesorted2$sample<-substring(rownames(doublesorted2),1,8)
doublesorted2<-merge(doublesorted2,details, by.x = "sample", by.y = "CII.ID")

samplematrix<-matrix()


