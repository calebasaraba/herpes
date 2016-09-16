## FUNCTIONS
library(doBy)

dataframe1<-data.frame()

samplesimulate<-function(df, column, iterator, number){
  results<-rep(NA, number)
  i<-1
  while (i <= number){
    dfRandom<-transform(df, column = sample(column))
    dfRandomReduced<-subset(dfRandom, substring(dfRandom$Prime,1,8)==substring(dfRandom$Comparison, 1,8))
    foo<-summaryBy(column ~iterator, data=dfRandomReduced, FUN=max)
    results[i]<-max(foo$column.max)
    i<-i+1
  }
  results
}
samplesimulate(basaraba, Similarity, Sample, 1000)

findGreatest<-function()




print(createdata(original[3,1]))