library(sicegar)
library(tidyverse)

# generate data frame
thisList = list()
sampleList = list()
for(i in colnames(geneSet)){
  time <- seq(1, 45, 1)
  intensity <- pull(geneSet, i)
  df1 <- data.frame(time, intensity)
  assign(colnames(geneSet)[i], data.frame(df1))
  thisList
  sampleSet = sampleList[[i]]
  dataOutput = dataCheck(sampleSet, showDetails = TRUE)
  fitCatObj = fitAndCategorize(sampleSet)
  sampleList[[length(sampleList)+1]] = dataOutput
  sampleList2[[length(sampleList2+1)]] = fitCatObj
  
  print(fitCatObj$summaryVector)
}

  for(i in colnames(geneSet)){
    time <- seq(1, 45, 1)
    intensity <- pull(geneSet, i)
    df1 <- data.frame(time, intensity)
    sampleSet <- assign(colnames(geneSet)[i], data.frame(df1))
    dataOutput = dataCheck(sampleSet, showDetails = TRUE)
    fitCatObj = fitAndCategorize(sampleSet)
  }
