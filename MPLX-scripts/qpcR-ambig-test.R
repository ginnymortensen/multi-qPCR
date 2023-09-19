########################################################################
############# IN PROGRESS: qPCR data analysis optimization #############
#############              for multiplex DPCR COV2 assays. #############
#############                              -GAM 06/13/2020 #############
########################################################################

#########################################################################################
##### INSTRUCTIONS ######################################################################
##### Import .sds file from instrument to Design & Analysis software by ThermoFisher ####
##### Export multicomponent file (pre-set export setting available) to files ############
#########################################################################################

# Load required packages
library(qpcR)
library(tidyverse)
library(sicegar)

# Choose file and directory
myObj <- read.table(file.choose(), sep="\t", header=TRUE)
setwd("C:/Users/srlgam/Desktop/Projects/DPCR-test/data/qpcR-analysis")

# Tidy data file
myObj <- rename(myObj, TEXASRED = TEXAS.RED)
gene <- c("VIC", "FAM", "TEXASRED", "CY5")
genelist <- list()

for(i in gene){
  geneSet <- select(myObj, Cycle.Number, Well.Position, matches(i))
  geneSet <- reshape(geneSet, 
                     idvar = "Cycle.Number", 
                     timevar = "Well.Position", 
                     direction = "wide")
  geneSet <- geneSet[-c(1)]
  genelist[[length(genelist)+1]] = geneSet
  assign(paste("gene", i, sep = "_"), geneSet) %>%
    write.table(file = paste0("COV2_06xx2020_01_", i), 
                row.names = TRUE, 
                col.names = NA)
}

# Import tidy data
path2 <- Sys.glob(file.path("C:/Users/srlgam/Desktop/Projects/DPCR-test/data/qpcR-analysis"))
rawlist <- pcrimport(file = path2,
                  sep = "", dec = ".",
                  delCol = 1, delRow = 0,
                  format = "col", sampleDat = 0,
                  names = 1, sampleLen = 0, 
                  check = FALSE)

# Conduct analysis
fitlist <- list()
toplist <- list()
modlisty <- list()

for(j in 1:4){
  fitSet = rawlist[[j]]
  fitfunc = pcrfit(data = fitSet, fluo = 2:97)
  fitlist[[length(fitlist)+1]] = fitfunc
  
  topSet = fitlist[[j]]
  topfunc = takeoff(topSet)
  toplist[[length(toplist)+1]] = topfunc
  
  modSet = fitlist[[j]]
  modfunc = mselect(modSet, do.all = TRUE)
  modlisty[[length(modlisty)+1]] = modfunc 
}

# Repopulate with graphs
plotlist <- list()
for(k in 1:4){
  thatdat <- as.data.frame(fitlist[[k]]$DATA)
  p = ggplot(data = thatdat, aes(x = Cycles, y = Fluo)) +
    geom_point() +
    geom_vline(xintercept = toplist[[k]]$top) +
    geom_hline(yintercept = toplist[[k]]$f.top)
  plotlist[[k]] = p
  print(plotlist[[k]])
}

thisList = list()
sampleList = list()
sampleList2 =
for(i in colnames(geneSet)){
  time <- seq(1, 45, 1)
  intensity <- pull(geneSet, i)
  df1 <- data.frame(time, intensity)
  sampleSet <- assign(colnames(geneSet)[i], data.frame(df1))
  #sampleSet <- c(sampleList[[i]])
  dataOutput = dataCheck(sampleSet, showDetails = TRUE)
  fitCatObj = fitAndCategorize(sampleSet)
  #sampleList[[length(sampleList)+1]] = dataOutput
  #sampleList2[[length(sampleList2+1)]] = fitCatObj
  
  #print(fitCatObj$summaryVector)
}
