
## Problems: current Results files have no Amp Score cols. Need to edit export file settings
##  on instruments and adjust Brian's code to accept the additional column when importing files.

library(tidyverse)

setwd("C:/Users/srlgam/Desktop/Projects")
fileList <- list.files(pattern = "^MPLEX(.*)_Result(.*)txt$")
fileList

all.data = data.frame()
for(i in fileList){
  this.file <- read.table(file = i, header = TRUE, sep = "\t")
  print(i)
  for(j in colnames(this.file)){
    if(j == "Amp.Score"){
      all.data <- rbind(all.data, this.file)
    }
  }
}

cov2.data <- subset(all.data, Target == "COV2")
cov2.data$Amp.Status <- factor(cov2.data$Amp.Score, levels = c("Amp", "Inconclusive", "No Amp"))

this.plot = ggplot(cov2.data, aes(x=Sample, 
                                        y=Amp.Score)) + 
  geom_bar() +
  scale_y_continuous(limits = c(0, 2)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  xlab("Samples")+
  ylab("Amp.Score")+
  labs(title = "Amp Score Distribution")+
  NULL
this.plot
