
## Problems: current Results files have no Amp Score cols. Need to edit export file settings
##  on instruments and adjust Brian's code to accept the additional column when processing MPLEX files.

library(tidyverse)
library(plotly)
library(sicegar)

#select files
setwd("C:/Users/srlgam/Desktop/Projects/MPLX-results")
fileList <- list.files(pattern = "^MPLEX(.*)_Result(.*)txt$")
fileList2 <- list.files(pattern = "^MPLEX(.*)_Multicomponent(.*)txt$")
fileList2
fileList

#merge into data frame of amp.scores
all.data = data.frame()
this.file2 = data.frame()
for(i in fileList){
  for(k in fileList2){
    this.file <- read.table(file = i, header = TRUE, sep = "\t")
    that.file <- read.table(file = k, header = TRUE, sep = "\t")
    this.file2 <- full_join(this.file, that.file, by = "Well.Position")
    all.data <- rbind(all.data, this.file2)
    print("Files Merged")
  }
}


#tidy data
cov2.data <- subset(all.data, Target == "COV2" & Amp.Score != 0)
#cov2.data$Sample <- sub('[.]', '_', make.names(cov2.data$Sample, unique=TRUE))
cov2.data$Amp.Status <- factor(cov2.data$Amp.Status)

#plot data
this.plot = ggplot(cov2.data, aes(x=reorder(Sample, Amp.Score), y = Amp.Score, fill = Amp.Status)) + 
  geom_bar(stat = "identity", position = position_dodge())+
  scale_y_continuous("Amp.Score", limits = c(0, max(cov2.data$Amp.Score))) +
  scale_x_discrete("Samples")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  labs(title = "Amp Score Distribution")+
  NULL
this.plot

#save output
ggsave("cov2-amp-score-dist.pdf")
this.plotly <- ggplotly(this.plot)
htmlwidgets::saveWidget(as_widget(this.plotly), "this.plotly.html")
this.plotly

#stat data
incon.cov2.data <- subset(cov2.data, Amp.Status == "Inconclusive")
pos.cov2.data <- subset(cov2.data, Amp.Status == "Amp")
neg.cov2.data <- subset(cov2.data, Amp.Status == "No Amp")

