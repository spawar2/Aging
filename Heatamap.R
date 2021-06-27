setwd("/Users/yalegenomecenter/Desktop")
data <- read.csv("heatmap.csv", header=TRUE)
library("ggplot2")
mine.heatmap <- ggplot(data = data, mapping = aes(x = Gene.ID,
                                                       y = Class,
                                                       fill = Fold.Change..Log2.)) +
  geom_tile() +
  xlab(label = "Transcription Factors") +
scale_fill_gradient(name = "Fold Change (Log 2)",
low="blue", high="red") +
  theme(strip.placement = "outside") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5), axis.text=element_text(size=5), panel.background = element_rect(fill = 'light green', colour = 'red')) 


mine.heatmap
