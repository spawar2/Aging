setwd("/Users/yalegenomecenter/Desktop/")
data <- read.csv("Pathway-HeatMap.csv", header=TRUE)

library(dplyr)
library(data.table)

New <- setDT(data)[, lapply(.SD, na.omit), by = Description]

write.csv(New, file = "Intermediate.csv", row.names = TRUE)

data2 <- read.csv("Intermediate-2.csv", header=TRUE)

New2 <- setDT(data2)[, lapply(.SD, na.omit), by = Description]

samp2 <- New2[,-1]
New2 <- as.data.frame(New2)
samp2 <- New2[,-1]
rownames(samp2) <- New2[,1]

samp2_P <- samp2[c(-1,-8,-2,-4, -6)]
samp2_P <- samp2_P[,c(4,1,2,3)]
samp2_P[is.na(samp2_P)] <- 1

#########
samp2_NES <- samp2[c(-8,-2,-4, -6)]
samp2_NES <- samp2_NES[,c(4,1,2,3)]
samp2_NES[is.na(samp2_NES)] <- 0

samp2_P <- samp2[c(-1,-3,-5,-7)]
samp2_P <- samp2_P[,c(4,1,2,3)]
df3 <- as.matrix(samp2_P)
#########

df2 <- data.frame(sapply(samp2_NES, function(x) as.numeric(as.character(x))))
rownames(df2) <- New2[,1]
df2 <- as.matrix(df2)

df3 <- data.frame(sapply(samp2_P, function(x) as.numeric(as.character(x))))
rownames(df3) <- New2[,1]
df3 <- as.matrix(df3)
df3 <- round(df3, digits = 4)

#should be one more than col vector (40), white centered around 0
pairs.breaks <- seq(-2, 2, length.out=41)
heatmap.2(df2, hclustfun=function(xx)hclust(xx, method="ward.D2"), dendrogram="row", Colv=FALSE,  breaks=pairs.breaks,  cellnote= df3, notecol="black",notecex=0.5, trace="none",density.info="none",col=colorpanel(40,  "darkblue","white",  "darkred"), colsep=c(7,12,20,27,31),  sepcolor="lightgrey",margins = c(8, 20), lhei=c(1,4), lwid=c(1,3), cexRow=0.25, cexCol=0.5)

#########################################################################

data3 <- read.csv("TF-Heatmap copy.csv", header=TRUE)
samp3 <- data3[,-1]
rownames(samp3) <- data3[,1]
samp3 <- samp3[c(-2, -4, -6, -8)]
samp3 <- samp3[,c(4,1,2,3)]
samp3 <- as.matrix(samp3)

note <- data3[,-1]
rownames(note) <- data3[,1]
note <- note[c(-1, -3, -5, -7)]
note <- note[,c(4,1,2,3)]
note <- as.matrix(note)

pairs.breaks <- seq(-2, 2, length.out=41)
heatmap.2(samp3, hclustfun=function(xx)hclust(xx, method="ward.D2"), dendrogram="row", Colv=FALSE,  breaks=pairs.breaks,  cellnote= note, notecol="black",notecex=0.5, trace="none",density.info="none",col=colorpanel(40,  "darkblue","white",  "darkred"), colsep=c(7,12,20, 27, 31),  sepcolor="lightgrey",margins = c(8, 20), lhei=c(1,4), lwid=c(1,3), cexRow=0.25, cexCol=0.5)

#########################################################################

data4 <- read.csv("TF-starplot copy.csv", header=TRUE)
samp4 <- data4[,-1]
rownames(samp4) <- data4[,1]

Map <- samp4[,c(1:4)]
Map <- Map[,c(4,1,2,3)]
Cell <- samp4[,c(7:10)]

Map <- as.matrix(Map)
Cell <- as.matrix(Cell)
Cell[Cell == 0] <- NA

pairs.breaks <- seq(-0.28, 0.28, length.out=41)
heatmap.2(Map, hclustfun=function(xx)hclust(xx, method="ward.D2"), dendrogram="none", Colv=FALSE,  breaks=pairs.breaks, cellnote= Cell, notecol="black",notecex=5, trace="none",density.info="none",col=colorpanel(40,  "white","white",  "white"), colsep=c(1,2,3,4),  sepcolor="white",margins = c(10, 10), lhei=c(1,4), lwid=c(1,3), cexRow=0.5, cexCol=0.5)

#########################################################################














