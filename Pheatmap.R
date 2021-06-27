#Pheatmaps, 06/27/2021

setwd("/Users/yalegenomecenter/Desktop/June-27-Revision/")
dataC <- read.csv("res_CM_O_v_Y-Pathway-List.csv", header=TRUE)
dataH <- read.csv("res_H_O_v_Y-Pathway-List.csv", header=TRUE)
dataL <- read.csv("res_L_O_v_Y-Pathway-List.csv", header=TRUE)
dataN <- read.csv("res_N_O_v_Y-Pathway-List.csv", header=TRUE)

dataC.1 <- dataC[ which(dataC$qvalues < 0.1),]
dataH.1 <- dataH[ which(dataH$qvalues < 0.1),]
dataL.1 <- dataL[ which(dataL$qvalues < 0.1),]
dataN.1 <- dataN[ which(dataN$qvalues < 0.1),]

dataC.1.2 <- dataC.1[,c(3,6)]
dataH.1.2 <- dataH.1[,c(3,6)]
dataL.1.2 <- dataL.1[,c(3,6)]
dataN.1.2 <- dataN.1[,c(3,6)]

dataH.C <- merge(dataH.1.2, dataC.1.2, "Description", all=TRUE)
dataH.C.L <- merge(dataH.C, dataL.1.2, "Description", all=TRUE)
dataH.C.L <- setNames(dataH.C.L, c("Description","H","C", "L"))
dataH.C.L.N <- merge(dataH.C.L, dataN.1.2, "Description", all=TRUE)
dataH.C.L.N <- setNames(dataH.C.L.N, c("Description","H","C", "L", "N"))
dataH.C.L.N[is.na(dataH.C.L.N)] <- 0 

Final <- dataH.C.L.N[,-1]
rownames(Final) <- dataH.C.L.N[,1]

library(pheatmap)
pdf("NES-pheatmap.pdf", width=5, height=12)
  out <- pheatmap(Final, 
      show_rownames=T,
      cluster_cols=T,
      cluster_rows=T,
      scale="row",
      clustering_distance_rows="euclidean",
      clustering_distance_cols="euclidean",
      clustering_method="complete",
      border_color=FALSE,
      cex=0.7)
dev.off()

TF.Logfold <- read.csv("TF-Heatmap copy.csv", header=TRUE)
Final2 <- TF.Logfold[,-1]
rownames(Final2) <- TF.Logfold[,1]

pdf("TF-pheatmap.pdf", width=5, height=12)
  out <- pheatmap(Final2, 
      show_rownames=T,
      cluster_cols=T,
      cluster_rows=T,
      scale="row",
      clustering_distance_rows="euclidean",
      clustering_distance_cols="euclidean",
      clustering_method="complete",
      border_color=FALSE,
      cex=0.7)
dev.off()







