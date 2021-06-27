library(clusterProfiler)
library(enrichplot)
library(ggplot2)
organism = "org.Hs.eg.db"
BiocManager::install(organism, character.only = TRUE)
library(organism, character.only = TRUE)
setwd("/Users/yalegenomecenter/Desktop/Final")
df = read.csv("res_N_O_v_Y.csv", header=TRUE)
original_gene_list <- df$stats
names(original_gene_list) <- df$Gene_ID
gene_list<-na.omit(original_gene_list)
gene_list = sort(gene_list, decreasing = TRUE)
ids<-bitr(names(original_gene_list), fromType = "ENSEMBL", toType = "ENTREZID", OrgDb=organism)
dedup_ids = ids[!duplicated(ids[c("ENSEMBL")]),]
df2 = df[df$Gene_ID %in% dedup_ids$ENSEMBL,]
df2$Y = dedup_ids$ENTREZID
kegg_gene_list <- df2$stats
names(kegg_gene_list) <- df2$Y
kegg_gene_list<-na.omit(kegg_gene_list)
kegg_gene_list = sort(kegg_gene_list, decreasing = TRUE)
kegg_organism = "hsa"
kk2 <- gseKEGG(geneList     = kegg_gene_list,
               organism     = kegg_organism,
               nPerm        = 10000,
               minGSSize    = 3,
               maxGSSize    = 800,
               pvalueCutoff = 1,
               pAdjustMethod = "none",
               keyType       = "ncbi-geneid")
dotplot(kk2, showCategory = 10, title = "Enriched Pathways" , split=".sign") + facet_grid(.~.sign)
the_plot <- cnetplot(kk2, categorySize="pvalue", foldChange=gene_list)
ridgeplot(kk2) + labs(x = "enrichment distribution")
gseaplot(kk2, by = "all", title = kk2$Description[1], geneSetID = 1)
write.csv(kk2, file = "/Users/yalegenomecenter/Desktop/Pathway-List.csv")

