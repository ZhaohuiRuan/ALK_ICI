library(tidyverse)
library(openxlsx)
library(Seurat)
load('plot_data_Figure3D.RData')
dat <- dat[c("Activated B cell","Immature  B cell", "Memory B cell", 
             "Activated CD8 T cell",  "Central memory CD8 T cell", "Effector memeory CD8 T cell", 
             "Activated CD4 T cell", "Central memory CD4 T cell", "Effector memeory CD4 T cell", "Regulatory T cell", "T follicular helper cell",
             "Type 1 T helper cell", "Type 17 T helper cell", "Type 2 T helper cell",
             "Natural killer T cell",
             "Natural killer cell", "CD56bright natural killer cell",  "CD56dim natural killer cell",
             "Eosinophil", "Gamma delta T cell",  
             "Macrophage", "Mast cell","Monocyte",  "MDSC","Activated dendritic cell", "Immature dendritic cell", "Plasmacytoid dendritic cell",
             "Neutrophil"
),]


library(pheatmap)

dist_mat <- dist(t(dat), method = "canberra")
hclust_res <- hclust(dist_mat, method = "ward.D2")
clusters <- cutree(hclust_res, k = 2)
my_colors <- colorRampPalette(c("#1f2a65",'#856bad',"white","#c07a6a", "#7f191e"))(n = 100)  # 'n' 表示生成多少种颜色

annotation_colors <- list(
  Group = c(ALK3 = "#dfe9f2", ALK5 = "#80529c")  # 为每个组分配颜色
)

pdf('Figure3D.pdf',height = 4,width = 4.5)
pheatmap(dat,cluster_rows = F,cluster_cols = hclust_res,annotation_colors = annotation_colors,
         cutree_cols = 2,
         # clustering_distance_cols = "manhattan",
         scale = 'row',show_colnames = F,annotation_col = anno_col,color = my_colors,border_color = NA
)
dev.off()
