library(tidyverse)
library(Seurat)


load('plot_data_Figure3A.RData')

immune_genes <- c("CCL2", "IL1B", "GZMA", "MS4A4A", "GNLY", "TWSG1", "IL6", "CSF3R", "BATF")
genes <- c("TNF", "TNFSF4", "MICB", "CD28", "IL7", "CD80", "CD274", "TIGIT", 
           "PDCD1LG2", "IL1B", "IL2RA", "HLA-DQB2", "GZMA", "IFNG", "HLA-DQB1")
gene_used <- c(immune_genes,'CD274','PDCD1') %>% unique()

dat$label <- ifelse(dat$SYMBOL%in% gene_used,dat$SYMBOL,NA)

dat$Significant <- ifelse(is.na(dat$label),"Not Labelled","Labelled")

max_value <- abs(dat$log2FC) %>% max
library(ggrepel)
pdf('Figure3A.pdf',height = 4,width = 5)
ggplot(dat, aes(x = log2FC, y = -log10(PValue))) +
  geom_point(aes(color = Significant,alpha = Significant),color = "grey50",alpha = 0.1) +
  geom_point(data = filter(dat, !is.na(label)),color = "#c88775") +
  # scale_color_manual(values = c("Not Labelled" = "grey50", "Labelled" = "#c88775")) +
  # scale_shape_manual(values = c("Not Labelled" = 16, "Labelled" = 17)) +
  # scale_alpha_manual(values = c("Not Labelled" = 0.1, "Labelled" =1))+
  geom_text_repel(
    data = filter(dat, !is.na(label)),
    aes(label = SYMBOL,color = Significant),
    box.padding = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50',
    fontface = "italic"
  ) +
  theme_classic() +
  labs(x = "log2(Fold change)", y = "-log10(p-value)",
       title = "") +
  xlim(-max_value,max_value)+
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.text = element_text(color = 'black'))
dev.off()
