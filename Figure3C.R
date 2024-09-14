library(tidyverse)
library(Seurat)

load('plot_data_Figure3C.RData')
dat$Group <- plyr::mapvalues(dat$Group %>% as.character(),
                             from = c("ALK5", "ALK3"),
                             to = c("3’-ALK\nRetain 5’-ALK", "3’-ALK"))
library(ggsignif)

pdf('Figure3C.pdf',height = 4,width = 2)
ggplot(data = dat,aes(x = Group,y = Immune_score))+
  geom_boxplot(aes(fill = Group),outlier.size = 0)+
  geom_jitter(alpha = 0.2)+
  theme_classic()+
  theme(axis.text.y = element_text(color = 'black'),
        axis.text.x = element_text(color = 'black',angle = 45,hjust = 1,vjust = 1))+
  labs(x = '',y = 'Immune score')+
  scale_fill_manual(values = c('#dfe9f2','#80529c'))+
  geom_signif(comparisons = list(c("3’-ALK\nRetain 5’-ALK", "3’-ALK")),step_increase = 0.1,
              map_signif_level = F,test = wilcox.test,tip_length = 0)+NoLegend()
dev.off()
