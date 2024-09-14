library(tidyverse)
library(Seurat)
load('plot_data_Figure3G.RData')
plot_dat$alk_group <- plyr::mapvalues(plot_dat$alk_group %>% as.character(),
                                      from = c("ALK5", "ALK3"),
                                      to = c("3’-ALK\nRetain 5’-ALK", "3’-ALK"))
library(ggsignif)
pdf('Figure3G.pdf',height = 4,width = 1.4)
ggplot(data = plot_dat,aes(x = alk_group,y = preBcells))+
  geom_boxplot(aes(fill = alk_group),outlier.size = 0)+
  geom_jitter(alpha = 0.2)+
  theme_classic()+
  theme(axis.text.y = element_text(color = 'black'),
        axis.text.x = element_text(color = 'black',angle = 45,hjust = 1,vjust = 1))+
  labs(x = '',y = 'pre B cells')+
  scale_fill_manual(values = c('#dfe9f2','#80529c'))+
  # scale_y_continuous(expand = c(-0.1,0.1))+
  geom_signif(comparisons = list(c("3’-ALK\nRetain 5’-ALK", "3’-ALK")),step_increase = 0.1,
              map_signif_level = F,test = wilcox.test,tip_length = 0)+NoLegend()
dev.off()
