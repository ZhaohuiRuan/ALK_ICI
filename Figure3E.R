library(tidyverse)
library(Seurat)
load('plot_data_Figure3E.RData')
sample_anno <- data.frame(sample = names(clusters),
                          immu_group = clusters,stringsAsFactors = F)
sample_anno$immu_group <- plyr::mapvalues(sample_anno$immu_group %>% as.character(),
                                          from = c("1", "2"),
                                          to = c("Hot", "Cold"))
sample_anno$alk_group <- plyr::mapvalues(sample_anno$sample %>% as.character(),
                                         from = anno_col %>% rownames(),
                                         to = anno_col$Group)


fisher_pvalue <- table(sample_anno$alk_group,sample_anno$immu_group) %>% fisher.test()

plot_dat <- sample_anno %>% rename(group = alk_group,group2 = immu_group)

plot_dat_sum <- plot_dat %>% group_by(group,group2) %>% summarise(num = n())
plot_dat_sum$perc <- with(plot_dat_sum, num/ ave(num, group, FUN = sum))

plot_dat_sum$group <- plyr::mapvalues(plot_dat_sum$group %>% as.character(),
                                      from = c("ALK5", "ALK3"),
                                      to = c("3’-ALK\nRetain 5’-ALK", "3’-ALK"))

pdf('Figure3E.pdf',height = 3.5,width = 1.8)
ggplot(data = plot_dat_sum,aes(x = group,group = group2,y = perc)
)+
  geom_bar(aes(fill = group2,color = group2),stat = "identity",
           position = "fill"
  )+
  geom_text(aes(label =  scales::percent(round(perc,3))),
            position = position_fill(vjust = 0.5))+
  labs(x = '',y = 'Percentage',subtitle = str_glue('p = {signif(fisher_pvalue$p.value,4)}'))+
  theme_classic()+
  theme(axis.text.y = element_text(color = 'black'),
        axis.text.x = element_text(color = 'black',angle = 45,hjust = 1,vjust = 1),
        plot.title = element_text(hjust = 0.5)
  )+
  scale_y_continuous(labels = scales::percent_format())+
  scale_color_manual(values = c('#cc9182','#f0ecf5') %>% rev)+
  scale_fill_manual(values = c('#cc9182','#f0ecf5') %>% rev)+NoLegend()
dev.off()
