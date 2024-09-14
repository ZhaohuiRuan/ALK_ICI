library(tidyverse)
library(Seurat)
library(openxlsx)

##########--------------Figure 3B
load('plot_data_Figure3B.RData')
dat <- dat %>% filter(p.adjust<0.05)
dat <- dat %>% arrange(desc(NES))
dat$ID <- str_replace_all(dat$ID,'HALLMARK_','')
dat$ID <- str_replace_all(dat$ID,'_',' ')
dat$ID <- str_to_sentence(dat$ID)
dat$ID <- plyr::mapvalues(dat$ID,
                          from = c("Il6 jak stat3 signaling", "Epithelial mesenchymal transition", 
                                   "Allograft rejection", "Tnfa signaling via nfkb", "Complement", 
                                   "Il2 stat5 signaling", "Coagulation", "Inflammatory response", 
                                   "Interferon gamma response", "Adipogenesis", "Oxidative phosphorylation", 
                                   "Fatty acid metabolism", "E2f targets", "G2m checkpoint", "Myc targets v1"
                          ),
                          to = c("IL6 JAK STAT3 signaling", "Epithelial mesenchymal transition", 
                                 "Allograft rejection", "TNFA signaling via NFKB", "Complement", 
                                 "IL2 STAT5 signaling", "Coagulation", "Inflammatory response", 
                                 "Interferon gamma response", "Adipogenesis", "Oxidative phosphorylation", 
                                 "Fatty acid metabolism", "E2F targets", "G2M checkpoint", "MYC targets v1"
                          ))
dat$ID <- factor(dat$ID %>% as.character(),levels = dat$ID %>% as.character() %>% rev)
dat$group <- ifelse(dat$NES>0,"5'-ALK","3'-ALK")
pdf('Figure3B.pdf',height = 3.5,width = 5.5)
ggplot(dat, aes(x = ID, y = NES, fill = group)) +
  geom_col() +  geom_hline(yintercept = 0)+
  coord_flip() +  # This will flip the axes so the pathways are on the y-axis
  scale_fill_manual(values = c("3'-ALK" = "#dfe9f2", "5'-ALK" = "#a86fa2")) +
  labs(y = "NES", x = "Hallmark pathways") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 8,color= 'black'),
        axis.text.x = element_text(color = 'black'),axis.line.y = element_blank())
dev.off()
