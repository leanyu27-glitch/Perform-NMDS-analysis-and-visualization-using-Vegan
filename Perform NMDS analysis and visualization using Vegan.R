#### NMDS
library(vegan)
otu <- read.csv("", head = T, fileEncoding = "GBK",row.names = 1)
otu <- data.frame(t(otu))
bray_dis <- vegdist(otu, method = "bray") # dist 

###
#dis <- read.csv("距离矩阵.csv", row.names = 1, header = T)
#bray_dis <- as.dist(dis)

# NMDS  ?metaMDS
nmds_dis <- metaMDS(bray_dis, k = 2)

nmds_dis$stress # < 0.3

# score
nmds_dis_site <- data.frame(nmds_dis$points)

#  ?wascores
# nmds_dis_species <- wascores(nmds_dis$points, otu)

library(ggplot2)
library(tidyverse)
library(ggpubr)

# input group
group <- read.csv("", row.names = 1, head = T, fileEncoding = "GBK")
nmds_dis_site <- cbind(nmds_dis_site, group)
str(nmds_dis_site)
p <- ggplot(nmds_dis_site, aes(MDS1, MDS2, color = group, shape = group)) +
  geom_point(size = 4) +
  # stat_ellipse(type = "t", linetype = 2, size = 1) +
  geom_vline(xintercept = 0, lty = "dashed") +
  geom_hline(yintercept = 0, lty = "dashed") +
  # stat_ellipse(geom = "polygon",level=0.95,linetype = 2,size=0.5,aes(color=group$Group),alpha=0.2,show.legend = T)+
  # stat_chull(geom = "polygon", aes(group = group, color = group, fill = group), alpha = 0.1) +
  labs(x = "NMDS1", y = "NMDS2") +
  annotate("text", label = paste("Stress =", round(nmds_dis$stress, 3)), x = .25, y = .28, size = 6, colour = "black") +
  annotate("text", label = paste("p = 0.03"), x = .25, y = .4, size = 6, colour = "black") +
  theme_bw() +
  theme(
    text = element_text(size = 19), panel.grid = element_blank(),
    axis.text = element_text(colour = "black"), 
    axis.title = element_text(colour = "black"),
    legend.key = element_blank(),  legend.background = element_rect(fill = "transparent"),
    axis.line = element_line(color="black"),
    strip.placement = "outside",strip.background = element_blank()
  )
p
anosim_result <- anosim(otu, nmds_dis_site$group, distance = "bray", permutations = 999)
anosim_result
library(eoffice)
topptx(filename = "nmds.pptx", height = 5, width = 6)

getwd()
ggsave(plot = p, "", height = 4.5, width = 7, dpi = 600)
