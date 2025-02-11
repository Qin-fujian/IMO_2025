rm(list = ls())

library(pheatmap)
library(circlize)

dat <- read.csv("./heatmap.csv",header = T,row.names = 1)

grp <- read.table("./Fall-grouping.txt",header = T,sep = '\t')
name <- grp$name
grp <- as.data.frame(grp[,2])
colnames(grp) <- "group"
rownames(grp) <- name


mycol <- colorRampPalette(c("DarkSlateGray","white","red"))(100)
ann_colors = list(group = c(Control = "#1B9E77", DSS = "#DC143C",'DSS+Pg' = "#7570B3"))


pheatmap(as.matrix(dat),
         scale = "row",
         cellwidth = 10,cellheight = 10,
         #cluster_rows = F,
         show_colnames = F,
         #legend_breaks=c(0,0.5,1), # 设置图例的范围
         fontsize_row = 10, # 分别设置行列标签字体大小
         fontsize_col = 10,
         cutree_rows=2,
         cutree_cols=3,
         gaps_col = c(7, 15),
         cluster_cols = F,
         border_color ="black",
         color = mycol,
         annotation_col = grp,
         annotation_colors = ann_colors,
         show_rownames=T,filename = "figB.pdf")







