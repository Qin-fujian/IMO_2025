rm(list = ls())
library(tidyverse)
library(RColorBrewer)
require(ggpubr)
library(paletteer)
library(patchwork)
library(ggsignif) #默认为wilcox test
library(ggsci)
library(ggprism)
library(rstatix)


mycolors <- c("#979797","#D1F6D1","#FE8E53","#A380AA")
#mycolor <- brewer.pal(8,"Set1")[2:1]


#1.Fibre-free diet-----
files <- Sys.glob("CD_*csv")
num <- length(files)
plots <- list()

for (i in 1:num) {
  
  data1 <- read.csv(files[[i]],header = T,check.names = F)
  colnames(data1)[1] <- "Group"
  data1$Group <- factor(data1$Group,levels = c("Control","DSS","P.goldsteinii"),
                        labels = c("Control","DSS","DSS+Pg"))
  
  tt <- strsplit(files[[i]],"_")[[1]][2]
  title <- strsplit(tt,".",fixed = T)[[1]][1]
  #dat1 <- data.frame(table(data1$Group))
  #colnames(dat1)[1] <- "Group" #
  #dat1$label <- paste(dat1$Group,"\n ( n = ",dat1$Freq,")")
  ylim <- max(data1$Concentration)
  
  # if (i %in% c(1,5,9)) {
  #   ytitle = "Concentration(mg/g)"
  # } else {
  #   ytitle = " "
  # }
  
  
  plots[[i]] <- ggplot(data1,aes(x =Group ,y=Concentration,fill = Group))+
    stat_summary(fun = mean,geom = 'col',
                 position = position_dodge2(),
                 width = 0.8) +
    
    stat_summary(fun.data = 'mean_se',geom = 'errorbar',
                 color = 'black',
                 position = position_dodge(width = 0.9),
                 width = 0.3,size = 0.6) +
    
    scale_fill_manual(values=mycolors)+
    
    # 添加抖动点
    geom_point(aes(color = "black"),
               # 不显示图例
               show.legend = F,
               position = position_jitterdodge(seed = 123,
                                               jitter.width = 0.6,
                                               dodge.width = 0.9),
               # 形状
               shape = 21,
               # 填充颜色
               fill = "white",
               # 大小
               size = 2) +
    # 抖动点边框颜色
    scale_color_manual(values = mycolors) +
    
    labs(y="Concentration(μg/g)",x="",title = title)+
    #scale_x_discrete(labels=dat1$label)+ #自定义X轴标签
    ylim(0,ylim*1.5)+
    theme_bw(base_size = 10)+
    theme(panel.grid = element_blank(),legend.position="none",plot.title = element_text(hjust = 0.5,face = "bold"),
          axis.text.x  = element_text(size=12,color ="black",angle = 45,hjust = 1),
          axis.text.y  = element_text(size=12,color ="black"),
          axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))+
    geom_signif(aes(Group,Concentration),
                y_position = c(ylim*1.05, ylim*1.20, ylim*1.35),
                #annotations = c("hello", "world","nihao"),
                textsize = 3,
                tip_length = 0,
                test="wilcox.test",
                map_signif_level = TRUE,## 转化为星号
                comparisons =list(c("Control", "DSS"),
                                  c("Control", "DSS+Pg"),
                                  c("DSS","DSS+Pg")))
  
}




p <- plots[[6]] + plots[[7]] + plots[[9]] + plots[[8]] +plots[[4]] +
  plots[[1]] + plots[[2]] + plots[[12]] + plots[[11]] + plots[[10]] +
  plots[[3]] +  plots[[5]] +
  plot_layout(nrow = 3) +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 16))


ggsave("figS7.pdf",p,width = 12,height = 12)


