## orthoI = NA时，执行OPLS，并通过交叉验证自动计算适合的正交组分数
#R2X和R2Y分别表示所建模型对X和Y矩阵的解释率，Q2表示模型的预测能力，它们的值越接近于1表明模型的拟合度越好

rm(list = ls())


# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ropls")


library(ropls)
library(tidyverse)
library(ggalt)


####################### pre ######
getScoreMN <- function(dataFile, groupFile, grp_col = 2) {
  grpInfo <- read.table(groupFile,header=T,sep='\t');
  FileName <- strsplit(basename(dataFile),'.',fixed=T)[[1]];
  SamID <- FileName[1];
  Data <- read.table(dataFile,header=T,row.names = 1);
  Data.tm <- t(as.matrix(Data)); 
  groupname <- c();
  
  for(i in 1:length(colnames(Data)))
  {
    groupname <- append(groupname,as.character(grpInfo[grpInfo[,1] == colnames(Data)[i],grp_col]));
  }
  
  Groups <- levels(as.factor(groupname));
  
  Data.oplsda <- try(opls(Data.tm ,groupname, predI =1, orthoI = NA),silent=TRUE);
  if ('try-error' %in% class(Data.oplsda))
  {
    Data.oplsda <- opls(Data.tm ,groupname, predI =1, orthoI = 2);
  } 
  
  scoreMN <- as.data.frame(
    cbind(Data.oplsda@scoreMN[,1],
          Data.oplsda@orthoScoreMN[,1])
  )
  scoreMN$name <- rownames(scoreMN)
  scoreMN <- full_join(scoreMN, grpInfo) %>% 
    select(-name) 
  colnames(scoreMN) <- c("h1", paste0("o", 1), "SampleType")
  scoreMN$SampleType <- as.factor(scoreMN$SampleType)
  scoreMN$R2X <- paste0(
    "t1 (", 
    format(Data.oplsda@modelDF[1, "R2X"] * 100,
           digits = 1, nsmall = 2), 
    "%)"
  )
  return(scoreMN)
}

mi=c("#1B9E77" ,"#DC143C", "#7570B3","#E7298A",	"#ADFF2F","#FFD700","#D95F02", "grey")

####################### Control-DSS ######
myCol <- mi[c(1, 2)]
myScore <- getScoreMN("./Fcontrol_DSS.data.txt", "./Fcontrol_DSS-grouping.txt")
myScore$Type <- "Control vs DSS"
scoreMN <- myScore


p <- ggplot(scoreMN, 
             aes(
               h1, 
               o1,
               color = SampleType
             )
) + 
  geom_point( size = 2, alpha = 0.7) +
  stat_ellipse(type = "t", linetype = 2, show.legend = FALSE)+ #linetype ='solid'
  geom_encircle(s_shape=1, expand=0, alpha = 0.7) +
  labs(
    x = paste0(as.character(unique(scoreMN$R2X))),
    y = "to1",
    title = as.character(unique(scoreMN$Type))
  ) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = myCol) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.16,0.85)
  ) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank())+
  geom_hline(yintercept = 0, linetype = 2, size = 0.4, alpha = 0.6) + 
  geom_vline(xintercept = 0, linetype = 2, size = 0.4, alpha = 0.6) 

p
p1 <- p + annotate("text", x = c(-50, -35,-20), y = c(-80, -80,-80),
                   label = c("R2X\n0.427", "R2Y\n0.994","Q2\n0.924"),size=2 )

p1


####################### CONTROL-PG ######
myCol <- mi[c(1, 3)]
myScore <- getScoreMN("./Fcontrol_PG.data.txt", "./Fcontrol_PG-grouping.txt")
myScore$Type <- "Control vs DSS+Pg"
scoreMN <- myScore


p <- ggplot(scoreMN, 
            aes(
              h1, 
              o1,
              color = SampleType
            )
) + 
  geom_point( size = 2, alpha = 0.7) +
  stat_ellipse(type = "t", linetype = 2, show.legend = FALSE)+ #linetype ='solid'
  geom_encircle(s_shape=1, expand=0, alpha = 0.7) +
  labs(
    x = paste0(as.character(unique(scoreMN$R2X))),
    y = "to1",
    title = as.character(unique(scoreMN$Type))
  ) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = myCol) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.22,0.85)
  ) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank())+
  geom_hline(yintercept = 0, linetype = 2, size = 0.4, alpha = 0.6) + 
  geom_vline(xintercept = 0, linetype = 2, size = 0.4, alpha = 0.6) 

p
p2 <- p + annotate("text", x = c(-48, -33,-18), y = c(-86, -86,-86),
                   label = c("R2X\n0.43", "R2Y\n0.99","Q2\n0.91"),size=2 )

p2

####################### DSS-PG ######
myCol <- mi[c(2, 3)]
myScore <- getScoreMN("./FDSS_PG.data.txt", "./FDSS_PG-grouping.txt")
myScore$Type <- "DSS vs DSS+Pg"
scoreMN <- myScore


p <- ggplot(scoreMN, 
            aes(
              h1, 
              o1,
              color = SampleType
            )
) + 
  geom_point( size = 2, alpha = 0.7) +
  stat_ellipse(type = "t", linetype = 2, show.legend = FALSE)+ #linetype ='solid'
  geom_encircle(s_shape=1, expand=0, alpha = 0.7) +
  labs(
    x = paste0(as.character(unique(scoreMN$R2X))),
    y = "to1",
    title = as.character(unique(scoreMN$Type))
  ) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = myCol) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.6,0.85)
  ) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank())+
  geom_hline(yintercept = 0, linetype = 2, size = 0.4, alpha = 0.6) + 
  geom_vline(xintercept = 0, linetype = 2, size = 0.4, alpha = 0.6) 

p
p3 <- p + annotate("text", x = c(-31, -21,-11), y = c(-84, -84,-84),
                   label = c("R2X\n0.423", "R2Y\n0.996","Q2\n0.819"),size=2 )

p3



library(patchwork)
p = p1 + p2 + p3



pdf("figS5.pdf", width = 10, height = 3)
p
dev.off()









