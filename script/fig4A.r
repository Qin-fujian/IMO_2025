rm(list = ls())

dataFile = "./Fall.KWfdr.Sig.spearman.VIP.txt";
groupFile = "./Fall-grouping.txt";
outDir <- "./";
grp_col <- 2;

library(ade4);
library(fpc);
library(RColorBrewer)
library(ggplot2)
library(ggalt)
library(vegan)

grpInfo <- read.table(groupFile,header=T);

FileName <- strsplit(basename(dataFile),'.',fixed=T)[[1]];
SamID <- FileName[1];
MetID <- FileName[2];
LID <- FileName[3];
DID <- FileName[4];

Beta <- as.matrix(data.frame(read.table(dataFile,header=T,row.names = 1,sep='\t')));

Beta <- Beta[order(rownames(Beta),decreasing=F),];
Beta <- Beta[,order(colnames(Beta),decreasing=F)];

groupname <- c();
for(i in 1:length(rownames(Beta)))
{
  groupname <- append(groupname,as.character(grpInfo[grpInfo[,1] == rownames(Beta)[i],grp_col]));
}

rownames(Beta) <- groupname;
colnames(Beta) <- groupname;

Groups <- as.character(levels(as.factor(grpInfo[,grp_col])))
Beta.Group <- c();
Symbol.Group <- c();
Color.Group <- c();

for(i in 1:length(Groups))
{
  Beta.Group[[i]] <- Beta[grep(Groups[i],rownames(Beta)),grep(Groups[i],colnames(Beta))];
  Symbol.Group <- append(Symbol.Group,as.vector(rep((17+i),nrow(Beta.Group[[i]]))));
  Color.Group <- append(Color.Group,as.vector(rep(brewer.pal(8,"Set1")[i],nrow(Beta.Group[[i]]))));
}

pc <- princomp(Beta);
#################
plotdata <- as.data.frame(pc$loadings[,1:2])
plotdata$SampleType <- rownames(pc$loadings[,1:2])
colnames(plotdata)[1:2] <- c("comp1", "comp2")

eig <- pc$sde

#显著性检测
set.seed(1)
dune <- read.table("./Fall.data.txt",header=T, row.names=1)
dune <- as.data.frame(t(dune))

dune.div <- adonis2(dune ~ group, #离子丰度表 
                    data = grpInfo, #分组
                    permutations = 999, #设置置换次数
                    method="bray")

dune_adonis <- paste0("adonis R2: ",round(dune.div$R2,2), "; p: ", dune.div$`Pr(>F)`)

library(ggalt)
library(ggplot2)
p = ggplot(plotdata, 
           aes(comp1, comp2,group = SampleType,color = SampleType)) + 
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(type = "t", linetype = 2)+
  geom_encircle(s_shape=1, expand=0, alpha = 0.7) +
  labs(x = paste("PC1(",sprintf("%.1f",eig[1]^2/sum(eig^2)*100),"%)",sep =""),
       y = paste("PC2(",sprintf("%.1f",eig[2]^2/sum(eig^2)*100),"%)",sep=""),
       title = dune_adonis)

mi=c("#1B9E77" , "#DC143C","#7570B3","#E7298A",	"#ADFF2F","#FFD700","#D95F02", "grey")
p1 = p +
  theme_bw(base_size = 15) +
  scale_colour_manual(values = mi)+
  theme(plot.title = element_text(hjust = 0.5),panel.grid = element_blank(),legend.position = c(.87, .8)) +
  #消除图例的背景填充
  theme(legend.background = element_blank()) +
  #消除图例中的项目的周围填充
  theme(legend.key = element_blank())+
  guides(color = guide_legend(title = NULL),shape = guide_legend(title = NULL)) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.4, alpha = 0.6) + 
  geom_vline(xintercept = 0, linetype = 2, size = 0.4, alpha = 0.6)


p1

ggsave("Fig4A.pdf",p1,width = 4.8, height = 4.8)

