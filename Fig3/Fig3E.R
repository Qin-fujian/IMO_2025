rm(list = ls())

library(tidyverse)#数据操作
library(ggplot2)#数据可视化


###绘图数据
AOM <- c(4,22,1)
PG <- c(8,10,0)

data <- data.frame(AOM,PG)
data <- t(data)
colnames(data) <- c("Normal","Adenoma","Adenocarcinoma")
row.names(data) <- c("AOM/DSS","AOM/DSS+Pg")
data
class(data)
data <- as.data.frame(data)
data$x <- rownames(data)

data2 <-data%>%
  pivot_longer(cols = 1:3,
               #cols = 1:4,
               names_to = "Pathogen",
               values_to ="Count")


data2$x <-as.factor(data2$x)


data2 <- data2%>%
  group_by(x)%>% #按照X分组
  mutate(percentage=Count/sum(Count))#计算X里面每个项目的百分比
data2$percentage<-round(data2$percentage,3)#保留3位小数


cols <- c("Normal" = "white", "Adenoma" = "lightblue","Adenocarcinoma" = "#1E90FF")
data3 <- data[,1:3]
aa <- fisher.test(data3)
round(aa$p.value,3)
library(Unicode)
p.val <- ' = 0.041'
italic_p <- u_char_inspect(u_char_from_name("MATHEMATICAL ITALIC SMALL P"))["Char"]
labels = paste("Fisher's test ",italic_p, p.val, sep='')



ggplot(data2,aes(x=x,y=percentage,fill=Pathogen))+
  geom_bar(stat ="identity",alpha=0.9, color="black", width=0.7,size=0.5)+
  #geom_text(aes(label=Count),position = position_stack(vjust =0.5),size=3)+
  annotate("text", x = 1 , y = 0.075,label = "4") +
  annotate("text", x = 1 , y = 0.5,label = "22") +
  annotate("text", x = 1 , y = 0.98,label = "1") +
  annotate("text", x = 2 , y = 0.25,label = "8") +
  annotate("text", x = 2 , y = 0.75,label = "10") +
  scale_fill_manual(values = cols,
                    labels=c("Adenocarcinoma","Adenoma","Normal"))+
  scale_y_continuous(expand = c(0,0),label=scales::percent_format( suffix = ""))+#将纵坐标刻度转换为百分数
  #scale_y_continuous(expand = c(0,0),breaks = seq(0, 100, 20))+#将纵坐标刻度转换为百分数
  labs(y="Pathological diagnosis (%)",title = labels)+
  theme_classic()+
  theme(axis.title.x =element_blank(),
        plot.title = element_text(hjust = 0.5,size=14,color="black"),#标题居中
        axis.title=element_text(size=13,color="black",face = "bold"), # x,y轴名设置
        axis.text = element_text(size=12,color="black",face = "bold"), # x,y轴文本设置
        #legend.position = c(1, 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL)) #去除图例名称


#ggsave("fig3e.png",width = 3.5,height = 4.5)
ggsave("fig3e.pdf",width = 3.5,height = 4.5)


