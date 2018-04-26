library(openxlsx)
rfm<-read.xlsx("C:/Users/sh002060/Desktop/数据集.xlsx",1)
names(rfm)

i<-5
result<-kmeans(fencheng,i)
table(result$cluster)
f<-rfm[,c(2,3,6)]
#构建标准化函数
standard <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
zsredfile<-sapply(X=f,FUN=standard)

result_index<-c()
k_set<-c(2:8)
for(i in k_set)
{
  result<-kmeans(zsredfile,i)
  result_index<-c(result_index,result$betweens/(result$tot.withinss+result$betweens))
  
}
plot(k_set,result_index,type='o')
result$betweens
i<-4
result<-kmeans(zsredfile,i)
table(result$cluster)
centervec<-result$centers
#客户价值分析可视化
library(ggplot2)
library(tidyr)
library(dplyr)
##聚类中心值
rownames(centervec) <- c('客户群1', '客户群2', '客户群3', '客户群4')
centervec <- as.data.frame(centervec)
centervec$客户群 <- rownames(centervec)
centervec <- centervec[, c(4,1:3)]
data <- centervec %>% gather(rfm, 聚类中心值, -客户群)
##体现不同的客户的价值
ggplot(data, aes(x=客户群, y=聚类中心值, fill=rfm))　+
  geom_bar(position = 'dodge', stat = 'identity') +
  theme(axis.text.x = element_text(size = rel(1.5)), axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2))) +
  ggtitle('聚类结果可视化分析') 
rfm$聚类结果<-result$cluster
