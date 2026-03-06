library(ggplot2)

myalltcgcolor=c("C1A"="#63953C","C1B"="#5516BC")
#Figure2A
drawdata=readRDS("Fig2A.rds")
drawdata_mean=aggregate(drawdata[,1:2],by=list(drawdata$Group),mean)
drawdata_mean_df=data.frame()
for(j in 1:nrow(drawdata_mean))
{
  tmpind=which(drawdata$Group==drawdata_mean$Group.1[j])
  dddd=data.frame(meanX1=rep(drawdata_mean$Axis.1[j],length(tmpind)),
                  meanX2=rep(drawdata_mean$Axis.2[j],length(tmpind)),drawdata[tmpind,c("Axis.1","Axis.2")],
                  Group=rep(drawdata_mean$Group.1[j],length(tmpind)))
  drawdata_mean_df=rbind(drawdata_mean_df,dddd)
}
ggplot(drawdata,aes(x=Axis.1,y=Axis.2))+
  geom_point(aes(color=Group),size=5)+
  geom_segment(data=drawdata_mean_df,aes(x=meanX1,y=meanX2,xend=Axis.1,yend=Axis.2,color=Group))+
  labs(x="21.05%",y="13.8%")+
  theme_bw()+
  theme(text=element_text(size=24),plot.title = element_text(hjust = 0.5,size=18),legend.title = element_blank(),axis.text = element_text(color="black"))+
  scale_color_manual(values = myalltcgcolor)
#Figure2C
drawdata=readRDS("Fig2C.rds")
ggplot(data=drawdata,aes(x=reorder(Description_wrapped,GeneRatioGW),y=GeneRatioGW))+
  geom_point(aes(size=Count,color=pvalue))+
  coord_flip()+
  scale_color_gradient(low = "#CD212A", high = "#00758F")+
  scale_size_continuous(name = "Count",range = c(5, 15)) +
  labs(x="",y="GeneRatio",color="P value")+
  theme_bw()+theme()+
  theme(text=element_text(size = 30))+guides(color = guide_colorbar(order = 1),size = guide_legend(order = 2))
#Figure2D
drawdata=readRDS("Fig2D.rds")
ggplot(data=drawdata,aes(x=reorder(Description_wrapped,GeneRatioGW),y=GeneRatioGW))+
  geom_point(aes(size=Count,color=pvalue))+
  coord_flip()+
  scale_color_gradient(low = "#CD212A", high = "#00758F")+
  scale_size_continuous(name = "Count",range = c(5, 15)) +
  labs(x="",y="GeneRatio",color="P value")+
  theme_bw()+theme()+
  theme(text=element_text(size = 30))+guides(color = guide_colorbar(order = 1),size = guide_legend(order = 2))
#Figure2G
drawdata=readRDS("Fig2G.rds")
pheatmap::pheatmap(t(drawdata),cluster_rows = F,cluster_cols = F,angle_col=90,
                   color=colorRampPalette(c("#0D1740","white","#EDE387","#F17C67"))(50),cellwidth = 9,cellheight = 9)

