library(ggplot2)
library(readxl)
library(ggvenn)
library(reshape2)
library(patchwork)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      sem= sd(x[[col]], na.rm=TRUE)/sqrt(length(which(is.na(x[[col]])==FALSE))))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}



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
#Figure2B
myallko=data.frame(read_xlsx("Figure2B_2E_2F.xlsx",sheet = "2B"),row.names = 1)
C1Alist=rownames(myallko)[which(myallko$C1A!=0)]
C1Blist=rownames(myallko)[which(myallko$C1B!=0)]
ggvenn(list(C1A=C1Alist,C1B=C1Blist),fill_color = c("#63953C","#5516BC"),set_name_size = 5,stroke_size = 0.5,show_percentage = F)
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
#Figure2E
myallscfa=data.frame(read_xlsx("Figure2B_2E_2F.xlsx",sheet = "2E"),row.names = 1)
mylist=c("fthfs","scpC","pct","But","Buk","AtoA","AtoD","4Hbt")
mypic=list()
for(i in 1:length(mylist)){
  tmpdata=myallscfa[which(myallscfa$gene==mylist[i]),]
  tmptest=wilcox.test(value~group,data=tmpdata)
  print(paste(mylist[i],tmptest$p.value))
  df2 <- data_summary(tmpdata, varname="value",
                      groupnames=c("group"))
  mypic[[i]]=ggplot(df2,aes(x=group,y=mean))+
    geom_bar(aes(fill=group),color="black",stat="identity",position = position_dodge())+
    geom_errorbar(aes(ymin=mean,ymax=mean+sd), width=.2,
                  position=position_dodge(.9)) +
    geom_jitter(data=tmpdata,aes(x=group,y=value),size=2,position = position_jitter(width = 0.3,height = 0),alpha=0.3)+
    labs(x="",y="Copy Number",title=mylist[i])+
    theme_bw()+
    theme(text=element_text(size=18),plot.title = element_text(hjust = 0.5,size=18),legend.title = element_blank(),axis.text = element_text(color="black"))+
    scale_fill_manual(values = myalltcgcolor)
}
wrap_plots(mypic)+plot_layout(guides = "collect",nrow = 4)
#Figure2F
myallvf=data.frame(read_xlsx("Figure2B_2E_2F.xlsx",sheet = "2F"),row.names = 1)
myallvf$VF=rownames(myallvf)
drawdata=melt(myallvf,)
ggplot(data=drawdata,aes(x=VF,y=value))+
  geom_bar(stat = "identity", position = "dodge",aes(fill=variable))+
  labs(x="",y="Number of VF genes")+
  theme_bw()+
  coord_flip()+
  theme(text=element_text(size = 30),legend.title = element_blank(),axis.text = element_text(color="black"))+
  scale_fill_manual(values = myalltcgcolor)

#Figure2G
drawdata=readRDS("Fig2G.rds")
pheatmap::pheatmap(t(drawdata),cluster_rows = F,cluster_cols = F,angle_col=90,
                   color=colorRampPalette(c("#0D1740","white","#EDE387","#F17C67"))(50),cellwidth = 9,cellheight = 9)

