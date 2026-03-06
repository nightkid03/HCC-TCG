library(ggplot2)
library(ggside)
library(vegan)
library(ape)

myallgroupshort_color=c("#2ca02c","#d62728")
#Figure1B
drawdata=readRDS("fig1B.rds")
drawdata_mean=aggregate(drawdata[,1:2],by=list(drawdata$groupshort),mean)
drawdata_mean_df=data.frame()
for(j in 1:nrow(drawdata_mean))
{
  tmpind=which(drawdata$groupshort==drawdata_mean$Group.1[j])
  dddd=data.frame(meanX1=rep(drawdata_mean$Axis.1[j],length(tmpind)),
                  meanX2=rep(drawdata_mean$Axis.2[j],length(tmpind)),drawdata[tmpind,c("Axis.1","Axis.2")],
                  Group=rep(drawdata_mean$Group.1[j],length(tmpind)))
  drawdata_mean_df=rbind(drawdata_mean_df,dddd)
}
ggplot(drawdata,aes(x=Axis.1,y=Axis.2))+
  geom_point(aes(color=groupshort),size=5)+
  geom_segment(data=drawdata_mean_df,aes(x=meanX1,y=meanX2,xend=Axis.1,yend=Axis.2,color=Group))+
  #stat_ellipse(aes(color=newgroup))+
  labs(x="PC1 (10.05%)",y="PC2 (7.05%)")+
  theme_bw()+
  theme(text=element_text(size=18),legend.title=element_blank())+
  scale_color_manual(values = myallgroupshort_color)+
  scale_fill_manual(values = myallgroupshort_color)+
  geom_xsidedensity(aes(fill = groupshort), alpha = 0.4, show.legend = FALSE)+
  geom_ysidedensity(aes(fill = groupshort), alpha = 0.4, show.legend = FALSE)
#Figure1D
drawdata=readRDS("fig1D.rds")
drawdata_mean=aggregate(drawdata[,1:2],by=list(drawdata$groupshort),mean)
drawdata_mean_df=data.frame()
for(j in 1:nrow(drawdata_mean))
{
  tmpind=which(drawdata$groupshort==drawdata_mean$Group.1[j])
  dddd=data.frame(meanX1=rep(drawdata_mean$Axis.1[j],length(tmpind)),
                  meanX2=rep(drawdata_mean$Axis.2[j],length(tmpind)),drawdata[tmpind,c("Axis.1","Axis.2")],
                  Group=rep(drawdata_mean$Group.1[j],length(tmpind)))
  drawdata_mean_df=rbind(drawdata_mean_df,dddd)
}
ggplot(drawdata,aes(x=Axis.1,y=Axis.2))+
  geom_point(aes(color=groupshort),size=5)+
  geom_segment(data=drawdata_mean_df,aes(x=meanX1,y=meanX2,xend=Axis.1,yend=Axis.2,color=Group))+
  #stat_ellipse(aes(color=newgroup))+
  labs(x="PC1 (13.53%)",y="PC2 (10.46%)")+
  theme_bw()+
  theme(text=element_text(size=18),legend.title=element_blank())+
  scale_color_manual(values = myallgroupshort_color)+
  scale_fill_manual(values = myallgroupshort_color)+
  geom_xsidedensity(aes(fill = groupshort), alpha = 0.4, show.legend = FALSE)+
  geom_ysidedensity(aes(fill = groupshort), alpha = 0.4, show.legend = FALSE)
#Figure1E
myallC1bray=readRDS('Figure1E_TCG_Bray.rds')
myallbray=readRDS('Figure1E_AllHQMAG_Bray.rds')
set.seed(333)
mantel(myallC1bray,myallbray)
myallC1bray_pcoa=pcoa(myallC1bray)
myallbray_pcoa=pcoa(myallbray)
set.seed(333)
protest(X=myallC1bray_pcoa$vectors[,1:2],Y=myallbray_pcoa$vectors[,1:2],choices=c(1,2))

pss=protest(Y=myallC1bray_pcoa$vectors[,1:2],X=myallbray_pcoa$vectors[,1:2],choices=c(1,2))
HQMAGspc=data.frame(PC1=pss$X[,1],PC2=pss$X[,2],class=(rep("1119HQMAGs",nrow(pss$X))),sample=rownames(pss$X))
C1pc=data.frame(PC1=pss$Yrot[,1],PC2=pss$Yrot[,2],class=(rep("TCG",nrow(pss$Yrot))),sample=rownames(pss$Yrot))
drawdata=rbind(C1pc,HQMAGspc)
ggplot(drawdata,aes(x=PC1,y=PC2))+
  geom_point(aes(color=class),size=4)+
  geom_path(aes(group=sample),alpha=0.6)+
  labs(x=paste("PC1"),y=paste("PC2"),color="")+
  theme_bw()+
  theme(text=element_text(size=40),legend.title = element_blank())+scale_color_manual(values = c("#317FD8","#FCBA12"))
