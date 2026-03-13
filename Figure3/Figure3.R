library(ggplot2)
library(patchwork)

#Figure3B
data=readRDS("Fig3B.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.7\n95% CI: (0.63-0.78)")
                                                   
#Figure3C
myalgroup_color=c("CRLM"="#d62728","HCC"="#034f84")
drawdata=readRDS("Fig3C.rds")
ggplot(drawdata,aes(x=Axis.1,y=Axis.2))+
  geom_point(aes(color=Group),size=5)+
  stat_ellipse(aes(color=Group))+
  labs(x="23.85%",y="10.99%")+
  theme_bw()+
  theme(text=element_text(size=40),legend.title=element_blank())+
  scale_color_manual(values = myalgroup_color)+
  scale_fill_manual(values = myalgroup_color)
#Figure3D
data=readRDS("Fig3D.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.78\n95% CI: (0.59-0.97)")

#Figure3E
yintercept=0.82
myauc=0.94
data=readRDS("Fig3E.rds")
ggplot(data,aes(x=rev(Recall),y=rev(Precision)))+
  geom_line(size=3)+
  #geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  geom_hline(yintercept=yintercept,color="grey",linetype=2,size=2)+
  labs(x="Recall",y="Precision")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.3,label=paste("AUC = ",myauc))
#Figure3F
myall3group_color=c("CTR"="#2ca02c","HCC"="#034f84","iCCA"="#d62728")
data=readRDS("Fig3F.rds")
ggplot(data,aes(x=Axis.1,y=Axis.2))+
  geom_point(aes(color=Group),size=5)+
  stat_ellipse(aes(color=Group))+
  labs(x="17.5%",y="10.51%")+
  theme_bw()+
  theme(text=element_text(size=40),legend.title=element_blank())+
  scale_color_manual(values =myall3group_color )
#Figure3G
data=readRDS("Fig3G.rds")
ggplot(data=data,aes(P1,P2,fill=R2))+
  geom_tile()+geom_text(aes(P1,P2,label=symbol),size=10)+
  theme_classic()+
  scale_fill_gradient(low="#EDE387",high="#868b9f")+
  labs(fill=bquote(R^2),x="",y="")+
  theme(text=element_text(size=4),plot.title = element_text(hjust = 0.5,size=18),
        legend.title = element_text(size=12),legend.text = element_text(size=12),axis.text = element_text(color="black"),
        axis.text.x = element_text(angle = 90,size=20),axis.text.y = element_text(size=20))
#Figure3H
data=readRDS("Fig3H.rds")
mypic=list()
mypic[[1]]=ggplot(data,aes(x=group,y=C1A))+
  geom_boxplot(aes(fill=group),stat = "boxplot",position = "identity",outlier.size = 2,outlier.shape = NA)+
  geom_jitter(size=1)+
  labs(y="Abundance (%) of\nC1A",x="")+
  theme_bw()+
  annotate("segment",x = 1, xend = 2, y = 45, yend = 45,colour = "#000000")+
  annotate("text",x=1.5,y=45,label="*",size=10)+
  annotate("segment",x = 1, xend = 3, y = 47, yend = 47,colour = "#000000")+
  annotate("text",x=2,y=47,label="***",size=10)+
  theme(text=element_text(size=40),legend.position = "none")+scale_fill_manual(values=myall3group_color)
mypic[[2]]=ggplot(data,aes(x=group,y=C1B))+
  geom_boxplot(aes(fill=group),stat = "boxplot",position = "identity",outlier.size = 2,outlier.shape = NA)+
  geom_jitter(size=1)+
  labs(y="Abundance (%) of\nC1B",x="")+ # Abundance of C1B (%)
  theme_bw()+
  annotate("segment",x = 1, xend = 2, y = 30, yend = 30,colour = "#000000")+
  annotate("text",x=1.5,y=30,label="***",size=10)+
  annotate("segment",x = 1, xend = 3, y = 27, yend = 27,colour = "#000000")+
  annotate("text",x=2,y=28,label="#",size=8)+
  theme(text=element_text(size=40),legend.position = "none")+scale_fill_manual(values=myall3group_color)
wrap_plots(mypic)+plot_layout(guides = "collect")
#Figure3I
data=readRDS("Fig3I.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.79\n95% CI: (0.67-0.91)")

#Figure3J
data=readRDS("Fig3J.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.85\n95% CI: (0.75-0.96)")

#Figure3K
data=readRDS("Fig3K.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.72\n95% CI: (0.57-0.88)")

