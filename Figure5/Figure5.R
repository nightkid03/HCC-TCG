library(ggplot2)

myallRecurrence_2y_color=c("No_Recurrence"="#5F8AFE","Recurrence"="#FDBE60")
#Figure5A
data=readRDS("Fig5A.rds")
ggplot(data,aes(x=group,y=mean))+
  geom_bar(aes(fill=group),color="black",stat="identity",position = position_dodge())+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  labs(x="",y="GGT")+
  theme_bw()+coord_flip()+
  theme(text=element_text(size=40),legend.position = "none",axis.text = element_text(color="black"))+
  scale_fill_manual(values = myallRecurrence_2y_color)
#Figure5B
data=readRDS("Fig5B.rds")
ggplot(data,aes(x=group,y=mean))+
  geom_bar(aes(fill=group),color="black",stat="identity",position = position_dodge())+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  labs(x="",y="GGT")+
  theme_bw()+coord_flip()+
  theme(text=element_text(size=40),legend.position = "none",axis.text = element_text(color="black"))+
  scale_fill_manual(values = myallRecurrence_2y_color)
#Figure5D
data=readRDS("Fig5D.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.78\n95% CI: (0.56-1)")

#Figure5E
yintercept=0.37
aucvalule=0.72
data=readRDS("Fig5E.rds")
ggplot(data,aes(x=rev(Recall),y=rev(Precision)))+
  geom_line(size=3)+
  #geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  geom_hline(yintercept=yintercept,color="grey",linetype=2,size=2)+
  labs(x="Recall",y="Precision")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.3,label=paste("AUC = ",aucvalule))
#Figure5F
data=readRDS("Fig5F.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.84\n95% CI: (0.69-1)")

#Figure5G
yintercept=0.34
aucvalule=0.80
data=readRDS("Fig5G.rds")
ggplot(data,aes(x=rev(Recall),y=rev(Precision)))+
  geom_line(size=3)+
  #geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  geom_hline(yintercept=yintercept,color="grey",linetype=2,size=2)+
  labs(x="Recall",y="Precision")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.3,label=paste("AUC = ",aucvalule))
#Figure5H
data=readRDS("Fig5H.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.89\n95% CI: (0.74-1)")

#Figure5I
yintercept=0.1
aucvalule=0.64
data=readRDS("Fig5I.rds")
ggplot(data,aes(x=rev(Recall),y=rev(Precision)))+
  geom_line(size=3)+
  #geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  geom_hline(yintercept=yintercept,color="grey",linetype=2,size=2)+
  labs(x="Recall",y="Precision")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.3,label=paste("AUC = ",aucvalule))
