library(ggplot2)

#Figure6B
data=readRDS("Fig6B.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.73\n95% CI: (0.56-0.89)")

#Figure6C
yintercept=0.36
aucvalue=0.50
data=readRDS("Fig6C.rds")
ggplot(data,aes(x=rev(Recall),y=rev(Precision)))+
  geom_line(size=3)+
  #geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  geom_hline(yintercept=yintercept,color="grey",linetype=2,size=2)+
  labs(x="Recall",y="Precision")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.3,label=paste("AUC = ",aucvalue))
#Figure6E
data=readRDS("Fig6E.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label="AUC = 0.64\n95% CI: (0.51-0.76)")

#Figure6F
yintercept=0.48
aucvalue=0.63
data=readRDS("Fig6F.rds")
ggplot(data,aes(x=rev(Recall),y=rev(Precision)))+
  geom_line(size=3)+
  #geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  geom_hline(yintercept=yintercept,color="grey",linetype=2,size=2)+
  labs(x="Recall",y="Precision")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.3,label=paste("AUC = ",aucvalue))
