#Figure4B
data=readRDS("Fig4B.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label=paste("AUC = ",round(g$auc,2),
                                                   "\n95% CI: (",round(ci95$ci[1],2),"-",round(ci95$ci[3],2),")",
                                                   sep = ""))
#Figure4C
yintercept=0.43
aucvalule=0.76
data=readRDS("Fig4C.rds")
ggplot(AUPRC_draw,aes(x=rev(Recall),y=rev(Precision)))+
  geom_line(size=3)+
  #geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  geom_hline(yintercept=yintercept,color="grey",linetype=2,size=2)+
  labs(x="Recall",y="Precision")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.3,label=paste("AUC = ",aucvalule))
#Figure4D
data=readRDS("Fig4D.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label=paste("AUC = ",round(g$auc,2),
                                                   "\n95% CI: (",round(ci95$ci[1],2),"-",round(ci95$ci[3],2),")",
                                                   sep = ""))
#Figure4E
yintercept=0.34
aucvalule=0.67
data=readRDS("Fig4E.rds")
ggplot(data,aes(x=rev(Recall),y=rev(Precision)))+
  geom_line(size=3)+
  #geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  geom_hline(yintercept=yintercept,color="grey",linetype=2,size=2)+
  labs(x="Recall",y="Precision")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.3,label=paste("AUC = ",aucvalule))
#Figure4F
data=readRDS("Fig4F.rds")
ggplot(data,aes(x=specificity,y=sensitivity))+
  geom_line(size=3)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  labs(x="1-Specificity",y="Sensitivity")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.25,label=paste("AUC = ",round(g$auc,2),
                                                   "\n95% CI: (",round(ci95$ci[1],2),"-",round(ci95$ci[3],2),")",
                                                   sep = ""))
#Figure4G
yintercept=0.11
aucvalule=0.42
data=readRDS("Fig4G.rds")
ggplot(data,aes(x=rev(Recall),y=rev(Precision)))+
  geom_line(size=3)+
  #geom_segment(x=0,y=0,xend=1,yend=1,size=2,color="grey",linetype=2)+
  geom_hline(yintercept=yintercept,color="grey",linetype=2,size=2)+
  labs(x="Recall",y="Precision")+
  theme_bw()+
  theme(text=element_text(size = 40))+
  annotate("text",size=10,x=0.8,y=0.3,label=paste("AUC = ",aucvalule))
