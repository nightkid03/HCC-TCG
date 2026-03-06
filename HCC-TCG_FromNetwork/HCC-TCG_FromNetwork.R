library(reshape2)
library(ggplot2)
library(dplyr)
library(purrr)
library(ggplot2)
library(igraph)
library(WGCNA)
#----
#0. local function
FastsparToABC=function(Fastspar_R,Fastspar_P)
{
  tmpdata=Fastspar_R
  tmpdata[lower.tri(tmpdata,diag = T)]=NA #set lower.tri and diag = NA
  tmpdata=melt(tmpdata) # melt the matrix
  tmpdata$node2=rep(colnames(Fastspar_R,length(colnames(Fastspar_R)))) # add node2 id
  tmpdata=tmpdata[which(is.na(tmpdata$value)==FALSE),]
  tmpR_abc=tmpdata
  tmpdata=Fastspar_P
  tmpdata[lower.tri(tmpdata,diag = T)]=NA #set lower.tri and diag = NA
  tmpdata=melt(tmpdata) # melt the matrix
  tmpdata$node2=rep(colnames(Fastspar_P,length(colnames(Fastspar_P)))) # add node2 id
  tmpdata=tmpdata[which(is.na(tmpdata$value)==FALSE),]
  tmpP_abc=tmpdata
  myresult=data.frame(Source=tmpP_abc$variable,Target=tmpP_abc$node2,FastsparR=tmpR_abc$value,FastsparP=tmpP_abc$value,stringsAsFactors = F)
  myresult$Source=as.character(myresult$Source)
  return(myresult)
}

FastsparABC_furprocess=function(myFastsparABC)
{
  myFastsparABC$Adjp=p.adjust(myFastsparABC$FastsparP,method = "BH")
  myFastsparABC$NorP="P"
  myFastsparABC$NorP[myFastsparABC$FastsparR<0]="N"
  myFastsparABC$R2=myFastsparABC$FastsparR^2
  for(i in 1:nrow(myFastsparABC))
  {
    if(myFastsparABC$Source[i]<myFastsparABC$Target[i])
    {
      myFastsparABC$link[i]=paste(myFastsparABC$Source[i],myFastsparABC$Target[i],sep = "/")
    }else{
      myFastsparABC$link[i]=paste(myFastsparABC$Target[i],myFastsparABC$Source[i],sep = "/")
    }
  }
  myFastsparABC$link_R=paste(myFastsparABC$link,myFastsparABC$NorP,sep = "=")
  return(myFastsparABC)
}

#----
#1. load GTDBTK result
myallGTDBTK=read.table("GTDBTK.txt",header = T,sep = "\t",row.names = 1,check.names = F)

#----
#2. load fastspar result
myallBTFastsparR=read.table('BT.R.txt',header = T,row.names = 1,sep = "\t",check.names = F)
myallBTFastsparP=read.table('BT.P.txt',header = T,row.names = 1,sep = "\t",check.names = F)
myallMTFastsparR=read.table('MT.R.txt',header = T,row.names = 1,sep = "\t",check.names = F)
myallMTFastsparP=read.table('MT.P.txt',header = T,row.names = 1,sep = "\t",check.names = F)

myalledge=list()
myalledge$BT=FastsparToABC(Fastspar_R = myallBTFastsparR,Fastspar_P = myallBTFastsparP)
myalledge$BT=FastsparABC_furprocess(myalledge$BT)
myalledge$Tumor=FastsparToABC(Fastspar_R = myallMTFastsparR,Fastspar_P = myallMTFastsparP)
myalledge$Tumor=FastsparABC_furprocess(myalledge$Tumor)
class(myalledge$BT$FastsparR) # check! should be numeric
class(myalledge$BT$FastsparP) # check! should be numeric

#----
#3. get signficiant edges
mysigedge=c()
mytestedge=myalledge# 
mysigedge.list=list()
alledge.list=list() # for bar plot
for (i in 1:2)
{
  mytestedge[[i]]$NorP="P"
  mytestedge[[i]]$NorP[mytestedge[[i]]$FastsparR<0]="N"
  mytestedge[[i]]$NorP[which(mytestedge[[i]]$Adjp>=0.05)]="U"
  ind=which(mytestedge[[i]]$Adjp<0.05)
  print(length(ind))
  tmpedge=mytestedge[[i]][ind,]
  print(length(unique(c(tmpedge$Source,tmpedge$Target))))
  mysigedge=c(mysigedge,tmpedge$link_R)
  mysigedge.list[[i]]=tmpedge
  alledge.list[[i]]=mytestedge[[i]]
}
mysigedge_count=table(mysigedge)
table(mysigedge_count)

c=alledge.list%>%reduce(full_join,by="link")
d=c[,c("link","NorP.x","NorP.y")]
d[is.na(d)]="U"
e=table(paste(d$NorP.x,d$NorP.y,d$NorP,sep = ""))
e=data.frame(e)
e$Freq_ratio=e$Freq/sum(e$Freq)*100
e=e[order(e$Freq),]
e$Var1=factor(e$Var1,levels = e$Var1)
ggplot(data = e,aes(x=Var1,y=Freq))+
  geom_bar(stat = "identity",aes(fill=Var1))+
  coord_flip()+
  labs(x="",y="Number")+
  theme_bw()+
  theme(text=element_text(size=40))+theme(legend.position = "none")  #--> figure S2

#----
#4 get stable correlations
a=names(mysigedge_count)[which(mysigedge_count==2)] # =2 -> in all 2 groups
alink=gsub("=.*","",a)
mysigedge_stable=data.frame(
  Source=gsub("/.*","",alink),
  Target=gsub(".*/","",alink),
  NorP=gsub(".*=","",a),
  link=alink
)
mysigedge_stable$NorPv2=ifelse(mysigedge_stable$NorP=="P",1,-1)
mysignode_stable=data.frame(node=unique(c(mysigedge_stable$Source,mysigedge_stable$Target)))
mysignode_stable$GTDBTK=myallGTDBTK$classification[match(mysignode_stable$node,myallGTDBTK$ID)]
write.table(mysigedge_stable,"372stable.edge.txt",sep = "\t",quote = F,row.names = F)
write.table(mysignode_stable,"372stable.node.txt",sep = "\t",quote = F,row.names = F)

#----
#5.load ccluster from Cytoscape 
mysignode_stable_ccluster=read.table("ccCluster.txt",header = T,sep = "\t",check.names = F,row.names = 1)
mysignode_stable$CCcluster=mysignode_stable_ccluster$ccCluster[match(mysignode_stable$node,rownames(mysignode_stable_ccluster))]
mysignode_stable$CCcluster=paste("C",mysignode_stable$CCcluster,sep = "")
#5.1 find clusters in C1
C1genome=mysignode_stable$node[which(mysignode_stable$CCcluster=="C1")]
ind=which(mysigedge_stable$Source%in%C1genome&mysigedge_stable$Target%in%C1genome)
tmpdata=graph.data.frame(mysigedge_stable[ind,],directed = F)
tmpdata=get.adjacency(tmpdata,attr="NorPv2",sparse = F)
tmpdist=1-tmpdata
tmpcluster=hclust(as.dist(tmpdist),method = "average")
hc=hclust(as.dist(tmpdist),method = "average")
b=plot(hc,hang=-1,cex=0.8)
mynamicmods=cutreeDynamic(dendro = hc,distM=tmpdist)
table(mynamicmods)

mynamicmods=data.frame(ID=colnames(tmpdata),g=mynamicmods)
mynamicmods$g=plyr::mapvalues(mynamicmods$g,from=c(0:25),to=LETTERS)
mynamicmods$g=paste("C1",mynamicmods$g,sep = "")
mynamicmods$ID_raw=rownames(myallGTDBTK)[match(mynamicmods$ID,myallGTDBTK$ID)]
