# HCC-TCG
Codes related to "Two Stable Gut Microbiome Guilds Predict Liver Tumor Class and Treatment Responses"
-------------

## 1.	Data quality control of metagenomes

### Tool: kneaddata (https://huttenhower.sph.harvard.edu/kneaddata/)

### Input: $R1: R1 reads, $R2: R2 reads
```
kneaddata -i1 $R1 -i2 $R2 -o $prefix -db $refdb --output-prefix $prefix \
-t 20 --decontaminate-pairs strict --sequencer-source $sequencer_source --run-trim-repetitive \
--trimmomatic-options=\"ILLUMINACLIP:$adapter:2:30:10 SLIDINGWINDOW:4:20 MINLEN:60\" \
--run-fastqc-start --run-fastqc-end --bypass-trf \
--trimmomatic Trimmomatic-0.39 \\
--fastqc FastQC --remove-intermediate-output –reorder
```

## 2.	De novo assembles contigs from high quality reads.

### Tool: Megahit (https://github.com/voutcn/megahit)

### Input: $R1: high quality reads R1, $R2: high quality reads R2
```
megahit -1 $R1 -2 $R2 -o $prefix --min-contig-len 500 --tmp-dir $tmp_dir --num-cpu-threads 16 --presets meta-large
```

## 3. Binning assembled contigs

### Tool: metawrap (https://github.com/bxlab/metaWRAP)

### Input: $R1: high quality reads R1, $R2: high quality reads R2
```
metawrap binning -t 16 -a $input -o $prefix --metabat2 --maxbin2 $R1 $R2
```

## 4. Bin refinement

### Tool: metawrap (https://github.com/bxlab/metaWRAP)
### Input: $bin1: output of maxbin2, $bin2: output of metabat2 (remove bin.unbinned.fa)
```
metawrap bin_refinement -t 16 -c 90 -x 5 -o $prefix -A $bin1 -B $bin2
```

## 5. Check bin quality

### Tool: Checkm (https://github.com/Ecogenomics/CheckM)

### Input: $input: refined bins from step 4.
```
checkm lineage_wf -f $prefix.CheckM.txt -t 16 -x fa $input $prefix
```

## 6. Genome dereplication 

### Tool: dRep (https://drep.readthedocs.io/en/latest/)

### Input: all bins meet quality cutoff
```
dRep dereplicate --S_algorithm ANIn -sa 0.99 -p 16 --ignoreGenomeQuality $output -g $input/*.fa
```
## 7.	Genome Abundance estimation

### Tool: Ditasic (https://rki_bioinformatics.gitlab.io/ditasic/)

### Input: dereplicated genomes, high quality reads. 

-------------
## 8. Identify TCG (R codes)

### inputs
myalldata (sample metadata, n samples x column) 

myallabun (HQMAG abundance table, n sample x m HQMAGs. Result from Ditasic)

myallgroup_color (color codes for each group)

myallhqreads (number of high quality reads of each sample)


```
myallabun_rel=myallabun
for(i in 1:nrow(myallabun_rel))
{
  myallabun_rel[i,]= myallabun_rel[i,]/myallhqreads[i]
}
myallabun_rel[myallabun_rel <0.00001]=0
```

### 8.1 beta diversity, PCoA
```
mydata=myalldata
myabun=myallabun_rel[rownames(mydata),]
mydist=as.matrix(vegdist(myabun,method="bray"))
tmppcoa=pcoa(mydist)
a=diag(t(tmppcoa$vectors)%*%tmppcoa$vectors)
explain=a/sum(a)*100
explain=round(explain,2)
drawdata=data.frame(mydata,tmppcoa$vectors)
ggplot(drawdata,aes(x=Axis.1,y=Axis.2))+
  geom_point(aes(color=group),size=5)+
  labs(x=paste("PC1 (",explain[1],"%)"),y=paste("PC2 (",explain[2],"%)"),)+
  theme_bw()+
  theme(text=element_text(size=18),legend.title=element_blank())+
  scale_color_manual(values = myallgroup_color)+
  scale_fill_manual(values = myallgroup_color)
```

### 8.2 PERMANOVA test
```
pairwise.adonis <-function(x,factors,p.adjust.m)
{
  library(vegan)
  co = as.matrix(combn(unique(factors),2))
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  for(elem in 1:ncol(co)){
    ind1=which(factors==co[1,elem])
    ind2=which(factors==co[2,elem])
    m=x[c(ind1,ind2),c(ind1,ind2)]
    group=c(factors[ind1],factors[ind2])
    m=as.dist(m)
    group=as.data.frame(group)
    set.seed(33)
    ad=adonis2(m~group,group,permutations = 999)
    pairs =c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$F[1]);
    R2 = c(R2,ad$R2[1]);
    p.value = c(p.value,ad$`Pr(>F)`[1])
  }
  p.adjusted =p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}
pairwise.adonis(x = mydist,factors = mydata$group,p.adjust.m = "BH")
```

### 8.3 Selece HQMAGs for Fastspar
```
b=aggregate(myallabun_rel,by=list(myalldata$group),FUN = function(x){length(which(x>0))/length(x)})  # group: BT, MT
rownames(b)=b$Group.1
b$Group.1=NULL
apply(b,1,FUN = function(x){length(which(x>0.75))})
selectHQMAG=colnames(b)[which(apply(b,2,min)>0.75)]  #HQMAGs have prevalence > 75% in each group.  
```
### 8.4 run Fastspar, https://github.com/scwatts/fastspar. 

### 8.5 load Fastspar result, R and P matrix
```
myall_g1_FastsparR=read.table(...)# group g1 Fastspar R   # g1: BT, g2: MT
myall_g1_FastsparP=read.table(...)# group g1 Fastspar P
myall_g2_FastsparR=read.table(...)# group g2 Fastspar R
myall_g2_FastsparP=read.table(...)# group g2 Fastspar P
```
### 8.6 transfer wide to long format
```
FastsparToABC=function(Fastspar_R,Fastspar_P)
{
  tmpdata=Fastspar_R
  tmpdata[lower.tri(tmpdata,diag = T)]=NA #set lower.tri and diag = NA
  tmpdata=melt(tmpdata) # melt the matrix
  tmpdata$node2=rep(colnames(Fastspar_R)) # add node2 id
  tmpdata=tmpdata[which(is.na(tmpdata$value)==FALSE),]
  tmpR_abc=tmpdata
  tmpdata=Fastspar_P
  tmpdata[lower.tri(tmpdata,diag = T)]=NA #set lower.tri and diag = NA
  tmpdata=melt(tmpdata) # melt the matrix
  tmpdata$node2=rep(colnames(Fastspar_R)) # add node2 id
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

myalledge=list()
myalledge$g1=FastsparToABC(myall_g1_FastsparR,myall_g1_FastsparP)
myalledge$g1=FastsparABC_furprocess(myalledge$g1)
myalledge$g2=FastsparToABC(myall_g2_FastsparR,myall_g2_FastsparP)
myalledge$g2=FastsparABC_furprocess(myalledge$g2)
class(myalledge$g1$FastsparR) # should be numeric
class(myalledge$g1$FastsparP) # should be numeric
```

### 8.7 significant edges
```
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
```
### 8.8 find stable edges and output for Cytoscape
```
a=names(mysigedge_count)[which(mysigedge_count==length(mysigedge.list)]
alink=gsub("=.*","",a)
mysigedge_stable=data.frame(
  Source=gsub("/.*","",alink),
  Target=gsub(".*/","",alink),
  NorP=gsub(".*=","",a),
  link=alink
)
mysigedge_stable$NorPv2=ifelse(mysigedge_stable$NorP=="P",1,-1)
print(dim(mysigedge_stable))
mysignode_stable=data.frame(node=unique(c(mysigedge_stable$Source,mysigedge_stable$Target)))
write.table(mysigedge_stable,"stable.edge.txt",sep = "\t",quote = F,row.names = F)
write.table(mysignode_stable,"stable.node.txt",sep = "\t",quote = F,row.names = F)
```
### 8.9 input into Cytoscape and do Connected components clustering analysis

### 8.10 load connected components clustering analysis result
```
mysignode_stable_ccluster=read.table("components clustering analysis result")
mysignode_stable$CCcluster=mysignode_stable_ccluster$ccCluster[match(mysignode_stable$node,rownames(mysignode_stable_ccluster))]
mysignode_stable$CCcluster=paste("C",mysignode_stable$CCcluster,sep = "")
```

### 8.11 further clustering C1
```
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
```
-------------
## 9.	RandomForest, classificaiton
```
selectgroup=c("g1","g2")
ind=which(myalldata$group%in%selectgroup)
mydata=myalldata[ind,]
mydata$group=factor(as.character(mydata$group))
myabun_TCGgenome=myallabun[ind,TCG_HQMAGs]
tmptraindata=mydata
tmptrainabun=myabun_TCGgenome
train.control=trainControl(method="LOOCV", savePredictions=TRUE,classProbs = TRUE)
set.seed(3)
registerDoMC(2) # parallel 
model=train(x=tmptrainabun_rel,y=tmptraindata$Group,method = "rf",trControl = train.control)
```
-------------
## 10. Mantel test and procrustes analysis
```
myallC1bray=as.matrix(vegdist(myallabun_rel[,C1genome] ,method = "bray"))
myallbray=as.matrix(vegdist(myallabun_rel,method = "bray"))
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
drawdata$group=myalldata$group[match(drawdata$sample,rownames(myalldata))]
ggplot(drawdata,aes(x=PC1,y=PC2))+
  geom_point(aes(color=class),size=4)+
  geom_path(aes(group=sample),alpha=0.6)+
  #stat_ellipse(aes(color=group))+
  labs(x=paste("PC1"),y=paste("PC2"),color="")+
  theme_bw()+
  theme(text=element_text(size=40),legend.title = element_blank())+scale_color_manual(values = c("#317FD8","#FCBA12"))

