library(readxl)
library(tidyverse)
library(tidyr)
library(readr)
library(ggplot2)

#Read and gather the LJ List.
LJ1<-read_excel("D:/OneDrive/Excel/LJ List.xlsx",1)
LJ2<-read_excel("D:/OneDrive/Excel/LJ List.xlsx",2)
LJ3<-read_excel("D:/OneDrive/Excel/LJ List.xlsx",3)
LJG<-gather(LJ1,Language,Morpheme1,2:67)
LJ2G<-gather(LJ2,Language,Morpheme2,2:67)
LJ3G<-gather(LJ3,Language,Morpheme3,2:67)

LJG$Morpheme<-paste(LJG$Morpheme1,LJ2G$Morpheme2,LJ3G$Morpheme3,sep=".")
LJG$Morpheme<-gsub(".NA","",LJG$Morpheme)
LJG$Morpheme<-gsub("NA","",LJG$Morpheme)
LJG$Morpheme1<-NULL

LJG<-cbind(LJG,str_split_fixed(LJG$Morpheme,"[.]",n=Inf))
LJG$Morpheme<-NULL
LJG[,3:ncol(LJG)]<-lapply(3:ncol(LJG),function(x){as.character(LJG[,x])})
LJGG<-gather(LJG,Number,Phoneme,3:ncol(LJG))
LJGG<-subset(LJGG,Phoneme!="")

#Classify phonemes that are [+feature] as vectors.
IPA<-read.csv("D:/OneDrive/Excel/ipa_all.csv",header=TRUE,encoding="UTF-8")
IPA$ipa<-gsub("͡","",IPA$ipa)

syl<-subset(IPA,syl=="+")
syl<-syl$ipa
LJGG$'+syllabic' <- c(LJGG$Phoneme %in% syl)
son<-subset(IPA,son=="+")
son<-son$ipa
LJGG$'+sonorant' <- c(LJGG$Phoneme %in% son)
cons<-subset(IPA,cons=="+")
cons<-cons$ipa
LJGG$'+consonantal' <- c(LJGG$Phoneme %in% cons)
cont<-subset(IPA,cont=="+")
cont<-cont$ipa
LJGG$'+continuant' <- c(LJGG$Phoneme %in% cont)
delrel<-subset(IPA,delrel=="+")
delrel<-delrel$ipa
LJGG$'+delayed release' <- c(LJGG$Phoneme %in% delrel)
lat<-subset(IPA,lat=="+")
lat<-lat$ipa
LJGG$'+lateral' <- c(LJGG$Phoneme %in% lat)
nas<-subset(IPA,nas=="+")
nas<-nas$ipa
LJGG$'+nasal' <- c(LJGG$Phoneme %in% nas)
strid<-subset(IPA,strid=="+")
strid<-strid$ipa
LJGG$'+strident' <- c(LJGG$Phoneme %in% strid)
voi<-subset(IPA,voi=="+")
voi<-voi$ipa
LJGG$'+voiced' <- c(LJGG$Phoneme %in% voi)
sg<-subset(IPA,sg=="+")
sg<-sg$ipa
LJGG$'+spread' <- c(LJGG$Phoneme %in% sg)
cg<-subset(IPA,cg=="+")
cg<-cg$ipa
LJGG$'+constricted' <- c(LJGG$Phoneme %in% cg)
ant<-subset(IPA,ant=="+")
ant<-ant$ipa
LJGG$'+anterior' <- c(LJGG$Phoneme %in% ant)
cor<-subset(IPA,cor=="+")
cor<-cor$ipa
LJGG$'+coronal' <- c(LJGG$Phoneme %in% cor)
distr<-subset(IPA,IPA$dist=="+")
distr<-distr$ipa
LJGG$'+distributed' <- c(LJGG$Phoneme %in% distr)
lab<-subset(IPA,lab=="+")
lab<-lab$ipa
LJGG$'+labial' <- c(LJGG$Phoneme %in% lab)
hi<-subset(IPA,hi=="+")
hi<-hi$ipa
LJGG$'+high' <- c(LJGG$Phoneme %in% hi)
lo<-subset(IPA,lo=="+")
lo<-lo$ipa
LJGG$'+low' <- c(LJGG$Phoneme %in% lo)
back<-subset(IPA,back=="+")
back<-back$ipa
LJGG$'+back' <- c(LJGG$Phoneme %in% back)
round<-subset(IPA,round=="+")
round<-round$ipa
LJGG$'+round' <- c(LJGG$Phoneme %in% round)
velaric<-subset(IPA,velaric=="+")
velaric<-velaric$ipa
LJGG$'+velaric' <- c(LJGG$Phoneme %in% velaric)
tense<-subset(IPA,tense=="+")
tense<-tense$ipa
LJGG$'+tense' <- c(LJGG$Phoneme %in% tense)
long<-subset(IPA,long=="+")
long<-long$ipa
LJGG$'+long' <- c(LJGG$Phoneme %in% long)

#Delete features that are present in less than 50 languages.
LJsyl<-split(LJGG$'+syllabic',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJsyl[[x]])})),na.rm=TRUE) < 50) {LJGG$'+syllabic'<-NULL}
LJson<-split(LJGG$'+sonorant',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJson[[x]])})),na.rm=TRUE) < 50) {LJGG$'+sonorant'<-NULL}
LJcons<-split(LJGG$'+consonantal',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJcons[[x]])})),na.rm=TRUE) < 50) {LJGG$'+consonantal'<-NULL}
LJcont<-split(LJGG$'+continuant',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJcont[[x]])})),na.rm=TRUE) < 50) {LJGG$'+continuant'<-NULL}
LJdelrel<-split(LJGG$'+delayed release',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJdelrel[[x]])})),na.rm=TRUE) < 50) {LJGG$'+delayed release'<-NULL}
LJlat<-split(LJGG$'+lateral',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJlat[[x]])})),na.rm=TRUE) < 50) {LJGG$'+lateral'<-NULL}
LJnas<-split(LJGG$'+nasal',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJnas[[x]])})),na.rm=TRUE) < 50) {LJGG$'+nasal'<-NULL}
LJstrid<-split(LJGG$'+strident',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJstrid[[x]])})),na.rm=TRUE) < 50) {LJGG$'+strident'<-NULL}
LJvoi<-split(LJGG$'+voiced',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJvoi[[x]])})),na.rm=TRUE) < 50) {LJGG$'+voiced'<-NULL}
LJsg<-split(LJGG$'+spread',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJsg[[x]])})),na.rm=TRUE) < 50) {LJGG$'+spread'<-NULL}
LJcg<-split(LJGG$'+constricted',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJcg[[x]])})),na.rm=TRUE) < 50) {LJGG$'+constricted'<-NULL}
LJant<-split(LJGG$'+anterior',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJant[[x]])})),na.rm=TRUE) < 50) {LJGG$'+anterior'<-NULL}
LJcor<-split(LJGG$'+coronal',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJcor[[x]])})),na.rm=TRUE) < 50) {LJGG$'+coronal'<-NULL}
LJdistr<-split(LJGG$'+distributed',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJdistr[[x]])})),na.rm=TRUE) < 50) {LJGG$'+distributed'<-NULL}
LJlab<-split(LJGG$'+labial',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJlab[[x]])})),na.rm=TRUE) < 50) {LJGG$'+labial'<-NULL}
LJhi<-split(LJGG$'+high',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJhi[[x]])})),na.rm=TRUE) < 50) {LJGG$'+high'<-NULL}
LJlo<-split(LJGG$'+low',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJlo[[x]])})),na.rm=TRUE) < 50) {LJGG$'+low'<-NULL}
LJback<-split(LJGG$'+back',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJback[[x]])})),na.rm=TRUE) < 50) {LJGG$'+back'<-NULL}
LJround<-split(LJGG$'+round',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJround[[x]])})),na.rm=TRUE) < 50) {LJGG$'+round'<-NULL}
LJvelaric<-split(LJGG$'+velaric',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJvelaric[[x]])})),na.rm=TRUE) < 50) {LJGG$'+velaric'<-NULL}
LJtense<-split(LJGG$'+tense',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJtense[[x]])})),na.rm=TRUE) < 50) {LJGG$'+tense'<-NULL}
LJlong<-split(LJGG$'+long',LJGG$Language)
if (sum(unlist(lapply(1:66,function(x){any(LJlong[[x]])})),na.rm=TRUE) < 50) {LJGG$'+long'<-NULL}

#Splitting the LJ Lists by the 100 meanings
LJm<-split(LJGG,LJGG[,'Meaning'])

#Counting the number of phonemes per each meaning
N<-sapply(1:100,function(x){sum(!is.na(LJm[[x]]$Phoneme))})

#Counting the number of phonemes that are [+feature]
n<-lapply(5:ncol(LJGG),function(y){lapply(1:100,function(x){sum(LJm[[x]][,y],na.rm=TRUE)})})
n<-lapply(1:length(n),function(x){as.integer(n[[x]])})
names(n)<-colnames(LJGG[,5:ncol(LJGG)])

#Calculating the mean proportion of each [+feature] across 100 meanings
M<-lapply(1:length(n),function(y){(lapply(1:100,function(x){mean(n[[y]][x]/N[[x]])}))})
M<-sapply(1:length(n),function(x){mean(as.numeric(M[[x]]))})

#p-values
pvalues<-data.frame(LJ1$Meaning)
pvalues[2:(length(n)+1)]<-lapply(1:length(n),function(x){lapply(1:100,function(y){as.numeric(binom.test(n[[x]][y],N[y],p=M[x])[3])})})
colnames(pvalues)<-c("Meaning",colnames(LJGG)[5:ncol(LJGG)])
values<-gather(pvalues,Feature,pvalue,2:ncol(pvalues))
values$pvalue<-as.numeric(values$pvalue)
values$Association<-unlist(lapply(1:length(n),function(x){lapply(1:100,function(y){n[[x]][y]/N[y]>M[x]})}))

#Multiple Correction
values$FDR<-p.adjust(values$pvalue,method="BH")
correlations<-subset(values,FDR<0.1)

#GGplot
ggplot(correlations,aes(Feature,Meaning))+geom_tile(aes(fill=Association),colour="black")+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_fill_manual(values=c("salmon","royalblue1"),labels=c("Negative","Positive"))

#Morpheme length
LJGno0<-subset(LJG,LJG[,3]!="")
MorphemeMean<-mean(sapply(1:nrow(LJGno0),function(x){sum(LJGno0[x,3:ncol(LJGno0)]!="")}))
MorphemeSD<-sd(sapply(1:nrow(LJGno0),function(x){sum(LJGno0[x,3:ncol(LJGno0)]!="")}))
