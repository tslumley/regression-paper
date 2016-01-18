library(survey)

## continuous data: Chapter 7, Volume 1, Breslow & Day
## http://faculty.washington.edu/norm/datasets.html
## control weight of 441 estimated from population size
tuyns<-read.table("tuynsc.txt",col.names=c("case","age","agegp","tobgp","tobacco","logtb","beer","cider",'wine',"aperitif","digestif","alcohol","logalc"))
tuyns$wt<-ifelse(tuyns$case==1,1,441)
tuyns<-subset(tuyns,  (tobacco!=99))
tdes<-svydesign(id=~1,strata=~case,data=tuyns,weight=~wt)

## Grouped data. Same original source, but now in data(esoph)
data(esoph)
options(contrasts=c("contr.treatment","contr.treatment"))
esophcase<-esoph[rep(1:88,esoph$ncases),1:3]
esophctrl<-esoph[rep(1:88,esoph$ncontrols),1:3]
esophcase$status<-1
esophctrl$status<-0
esophcase$wt<-1
esophctrl$wt<-441
esophlong<-rbind(esophcase,esophctrl)
des<-svydesign(id=~1,weights=~wt,data=esophlong)

##
## model fitting
grouped1s<-svyglm(status~agegp+alcgp+tobgp,family=binomial,design=des)
grouped2s<-svyglm(status~agegp+alcgp+tobgp+as.numeric(alcgp):as.numeric(tobgp),family=binomial,design=des)

grouped1<-glm(status~agegp+alcgp+tobgp,family=binomial,data=esophlong)
grouped2<-glm(status~agegp+alcgp+tobgp+as.numeric(alcgp):as.numeric(tobgp),family=binomial,data=esophlong)


cont1s<-svyglm(case~age+I(age^2)+alcohol+tobacco,family=binomial,design=tdes)
cont2s<-svyglm(case~age+I(age^2)+alcohol*tobacco,family=binomial,design=tdes)

cont1<-glm(case~age+I(age^2)+alcohol+tobacco,family=binomial,data=tuyns)
cont2<-glm(case~age+I(age^2)+alcohol*tobacco,family=binomial,data=tuyns)

##
SE(grouped1)/SE(grouped1s)
SE(grouped2)/SE(grouped2s)
SE(cont1)/SE(cont1s)
SE(cont2)/SE(cont2s)

pdf("ille-et-vilaine.pdf",height=5,width=7)
par(mar=c(5.1,4.1,1,1))
dotchart(c(SE(grouped1s)/SE(grouped1), NA, SE(cont1s)/SE(cont1))^2,pch=rep(c(1,19),c(12,6)),xlab="Estimated variance increase with weighting")
dev.off()