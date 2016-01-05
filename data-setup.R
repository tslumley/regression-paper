library(foreign)
library(survey)

demo<-read.xport("demo_c.xpt")[,c(1:8,28:31)]
bp<-read.xport("bpx_c.xpt")
bm<-read.xport("bmx_c.xpt")[,c("SEQN","BMXBMI")]
diet<-read.xport("dr1tot_c.xpt")[,c(1:52,63,64)]
nhanes34<-merge(demo,bp,by="SEQN")
nhanes34<-merge(nhanes34,bm,by="SEQN")
nhanes34<-merge(nhanes34,diet,by="SEQN")
demo5<-read.xport("demo_d.xpt")[,c(1:8,39:42)]
bp5<-read.xport("bpx_x.xpt")
bp5$BPXSAR<-rowMeans(bp5[,c("BPXSY1","BPXSY2","BPXSY3","BPXSY4")],
    na.rm=TRUE)
bp5$BPXDAR<-rowMeans(bp5[,c("BPXDI1","BPXDI2","BPXDI3","BPXDI4")],
    na.rm=TRUE)
bm5<-read.xport("bmx_d.xpt")[,c("SEQN","BMXBMI")]
diet5<-read.xport("dr1tot_d.xpt")[,c(1:52,64,65)]
nhanes56<-merge(demo5,bp5,by="SEQN")
nhanes56<-merge(nhanes56,bm5,by="SEQN")
nhanes56<-merge(nhanes56,diet5,by="SEQN")
nhanes<-rbind(nhanes34,nhanes56)
nhanes$fouryearwt<-nhanes$WTDRD1/2
write.csv(nhanes,file="combined-data.csv",row.names=FALSE)
des<-svydesign(id=~SDMVPSU,strat=~SDMVSTRA,weights=~fouryearwt,
   nest=TRUE, data=subset(nhanes, !is.na(WTDRD1)))
des<-update(des, sodium=DR1TSODI/1000, potassium=DR1TPOTA/1000)
des<-update(des, namol=sodium/23, kmol=potassium/39)
save(nhanes,des,file="combined-data.rda")

