library(survey)
load("combined-data.rda")

dim(des)
dim(subset(des, !is.na(BPXDAR) & !is.na(RIDAGEYR)))

pdf("hexbp.pdf",height=5,width=5)
par(mar=c(5.1,4.1,1,1))
svyplot(BPXDAR~RIDAGEYR,style="hex",design=des,legend=0,xlab="Age (yrs)",ylab="Diastolic BP (mmHg)")
dev.off()

pdf("alphabp.pdf",height=5,width=5)
par(mar=c(5.1,4.1,1,1))
svyplot(BPXDAR~RIDAGEYR,style="trans",design=des,legend=0,xlab="Age (yrs)",ylab="Diastolic BP (mmHg)",pch=19,alpha=c(0,0.3))
dev.off()


pdf("thinbp1.pdf",height=5,width=5)
par(mar=c(5.1,4.1,1,1))
set.seed(2015-1-5)
svyplot(BPXDAR~RIDAGEYR,style="subsample",sample.size=1000,design=des,legend=0,xlab="Age (yrs)",ylab="Diastolic BP (mmHg)",ylim=c(0,120))
dev.off()

pdf("thinbp2.pdf",height=5,width=5)
par(mar=c(5.1,4.1,1,1))
svyplot(BPXDAR~RIDAGEYR,style="subsample",sample.size=1000,design=des,legend=0,xlab="Age (yrs)",ylab="Diastolic BP (mmHg)",ylim=c(0,120))
dev.off()

