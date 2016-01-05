library(survey)
load("combined-data.rda")

dim(des)
dim(subset(des, !is.na(BPXDAR) & !is.na(RIDAGEYR)))

pdf("hexbp.pdf")
svyplot(BPXDAR~RIDAGEYR,style="hex",design=des,legend=0,xlab="Age (yrs)",ylab="Diastolic BP (mmHg)")
dev.off()

pdf("alphabp.pdf",height=5,width=5)
svyplot(BPXDAR~RIDAGEYR,style="trans",design=des,legend=0,xlab="Age (yrs)",ylab="Diastolic BP (mmHg)",pch=19)
dev.off()
