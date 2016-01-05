library(survey)
load("combined-data.rda")

## remove zero diastolic BP
des<-subset(des, BPXDAR>0)

pdf("meanbp.pdf",height=5,width=5)
par(mar=c(5.1,4.1,1,1))
men<-svysmooth(BPXDAR~RIDAGEYR,design=subset(des, RIAGENDR==1),bandwidth=10)
women<-svysmooth(BPXDAR~RIDAGEYR,design=subset(des, RIAGENDR==2),bandwidth=10)
plot(men,ylim=c(40,80),ylab="Diastolic BP (mmHg)",xlab="Age (yrs)")
lines(women,lty=2)
legend("bottomright",lty=1:2,bty="n",legend=c("Men","Women"))
dev.off()

pdf("quantilebp.pdf",height=5,width=5)
par(mar=c(5.1,4.1,1,1))
median<-svysmooth(BPXDAR~RIDAGEYR,design=des,method="quantreg",quantile=0.5)
plot(median,ylim=c(30,90),ylab="Diastolic BP (mmHg)",xlab="Age (yrs)",lwd=2)
for(qi in c(.25,.75)){
	quartile <- svysmooth(BPXDAR~RIDAGEYR,design=des,method="quantreg",quantile=qi)
	lines(quartile,lwd=1)
}
for(qi in c(.1,.9)){
	decile <- svysmooth(BPXDAR~RIDAGEYR,design=des,method="quantreg",quantile=qi)
	lines(decile,lwd=1,lty=3)
}
legend("bottomright",lty=c(1,1,3),lwd=c(2,1,1),bty="n",legend=c("Median","Quartiles","10% and 90%"))
dev.off()
