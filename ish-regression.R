library(survey)
library(splines)
load("combined-data.rda")
## define isolated systolic hypertension

des<-update(des, ish=(BPXSAR>140) & (BPXDAR<90))
#linear splines, units in decades
des<-update(des, age1=pmin(RIDAGEYR,50)/10, age2=pmin(pmax(RIDAGEYR,50),65)/10, age3=pmin(pmax(RIDAGEYR,65),90)/10)

ish0s <- svyglm(ish~age1+age2+age3, design=des,family=quasibinomial)	
ish1s<- svyglm(ish~age1+age2+age3+factor(RIDRETH1), design=des,family=quasibinomial)	
ish2s<- svyglm(ish~age1+age2+age3+RIAGENDR+factor(RIDRETH1), design=des,family=quasibinomial)	
ish3s<- svyglm(ish~(age1+age2+age3)*RIAGENDR+factor(RIDRETH1), design=des,family=quasibinomial)	
ish4s<- svyglm(ish~(age1+age2+age3)*RIAGENDR+factor(RIDRETH1)+sodium, design=des,family=quasibinomial)	


regTermTest(ish4s,~(age1+age2+age3):RIAGENDR,method="Wald")

##
## compare to unweighted analysis
##
##   - logistic regression
unwt<-glm(ish~(age1+age2+age3)*RIAGENDR+factor(RIDRETH1)+sodium,data=model.frame(des),family=binomial)

library(lme4)
## make a unique cluster identifer by combining psu and stratum
des<-update(des, realpsu=interaction(SDMVPSU,SDMVSTRA))
## logistic GLMM
unwtclus<-glmer(ish~(1|realpsu)+(age1+age2+age3)*RIAGENDR+factor(RIDRETH1)+sodium,data=model.frame(des),family=binomial)


round(cbind(coef(unwt),fixef(unwtclus),coef(ish4s)),2)
round(cbind(SE(unwt),sqrt(diag(as.matrix(vcov(unwtclus)))),SE(ish4s)),2)


#### compare weighted and unweighted analyses in linear model for dietary sodium
sodiumwt<-svyglm(sodium~age1+age2+age3+factor(RIDRETH1)*factor(RIAGENDR),design=des)
sodiumunwt<-glm(sodium~age1+age2+age3+factor(RIDRETH1)*factor(RIAGENDR),data=model.frame(des))
sodiumclus<-lmer(sodium~(1|realpsu)+age1+age2+age3+factor(RIDRETH1)*factor(RIAGENDR),data=model.frame(des))

betas<-round(cbind(coef(sodiumwt),coef(sodiumunwt),fixef(sodiumclus)),2)
ses<-round(cbind(SE(sodiumwt),SE(sodiumunwt),sqrt(diag(as.matrix(vcov(sodiumclus))))),3)

## convert from gender interaction into associations in men and in women
effects<-betas
effects[10:13,]<-effects[10:13,]+effects[5:8,]

## AIC, BIC
xtable(round(cbind(dAIC=AIC(ish0s,ish1s,ish2s,ish3s,ish4s)[,2], dBIC=BIC(ish0s,ish1s,ish2s,ish3s,ish4s,maximal=ish4s)[,2])))

## Stabilised weights
weightmodel<-glm(fouryearwt~(age1 + age2 + age3)*factor(RIAGENDR) + factor(RIDRETH1) ,data=model.frame(des),family=Gamma(log))

des_data<-model.frame(des)
des_data$stabilizedwt<-des_data$fouryearwt/fitted(weightmodel)

stabilized_des<-svydesign(id=~SDMVPSU,strata=~SDMVSTRA,weights=~stabilizedwt,data=des_data,nest=TRUE)

ish3stbl<- svyglm(ish~(age1+age2+age3)*RIAGENDR+factor(RIDRETH1), design=stabilized_des,family=quasibinomial)	

round(cbind(fixef(unwtclus),coef(ish4s),coef(ish4stbl)),2)
stderrs<-round(cbind(sqrt(diag(as.matrix(vcov(unwtclus)))),SE(ish4s),SE(ish4stbl)),2)

relative_var<-(stderrs[,-2]/stderrs[,2])^2
rownames(relative_var)<-c("Intercept","Age (<50)","Age (50-65)","Age (65+)","Gender", "Other Hispanic","Non-Hispanic Black","Non-Hispanic White","Other","Sodium intake","Age:gender (<50)","Age:gender (50-65)","Age:gender (65+)")

pdf("stabilized-var.pdf",height=4,width=6)
dotplot(relative_var,pch=c(1,19),xlab="Relative estimated variance")
dev.off()