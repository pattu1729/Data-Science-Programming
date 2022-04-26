#LINEAR REGRESSION
bankloan=read.csv("bank-loan.csv",header=T)
str(bankloan)
bk=bankloan[1:700,]
riskmodel<-glm(default~age+employ+address+debtinc+othdebt,
               family=binomial,data=bk)
summary(riskmodel)
library(ROCR)
bk$predprob<-fitted(riskmodel)
pred<-prediction(bk$predprob,bk$default)
perf<-performance(pred,"tpr","fpr")
plot(perf)
