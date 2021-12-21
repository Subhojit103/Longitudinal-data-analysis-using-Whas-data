library(survival)
library(survminer)
library(readxl)
df<- read_excel("D:/IIT bombay notes/semester 3/biostatistics/whas data.xls")
attach(df)
df
# Define variables
time= lenfol
censored= fstat

#cox proportional hazards
cox_regn= coxph(Surv(time,censored)~age+sex+lenstay+yrgrp+chf,data= df)
summary(cox_regn)
# Plot the baseline survival function
ggsurvplot(survfit(cox_regn), color = "#2E9FDF",
           ggtheme = theme_minimal(),data=df)

oldman<-data.frame(age=50,sex=0,lenstay=10,yrgrp=1, chf=1)
p2_predict<-survfit(cox_regn, newdata=oldman)
plot(p2_predict, main="Survival Curve for a 50year old man", xlab="t", ylab="S(t)")




cox_regn_1= coxph(Surv(time,censored)~age+sex+lenstay+chf+strata(yrgrp),data= df)
summary(cox_regn_1)

ggsurvplot(survfit(cox_regn_1), color = "#2E9FDF",
           ggtheme = theme_minimal(),data=df)

StayVSFol<-(lenstay==lenfol) +0
p4_cox<-coxph(Surv(lenfol, fstat)~age+sex+lenstay+StayVSFol+strata(yrgrp)+chf)
summary(p4_cox)
