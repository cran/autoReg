## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message=FALSE
)

## ---- eval=FALSE--------------------------------------------------------------
#  #install.packages("devtools")
#  devtools::install_github("cardiomoon/autoReg")

## -----------------------------------------------------------------------------
library(autoReg)
library(survival)
library(dplyr)

## -----------------------------------------------------------------------------
data(melanoma,package="boot")
gaze(melanoma)

## -----------------------------------------------------------------------------
melanoma$status1 = ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
melanoma$statusCRR=factor(melanoma$status1,labels=c("survived","melanoma","other cause"))
melanoma$sex=factor(melanoma$sex,labels=c("Female","Male"))
melanoma$ulcer=factor(melanoma$ulcer,labels=c("Absent","Present"))

## -----------------------------------------------------------------------------
gaze(melanoma,show.n=TRUE)

## -----------------------------------------------------------------------------
fit=survfit(Surv(time/365.25,status!=2)~1,data=melanoma)
fit

## -----------------------------------------------------------------------------
summary(fit,times=0:5)

## ---- fig.height=4,fig.width=6------------------------------------------------
library(survminer)
ggsurvplot(fit)

## -----------------------------------------------------------------------------
fit=coxph(Surv(time,status!=2)~age+sex+thickness+ulcer,data=melanoma)

## -----------------------------------------------------------------------------
x=autoReg(fit,uni=TRUE,threshold=1)
x %>% myft()

## -----------------------------------------------------------------------------
x$name=c("Age(years)","Sex","","Tumor thickness (mm)","Ulceration","")
names(x)[1]="Overall Survival"
x %>% myft()

## ----fig.width=8,fig.height=5-------------------------------------------------
modelPlot(fit,uni=TRUE, threshold=1,show.ref=FALSE)

## -----------------------------------------------------------------------------
final=coxph(Surv(time,status!=2)~age+thickness+ulcer,data=melanoma)
x=x %>% addFitSummary(final,statsname="HR (final)") 
x %>% myft()

## -----------------------------------------------------------------------------
result=cox.zph(fit)
result

## ----fig.width=8,fig.height=6-------------------------------------------------
coxzphplot(result)

## -----------------------------------------------------------------------------
fit=coxph(Surv(time,status==1)~age+sex+thickness+ulcer,data=melanoma)
autoReg(fit,uni=TRUE,threshold=1) %>% myft()

## -----------------------------------------------------------------------------
fit1=crrFormula(time+status1~age+sex+thickness+ulcer,data=melanoma)
autoReg(fit,uni=TRUE,threshold=1) %>% 
  addFitSummary(fit1,"HR (competing risks multivariable)") %>% 
  myft()

## -----------------------------------------------------------------------------
library(cmprsk) # for use of cuminc()
melanoma$years=melanoma$time/365
fit=cuminc(melanoma$years,melanoma$status1)
fit

## ----fig.width=8, fig.height=5------------------------------------------------
ggcmprsk(years+status1~1,data=melanoma,id=c("alive","melanoma","others"),se=TRUE)

## ----fig.width=8, fig.height=5------------------------------------------------
ggcmprsk(years+status1~sex,data=melanoma,id=c("alive","melanoma","others"),
         strata=c("female","male"))

## -----------------------------------------------------------------------------
data(retinopathy, package="survival")
fit=coxph(Surv(futime,status)~trt*type+frailty(id),data=retinopathy)
summary(fit)

## -----------------------------------------------------------------------------
autoReg(fit) %>% myft()

## -----------------------------------------------------------------------------
fit=coxph(Surv(futime,status)~trt*type+cluster(id),data=retinopathy)
summary(fit)

## -----------------------------------------------------------------------------
autoReg(fit) %>% myft()

