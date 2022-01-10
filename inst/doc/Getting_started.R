## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,  comment = NA
)

## ---- eval=FALSE--------------------------------------------------------------
#  #install.packages("devtools")
#  devtools::install_github("cardiomoon/autoReg")

## -----------------------------------------------------------------------------
library(autoReg)

## -----------------------------------------------------------------------------
library(moonBook) # For use of example data acs
gaze(sex~.,data=acs)

## -----------------------------------------------------------------------------
library(dplyr) # for use of `%>%`
ft=gaze(sex~.,data=acs) %>% myft()
ft

## ----eval=FALSE---------------------------------------------------------------
#  library(rrtable)
#  
#  table2pptx(ft)

## ----eval=FALSE---------------------------------------------------------------
#  table2docx(ft)

## -----------------------------------------------------------------------------
gaze(sex+Dx~.,data=acs) %>% myft()

## -----------------------------------------------------------------------------
gaze(sex+DM+HBP~age,data=acs) %>% myft()

## -----------------------------------------------------------------------------
library(survival)   # For use of data colon
data(cancer)  

fit=glm(status~rx+sex+age+obstruct+perfor+nodes,data=colon,family="binomial")
summary(fit)

## -----------------------------------------------------------------------------
autoReg(fit)

## -----------------------------------------------------------------------------
autoReg(fit) %>% myft()

## -----------------------------------------------------------------------------
colon$status.factor=factor(colon$status,labels=c("Alive","Died"))
colon$obstruct.factor=factor(colon$obstruct,labels=c("No","Yes"))
colon$perfor.factor=factor(colon$perfor,labels=c("No","Yes"))
colon$sex.factor=factor(colon$sex,labels=c("Female","Male"))

fit=glm(status.factor~rx+sex.factor+age+obstruct.factor+perfor.factor+nodes,data=colon,family="binomial")
result=autoReg(fit) 
result %>% myft()

## -----------------------------------------------------------------------------
colon$status.factor=setLabel(colon$status.factor,"Mortality")
colon$rx=setLabel(colon$rx,"Treatment")
colon$age=setLabel(colon$age,"Age(Years)")
colon$sex.factor=setLabel(colon$sex.factor,"Sex")
colon$obstruct.factor=setLabel(colon$obstruct.factor,"Obstruction")
colon$perfor.factor=setLabel(colon$perfor.factor,"Perforation")
colon$nodes=setLabel(colon$nodes,"Positive nodes")

fit=glm(status.factor~rx+sex.factor+age+obstruct.factor+perfor.factor+nodes,data=colon,family="binomial")
result=autoReg(fit) 
result %>% myft()

## -----------------------------------------------------------------------------
shorten(result) %>% myft()

## -----------------------------------------------------------------------------
autoReg(fit, uni=TRUE) %>% myft()

## -----------------------------------------------------------------------------
autoReg(fit, uni=TRUE,threshold=1) %>% myft()

## -----------------------------------------------------------------------------
autoReg(fit, uni=TRUE,threshold=1, final=TRUE) %>% myft()

## -----------------------------------------------------------------------------
autoReg(fit, imputed=TRUE) %>% myft()

## ----fig.width=8,fig.height=7-------------------------------------------------
x=modelPlot(fit)
x

## ----fig.show='hide',eval=FALSE-----------------------------------------------
#  plot2pptx(print(x))

## ----fig.width=8,fig.height=7-------------------------------------------------
modelPlot(fit,uni=TRUE,threshold=1,show.ref=FALSE)

