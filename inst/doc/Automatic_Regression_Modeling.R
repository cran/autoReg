## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA
)

## ---- eval=FALSE--------------------------------------------------------------
#  #install.packages("devtools")
#  devtools::install_github("cardiomoon/autoReg")

## -----------------------------------------------------------------------------
library(autoReg)
library(dplyr)

## -----------------------------------------------------------------------------
fit=lm(mpg~wt*hp*am,data=mtcars)
autoReg(fit) 

## -----------------------------------------------------------------------------
autoReg(fit) %>% myft()

## -----------------------------------------------------------------------------
autoReg(fit,uni=TRUE, threshold=0.2) %>% myft()

## -----------------------------------------------------------------------------
fit=lm(mpg~hp+wt+am+wt:hp+wt:am,data=mtcars)
final=step(fit,trace=0)
summary(final)

## -----------------------------------------------------------------------------
fit=lm(mpg~hp*wt*am,data=mtcars)
autoReg(fit,uni=TRUE,final=TRUE) %>% myft()

## -----------------------------------------------------------------------------
fit=lm(Sepal.Length~Sepal.Width*Species,data=iris)
autoReg(fit,uni=TRUE, final=TRUE) %>% myft()

## -----------------------------------------------------------------------------
df=gaze(Sepal.Length~Sepal.Width+Species,data=iris)
df %>% myft()

## -----------------------------------------------------------------------------
fit=lm(Sepal.Length~Sepal.Width+Species,data=iris)
df=addFitSummary(df,fit,"Coefficient (original data)")
df %>% myft()

## -----------------------------------------------------------------------------
iris1=iris
set.seed=123
no=sample(1:150,50,replace=FALSE)
iris1$Sepal.Width[no]=NA
gaze(Sepal.Width~.,data=iris1,missing=TRUE) %>% myft()

## -----------------------------------------------------------------------------
fit1=lm(Sepal.Length~Sepal.Width+Species,data=iris1)
df=addFitSummary(df,fit1,"Coefficient (missed data)")
df %>% myft()

## -----------------------------------------------------------------------------
fit2=imputedReg(fit1, m=20,seed=1234)
df=addFitSummary(df,fit2,statsname="Coefficient (imputed)")
df %>% myft()

## ----fig.width=8,fig.height=5-------------------------------------------------
modelPlot(fit1,imputed=TRUE)

