## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA
)

## -----------------------------------------------------------------------------
library(autoReg)
library(dplyr)  # for use `%>%`
data(GBSG2,package="TH.data")
head(GBSG2)

## -----------------------------------------------------------------------------
GBSG2$cens.factor=factor(GBSG2$cens,labels=c("Alive","Died"))
fit=glm(cens.factor~horTh+pnodes+menostat,data=GBSG2,family="binomial")
summary(fit)

## -----------------------------------------------------------------------------
autoReg(fit) %>% myft()

## ----fig.width=8,fig.height=7-------------------------------------------------
modelPlot(fit)

## -----------------------------------------------------------------------------
newdata=expand.grid(horTh=factor(c(1,2),labels=c("no","yes")),
                    pnodes=1:51,
                    menostat=factor(c(1,2),labels=c("Pre","Post")))

## -----------------------------------------------------------------------------
df=bootPredict(fit,newdata,R=500)
head(df)

## ----fig.width=8,fig.height=7-------------------------------------------------
library(ggplot2)
ggplot(df,aes(x=pnodes,y=estimate))+
  geom_line(aes(color=horTh))+
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=horTh),alpha=0.2)+
  facet_wrap(~menostat)+
  theme_bw()+
  labs(x="Number of positive lymph nodes", y="Probability of death")

