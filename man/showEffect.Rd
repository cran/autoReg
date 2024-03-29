% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showEffect.R
\name{showEffect}
\alias{showEffect}
\title{Show effects of covariates}
\usage{
showEffect(
  fit,
  x = NULL,
  color = NULL,
  facet = NULL,
  autovar = TRUE,
  pred.values = list(),
  se = TRUE,
  logy = TRUE,
  collabel = label_both,
  rowlabel = label_both
)
}
\arguments{
\item{fit}{An object of class survreg}

\item{x}{character name of x-axis variable}

\item{color}{character name of color variable}

\item{facet}{character name of facet variable}

\item{autovar}{logical Whether or not select color and facet variable automatically}

\item{pred.values}{list list of values of predictor variables}

\item{se}{logical whether or not show se}

\item{logy}{logical WHether or not draw y-axis on log scale}

\item{collabel}{labeller for column}

\item{rowlabel}{labeller for row}
}
\value{
A ggplot
}
\description{
Show effects of covariates
}
\examples{
library(survival)
library(ggplot2)
fit=survreg(Surv(time,status)~ph.ecog+sex*age,data=lung,dist="weibull")
showEffect(fit)
fit=survreg(Surv(time,status)~rx+sex+age+obstruct+adhere,data=colon,dist="weibull")
showEffect(fit)
showEffect(fit,rowlabel=label_value)
fit=survreg(Surv(time,status)~ph.ecog+sex,data=lung,dist="weibull")
showEffect(fit)
fit=survreg(Surv(time,status)~ph.ecog+age,data=lung,dist="weibull")
showEffect(fit)
fit=survreg(Surv(time,status)~ph.ecog+sex*age,data=lung,dist="weibull")
showEffect(fit,x="age",color="sex",facet="ph.ecog")
showEffect(fit,pred.values=list(age=c(50,60,70),ph.ecog=c(0,3),sex=c(1,2)),
  x="ph.ecog",color="sex",facet="age",autovar=FALSE)
fit=survreg(Surv(time,status)~age,data=lung,dist="weibull")
showEffect(fit)
}
