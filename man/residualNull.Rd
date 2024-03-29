% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residualNull.R
\name{residualNull}
\alias{residualNull}
\title{Make a residual plot of NULL model}
\usage{
residualNull(x, add.log = TRUE, type = "martingale")
}
\arguments{
\item{x}{An object of calss coxph}

\item{add.log}{logical If true, log of predictor varaibles are added}

\item{type}{character type of residuals}
}
\description{
Make a residual plot of NULL model
}
\examples{
library(survival)
data(pharmacoSmoking,package="asaur")
pharmacoSmoking$priorAttemptsT=pharmacoSmoking$priorAttempts
pharmacoSmoking$priorAttemptsT[pharmacoSmoking$priorAttemptsT>20]=20
x=coxph(Surv(ttr,relapse)~age+priorAttemptsT+longestNoSmoke,data=pharmacoSmoking)
residualNull(x)
}
