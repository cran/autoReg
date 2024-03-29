% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSigVars.R
\name{getSigVars}
\alias{getSigVars}
\title{Get explanatory variables of a model with significance level below the threshold}
\usage{
getSigVars(fit, threshold = 0.2, final = TRUE)
}
\arguments{
\item{fit}{An object of class lm or glm}

\item{threshold}{Numeric}

\item{final}{logical if true, perform stepwise regression using step()}
}
\value{
A list containing the following components:
\describe{
  \item{sigVars}{names of explanatory variables which have significant levels below the threshold in univariable model}
  \item{finalVars}{names of explanatory variables included in final model as a result of \code{\link[stats]{step}}}
}
}
\description{
Get explanatory variables of a model with significance level below the threshold
}
\examples{
library(survival)
data(cancer,package="survival")
fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
getSigVars(fit)
fit=lm(mpg~hp*wt+am,data=mtcars)
getSigVars(fit)
}
