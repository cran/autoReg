% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gaze_sub.R
\name{my.t.test2}
\alias{my.t.test2}
\title{Statistical test for continuous variables}
\usage{
my.t.test2(y, x, method = 1, all = FALSE)
}
\arguments{
\item{y}{a categorical vector}

\item{x}{a numeric vector}

\item{method}{method    An integer indicating methods for continuous variables. Possible values in methods are
1 forces analysis as normal-distributed
2 forces analysis as continuous non-normal
3 performs a Shapiro-Wilk test or nortest::ad.test to decide between normal or non-normal
Default value is 1.}

\item{all}{A logical}
}
\value{
A numeric vector of length 1
}
\description{
Statistical test for continuous variables
}
\examples{
library(moonBook)
y=acs$sex
x=acs$height
my.t.test2(y,x)
}
