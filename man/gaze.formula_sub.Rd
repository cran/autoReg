% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gaze.R
\name{gaze.formula_sub}
\alias{gaze.formula_sub}
\title{Produce table for descriptive statistics}
\usage{
\method{gaze}{formula_sub}(x, data, missing = FALSE, ...)
}
\arguments{
\item{x}{An object of class "formula". Left side of ~ must contain the
name of one grouping variable or two grouping variables in an
additive way(e.g. sex+group~), and the right side of ~ must have
variables in an additive way.}

\item{data}{A data.frame}

\item{missing}{logical If true, missing value analysis performed}

\item{...}{Further arguments to be passed to gaze()}
}
\value{
An object of class "gaze" which inherits from the class "data.frame"
with at least the following attributes:
\describe{
\item{attr(*,"yvars)}{character. name of dependent variable}
}
}
\description{
Produce table for descriptive statistics by groups for several variables easily.
Depending on  the nature of these variables, different descriptive statistical
methods were used(t-test, ANOVA, Kruskal-Wallis, chi-squared, Fisher's,...)
}
