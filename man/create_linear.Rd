% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_linear.R
\name{create_linear}
\alias{create_linear}
\title{Create a linear regression}
\usage{
create_linear(
  response = mtcars$mpg,
  covariates = data.frame(wt = mtcars$wt, drat = mtcars$drat),
  df = mtcars,
  intercept = 1
)
}
\arguments{
\item{response}{our dependent response variable; defaults to mtcars$mpg.}

\item{covariates}{our independent response variable or variables; defaults to covariates=data.frame(wt=mtcars$wt, drat=mtcars$drat).}

\item{df}{dataset that our variables are from; defaults to mtcars from dplyr package.}

\item{intercept}{Intercept (set to 1) or no intercept (set to 0) linear regression.}
}
\value{
the beta estimates, se, t_values, p_values
}
\description{
Create a linear regression
}
\examples{
create_linear(response=mtcars$mpg, covariates=data.frame(wt=mtcars$wt, drat=mtcars$drat), df=mtcars, intercept=1)
}
