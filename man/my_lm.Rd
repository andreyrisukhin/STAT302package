% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_lm.R
\name{my_lm}
\alias{my_lm}
\title{Linear model}
\usage{
my_lm(formula, data)
}
\arguments{
\item{formula}{The formula object which describes what polynomial to fit.}

\item{data}{The data used to fit the polynomial. Expects a dataframe.}
}
\value{
A table describing the estimate, standard error, t and P values for
each coefficient.
}
\description{
This function fits a polynomial model to data, where the polynomial is defined
by the formula argument.
}
\examples{
my_lm(mpg ~ hp + wt, data = mtcars)

}
\keyword{inference}
\keyword{linear}
\keyword{model}
\keyword{prediction}
