% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_knn_cv.R
\name{my_knn_cv}
\alias{my_knn_cv}
\title{my_knn_cv}
\usage{
my_knn_cv(train, cl, k_nn, k_cv)
}
\arguments{
\item{train}{Input data frame to be validated}

\item{cl}{true class value of the training data}

\item{k_nn}{integer representing the number of neighbors}

\item{k_cv}{integer representing the number of folds}
}
\value{
a list with objects \code{output} .
}
\description{
This function performs a k-Nearest Neighbors Cross-Validation in R
}
\examples{

 my_knn_cv(train = na.omit(STAT302package::my_penguins)[, 3:6]
                        , cl = na.omit(STAT302package::my_penguins)$species,  k_nn = 5, k_cv = 5)
}
\keyword{Cross-Validation}
\keyword{Function}
\keyword{K-Nearest}
\keyword{Neighbors}
