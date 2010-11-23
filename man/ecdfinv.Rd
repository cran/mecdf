\name{ecdfinv}
\alias{ecdfinv}
\title{univariate continuous ecdf inverse}
\description{This function computes the inverse of a univariate continuous ECDF, with no model expansion. This function is also experimental. Unlike the mecdf function, it does not return a function, rather it returns a scalar value.}
\usage{
ecdfinv (x, p, sort=TRUE)
}
\arguments{
\item{x}{A numeric vector.}
\item{p}{Probability.}
\item{sort}{Whether or not to sort the data. Unless the data is already sorted, this must be true.}
}

