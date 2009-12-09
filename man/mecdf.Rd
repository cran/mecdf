\name{mecdf}
\alias{mecdf}
\alias{print.mecdf}
\alias{plot.mecdf}
\title{multivariate ecdfs}
\description{The function mecdf takes some data and returns a function representing a multivariate ECDF. The function returned is not vectorised properly, it takes a vector representing a single possible value of a vector random variable and evaluates the distribution function at that point. These functions are still experimental, and are not tested properly.}
\usage{
mecdf (x)
\method{print}{mecdf}(m, \dots)
\method{plot}{mecdf}(m, \dots)
}
\arguments{
\item{x}{A matrix representing realisations of a vector random variable. One row per realisation, one column for each component of the random variable.}
\item{m}{An mecdf function.}
\item{\dots}{Their just there.}
}

