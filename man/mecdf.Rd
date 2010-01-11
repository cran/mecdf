\name{mecdf}
\alias{mecdf}
\alias{print.mecdf}
\alias{plot.mecdf}
\title{multivariate ecdfs}
\description{The function mecdf takes a matrix as it's main argument, and returns a function, representing a univariate or multivariate ECDF. The rows of the matrix correspond to realisations (or observations) and the columns to random variables. It is possible to use a regular vector instead of a matrix, in which case it's converted to a matrix with one column. By definition ECDFs are step functions, however by setting continuous to true, it's possible to create continuous distribution functions, noting that continuous distribution functions generally require an expansion factor greater than zero. The function that is returned takes a single argument, a vector representing one new multivariate realisation (or point in the distribution function's domain), and returns the value of the distribution function at that point. Future versions are likely to allow a matrix argument. At present, evaluating multiple points, requires a loop. Note that these functions may change in future versions.}
\usage{
mecdf (x, continuous, expand=0.1, validate=TRUE, project=FALSE)
\method{print}{mecdf}(m, \dots)
\method{plot}{mecdf}(m, \dots)
}
\arguments{
\item{x}{A numeric matrix representing realisations of multiple random variables.}
\item{continuous}{A logical value. If true a continuous function, otherwise a step function. For univariate models, defaults to true, otherwise defaults to false.}
\item{expand}{Use NA to prevent expansion. Use any numeric value greater than zero to specify the expansion factor.}
\item{validate}{A logical indicating whether or not to validate the matrix. If true and there's any problem with the matrix, the function will stop.}
\item{project}{A logical indicating whether or not to project the model onto a unit cube.}
\item{m}{An ECDF model.}
\item{\dots}{When plotting bivariate ECDFs, one can include a col argument (for colour) and the arguments lower=TRUE or upper=TRUE, to add lines to the plot.}
}

