\name{mecdf}
\alias{mecdf}
\alias{print.mecdf}
\alias{plot.mecdf}
\alias{s3x_print.mecdf}
\alias{s3x_plot.mecdf}
\title{multivariate ecdfs}
\description{The function mecdf, maps a data matrix to an object of class "mecdf", representing multivariate ECDF models. In the mecdf package, such models are also functions. In principle, the mecdf function's first argument is a numeric matrix, representing multivariate data. Each row, represents one multivariate realisation (we can think of this as either, the realised value of a vector random variable or the realised values of multiple random variables). Each column, represents one (random) variable. The mecdf function's second argument determines whether or not the model is a step function or a continuous function. As of mecdf 0.6.0, all models are step functions by default.}
\value{The model that's returned (reiterating the model's a function), maps a data matrix to a vector of cumulative probabilities. In principle, the function's only argument is a numeric matrix, with the same row and column conventions as above. It maps each row of the matrix to one value in the vector. Note that a regular vector can be used as an argument, however it's meaning is ambiguous. If the model is univariate, vector arguments are equivalent to matrices with one column. If the model is multivariate, vector arguments, are equivalent to a matrices with one row.}
\usage{
mecdf (x, continuous=FALSE, \dots,
    validate=TRUE, expand=continuous, project=FALSE, expandf=0.1)
\method{print}{mecdf}(x, \dots)
\method{plot}{mecdf}(x, \dots)
\method{s3x_print}{mecdf}(m, \dots)
\method{s3x_plot}{mecdf}(m, \dots)
}
\arguments{
\item{m}{An object of class "mecdf".}
\item{x}{In principle, a numeric matrix representing multivariate realisations, where each row represents one realisation and each column represents one variable. A vector can also be used, in which case its converted to a matrix, using cbind.}
\item{continuous}{Logical (defaults to false), if true a continuous function, otherwise a step function.}
\item{validate}{Logical (defaults to true), whether or not to validate the mecdf arguments. In general, this should be true.}
\item{expand}{Logical (default, true if continuous, false if step), whether or not to correct the model, by adding two extra points.}
\item{project}{Logical (defaults to false), if true, the matrix is transformed, such that the function's values fall within a unit cube with uniform marginals.}
\item{expandf}{Numeric (defaults to 0.1), giving the expansion factor. Ignored, if expand is false.}
\item{\dots}{.}
}

