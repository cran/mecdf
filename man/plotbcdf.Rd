\name{plotbcdf}
\alias{plotbcdf}
\alias{plotbcdf.mecdf}
\alias{plotbcdf.matrix}
\alias{bcdfplot}
\alias{bcdf.plot}
\title{surface plots of bivariate cdfs}
\description{Prototype functions for plotting bivariate CDFs using three dimensional graphics. Note that setting regular to false, can be very slow. Also note bcdfplot and bcdf.plot are deprecated.}
\usage{
plotbcdf (m, \dots)
\method{plotbcdf}{mecdf}(m, regular=TRUE, res=16, ulim, vlim, \dots)
\method{plotbcdf}{matrix}(m, mmin=0, mmax=1, \dots, labs)
bcdfplot (\dots)
bcdf.plot (\dots)
}
\arguments{
\item{m}{Either an mecdf object or a square matrix.}
\item{regular}{Logical (defaults to true), whether or not to use a regularly spaced grid. If false, the plot uses the actual data values.}
\item{res}{Ignored unless regular is true. The number of points in both directions of the grid.}
\item{ulim}{Ignored unless regular is true. A vector giving the minimum and maximum values for the first variable on the grid. Defaults to the minimum and maximum observed values.}
\item{vlim}{Same as ulim, except for the second variable.}
\item{mmin}{Minimum value.}
\item{mmax}{Maximum value.}
\item{labs}{.}
\item{\dots}{.}
}


