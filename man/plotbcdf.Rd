\name{plotbcdf}
\alias{plotbcdf}
\alias{plotbcdf.mecdf}
\alias{plotbcdf.matrix}
\alias{bcdf.plot}
\title{plots of bivariate cdfs}
\description{Functions for plotting bivariate CDFs in 3D. Currently, very limited options. More options are likely to be added to future versions. Warning 1: The function bcdf.plot is deprecated, do not use. Warning 2: Setting simple to false, may require a very large amount of computational time and space, except for very small samples.}
\usage{
plotbcdf (m, ...)
\method{plotbcdf}{mecdf}(m, simple=TRUE, res=16, ulim, vlim, \dots)
\method{plotbcdf}{matrix}(m, mmin=0, mmax=1, \dots)
bcdf.plot (...)
}
\arguments{
\item{m}{An ECDF model or a square matrix, representing points over a regularly space grid, that should in principle, be monotonically nondecreasing from top to bottom (plotted in the direction left-close to right-far) and from left to right (plotted in the direction right-close to left-far).}
\item{simple}{A logical. If true a square matrix representing points over a regularly spaced grid is created, evaluated and plotted as a discrete approximation of a smooth surface. If false a more realistic plot is created. This will either be a plot of a bivariate step function, or a continuous surface defined over an irregularly spaced grid, depending on whether or not the the model is a step function.}
\item{res}{Ignored unless simple is true. The number of points in both directions of the grid,}
\item{ulim}{Ignored unless simple is true. A vector giving the minimum and maximum values for the first variable on the grid. Defaults to the minimum and maximum observed values.}
\item{vlim}{Same as ulim, except for the second variable.}
\item{mmin}{Minimum value.}
\item{mmax}{Maximum value.}
\item{\dots}{Their just there.}
}

