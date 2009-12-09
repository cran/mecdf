\name{bcdf.plot}
\alias{bcdf.plot}
\title{bivariate cdf surface plot}
\description{This function takes a square matrix representing values over a regularly spaced grid and creates a surface plot, suitable for a bivariate cdf. In principle, the values should be monotonically nondecreasing from top to bottom (plotted in the direction left-close to right-far) and from left to right (plotted in the direction right-close to left-far). The surface is shaded, with colour corresponding to the value of the directional derivative in the direction center-close to center-far. Note that the name and arguments of this function may be changed considerably.}
\usage{
bcdf.plot (m, mmin=0, mmax=1)
}
\arguments{
\item{m}{A square matrix.}
\item{mmin}{Minimum value.}
\item{mmax}{Maximum value.}
}

