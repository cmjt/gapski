% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit-gap.r
\name{fit.gap}
\alias{fit.gap}
\title{Fitting a model to a Matern void point process}
\usage{
fit.gap(points = NULL, lims = NULL, trunc = NULL, D.sv = NULL,
  D.bounds = NULL, trace = FALSE)
}
\arguments{
\item{points}{A matrix containing locations of observed points,
where each row corresponds to a point and each column corresponds
to a dimension.}

\item{lims}{A matrix with two columns, corresponding to the upper
and lower limits of each dimension, respectively.}

\item{trunc}{Truncation distance for the difference process.}

\item{D.bounds}{A named vector  specifying the bounds of the  parameter \code{D.bounds} in
optimisation.}

\item{trace}{Logical, if \code{TRUE}, parameter values are printed
to the screen for each iteration of the optimisation procedure.}

\item{sv}{A named vector with the starting values  for \code{D} in optimisation.}
}
\value{
An object with information that can be extracted via other
utility functions.
}
\description{
Fitting a model to a Matern void point process
}
