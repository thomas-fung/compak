% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compak_KLbandwidth.R
\name{compak_KLbandwidth}
\alias{compak_KLbandwidth}
\title{Calculates the optimal bandwidth via minimising Kullback-Leibler divergence}
\usage{
compak_KLbandwidth(a.sample, x = NULL, interval = c(0.025, 1), workers = 1L)
}
\arguments{
\item{a.sample}{numeric vector; the data sample from which the estimate is to be computed.}

\item{x}{a numeric vector: the points of the grid at which the density is to be estimated. This vector should be considerable longer than the range of the a.sample. If NULL, the function will populate the vector for you using a Chebyshev's Inequality type argument.}

\item{interval}{numeric vector; the end-points of the interval to be searched for the best bandwidth.}

\item{workers}{numeric; a positive integer to represent the number of cores used for parallel processing to evaluate the compak smoother.}
}
\value{
The optimal bandwidth parameter within the \code{interval}
}
\description{
Calculates the optimal bandwidth via minimising Kullback-Leibler divergence
}
\examples{
data(days)
h.KL <- compak_KLbandwidth(days)
}
