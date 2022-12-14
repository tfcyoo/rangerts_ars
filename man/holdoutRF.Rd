% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/holdoutRF.R
\name{holdoutRF}
\alias{holdoutRF}
\title{Hold-out random forests}
\usage{
holdoutRF(...)
}
\arguments{
\item{...}{All arguments are passed to \code{\link{rangerts}()} (except \code{importance}, \code{case.weights}, \code{replace} and \code{holdout}.).}
}
\value{
Hold-out random forests with variable importance.
}
\description{
Grow two random forests on two cross-validation folds.
Instead of out-of-bag data, the other fold is used to compute permutation importance.
Related to the novel permutation variable importance by Janitza et al. (2015).
}
\references{
Janitza, S., Celik, E. & Boulesteix, A.-L., (2015). A computationally fast variable importance test for random forests for high-dimensional data. Adv Data Anal Classif \url{https://doi.org/10.1007/s11634-016-0276-4}. \cr
}
\seealso{
\code{\link{rangerts}}
}
\author{
Marvin N. Wright
}
