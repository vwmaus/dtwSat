% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logisticWeight.R
\docType{methods}
\name{logisticWeight}
\alias{logisticWeight}
\title{Logistic weight function}
\usage{
logisticWeight(alpha, beta)
}
\arguments{
\item{alpha}{numeric. The steepness of logistic weight.}

\item{beta}{numeric. The midpoint of logistic weight.}
}
\value{
A \code{\link[base]{function}} object.
}
\description{
Builds a logistic time weight 
function to compute the TWDTW local cost matrix [1].
}
\details{
The linear \code{linearWeight} and \code{logisticWeight} weight functions 
can be passed to \code{\link[dtwSat]{twdtwApply}} through the argument \code{weight.fun}. 
This will add a time-weight to the dynamic time warping analysis. The time weight 
creates a global constraint useful to analyse time series with phenological cycles
of vegetation that are usually bound to seasons. In previous studies by 
\insertCite{Maus:2016;textual}{dtwSat} the logistic weight had better results than the 
linear for land cover classification. See \insertCite{Maus:2016;textual}{dtwSat} and 
\insertCite{Maus:2019;textual}{dtwSat}.
}
\examples{
log_fun = logisticWeight(alpha=-0.1, beta=100)
log_fun

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link[dtwSat]{twdtwApply}}
}
\author{
Victor Maus, \email{vwmaus1@gmail.com}
}
