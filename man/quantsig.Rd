
\name{quantsig}
\alias{quantsig}
\title{Sigmoidal quantile function estimator}
\usage{
quantsig(x, p)
}
\arguments{
\item{x}{a numeric whose sample quantiles are wanted.}

\item{p}{the probability with values in (0,1).}
}
\value{The estimated quantile.}

\description{
This function implements the sigmoidal quantile function estimator, which is a smooth nonparametric quantile function estimator based on a newly defined generalized expectile function.
}

\examples{
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 8, 9)
quantsig(x, 0.5)
}
\references{
Hutson AD. The generalized sigmoidal quantile function.
Communications in Statistics-Simulation and Computation. 2024 Feb 1;53(2):799-813.
}
\author{
Alan Hutson, Han Yu
}
