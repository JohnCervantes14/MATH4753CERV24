% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myProject1.R
\name{ntickets}
\alias{ntickets}
\title{myProject1}
\usage{
ntickets(N, gamma, p)
}
\arguments{
\item{N}{the number of seats available}

\item{gamma}{the probability of being overbooked}

\item{p}{the probability of a show}
}
\value{
a list of the optimal tickets for normal and discrete distributions
}
\description{
myProject1
}
\examples{
ntickets(N=400, gamma=0.02, p=0.95)
}
