% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DistancePoints.R
\name{DistancePoints}
\alias{DistancePoints}
\title{Computes the distance between two points}
\usage{
DistancePoints(P1, P2)
}
\arguments{
\item{P1}{Vector containing the xy-coordinates of point 1}

\item{P2}{Vector containing the xy-coordinates of point 2}
}
\value{
Returns the euclidean distance between two points
}
\description{
\code{DistancePoints} computes the distance between two points
}
\examples{
P1 <- c(0,0)
P2 <- c(1,1)
d <- DistancePoints(P1, P2)
}
