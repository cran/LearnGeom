% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IntersectLines.R
\name{IntersectLines}
\alias{IntersectLines}
\title{Finds the intersection of two lines}
\usage{
IntersectLines(Line1, Line2)
}
\arguments{
\item{Line1}{Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}}

\item{Line2}{Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}}
}
\value{
Returns a vector containing the xy-coordinates of the intersection point. In case of no intersection, the function tells the user
}
\description{
\code{IntersectLines} finds the intesection of two lines
}
\examples{
P1 <- c(0,0)
P2 <- c(1,1)
Line1 <- CreateLinePoints(P1, P2)
P3 <- c(1,-1)
P4 <- c(2,0)
Line2 <- CreateLinePoints(P3, P4)
intersection <- IntersectLines(Line1, Line2)
}
