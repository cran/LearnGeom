#' Creates a matrix that represents the segment that connects two points 
#' 
#' \code{DrawSegment} plots the segment that connects two points in a previously generated coordinate plane
#' @param P1 Vector containing the xy-coordinates of point 1
#' @param P2 Vector containing the xy-coordinates of point 2
#' @return Returns a matrix which contains the points that determine the extremes of the segment
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' Segment <- CreateSegmentPoints(P1, P2)
#' Draw(Segment, "black")
#' @export
CreateSegmentPoints<-function(P1, P2){
  Segment=rbind(P1,P2)
  colnames(Segment)=c("X","Y")
  class(Segment) <- append(class(Segment),"Segment")
  return(Segment)
}