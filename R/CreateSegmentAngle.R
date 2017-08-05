#' Creates a matrix that represents the segment that starts from a point with certain length and angle 
#' 
#' \code{DrawSegment} plots the segment that connects two points in a previously generated coordinate plane
#' @param P Vector containing the xy-coordinates of the point
#' @param l Positive number that indicates the length for the segment 
#' @param angle Angle in degrees (0-360) for the segment
#' @return Returns a matrix which contains the points that determine the extremes of the segment
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P <- c(0,0)
#' angle <- 30
#' l <- 1
#' Segment <- CreateSegmentAngle(P, angle, l)
#' Draw(Segment, "black")
#' @export
CreateSegmentAngle<-function(P, angle, l){
  P1=P
  angle=(pi*angle)/180
  P2=P+c(l*cos(angle), l*sin(angle))
  Segment=CreateSegmentPoints(P1,P2)
  return(Segment)
}