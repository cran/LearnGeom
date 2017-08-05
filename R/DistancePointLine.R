#' Computes the distance between a point and a line
#' 
#' \code{DistancePointLine} computes the distance between a point and a line
#' @param P Vector containing the xy-coordinates of a point
#' @param Line Vector object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}
#' @return Returns the distance between a point and a line. This distance corresponds to the distance between the point and its orthogonal projection into the line
#' @examples 
#' P <- c(2,1)
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' Line <- CreateLinePoints(P1, P2)
#' d <- DistancePointLine(P, Line)
#' @export
DistancePointLine<-function(P, Line){
  m=Line[1]
  n=Line[2]
  a=1
  b=-m
  c=-n
  d=abs(a*P[2]+b*P[1]+c)/sqrt(a^2+b^2)
  names(d)="distance"
  return(d)
}