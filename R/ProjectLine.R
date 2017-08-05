#' Computes the orthogonal projection of a point onto a line 
#' 
#' \code{ProjectPoint} computes the orthogonal projection of a point onto a line 
#' @param P Vector containing the xy-coordinates of a point
#' @param Line Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}, to be used as the axis of symmetry
#' @return Returns a vector which contains the xy-coordinates of the projection point
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' xx <- c(0,1,2)
#' yy <- c(0,1,0)
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' Line <- CreateLinePoints(P1, P2)
#' Draw(Line, "black")
#' P <- c(-2,2)
#' Draw(P, "black")
#' projection <- ProjectPoint(P, Line)
#' Draw(projection, "red")
#' @export
ProjectPoint<-function(P, Line){
  m=Line[1]
  n=Line[2]
  v=c(-m,1)
  P1=P+v
  Line1=CreateLinePoints(P,P1)
  proj=IntersectLines(Line, Line1)
  names(proj)=c("X","Y")
  return(proj)
}