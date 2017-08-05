#' Computes the reflected point about a line of a given point
#' 
#' \code{ReflectedPoint} computes the reflected point about a line of a given point
#' @param P Vector containing the xy-coordinates of a point
#' @param Line Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}, to be used as the axis of symmetry
#' @return Returns a vector which contains the xy-coordinates of the reflected point
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
#' reflected <- ReflectedPoint(P, Line)
#' Draw(reflected, "red")
#' @export
ReflectedPoint<-function(P, Line){
  m=Line[1]
  n=Line[2]
  a=1
  b=-m
  c=-n
  p=P[1]
  q=P[2]
  num1=(p*(a^2-b^2)-2*b*(a*q+c))
  num2=(q*(b^2-a^2)-2*a*(b*p+c))
  den=a^2+b^2
  new_x=num1/den
  new_y=num2/den
  reflected=c(new_x,new_y)
  names(reflected)=c("X","Y")
  return(reflected)
}