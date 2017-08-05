#' Finds the intersection between a line and a circumference
#' 
#' \code{IntersectLineCircle} finds the intesection between a line and a circumference
#' @param Line Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}
#' @param C Vector containing the xy-coordinates of the center of the circumference
#' @param r Radius for the circumference
#' @return Returns a vector containing the xy-coordinates of the intersection points. In case of no intersection, the function tells the user
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' Line <- CreateLinePoints(P1, P2)
#' C <- c(0,0)
#' r <- 2
#' intersection <- IntersectLineCircle(Line, C, r)
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' Draw(Line, "black")
#' Draw(CreateArcAngles(C, r, 0, 360), "black")
#' Draw(intersection[1,], "red")
#' Draw(intersection[2,], "red")
#' @export
IntersectLineCircle<-function(Line, C, r){
  m=Line[1]
  n=Line[2]
  a=1+m^2
  b=2*m*n-2*C[2]*m-2*C[1]
  c=C[1]^2+n^2+C[2]^2-2*C[2]*n-r^2
  if (sqrt(b^2-4*a*c)>=0){
    x1=(-b+sqrt(b^2-4*a*c))/(2*a)
    x2=(-b-sqrt(b^2-4*a*c))/(2*a)
    y1=m*x1+n
    y2=m*x2+n
    int=rbind(c(x1,y1),c(x2,y2))
    colnames(int)=c("X","Y")
  }
  else{
    int="The line and the circumference do not intersect"
  }
  return(int)
}