#' Creates a vector to represent a line that passes through a point and forms certain angle with X axis
#' 
#' \code{CreateLineAngle} creates a vector to represent a line that passes through a point and forms certain angle with X axis
#' @param P Vector containing the xy-coordinates of a point
#' @param angle Angle in degrees (0-360) for the line
#' @return Returns a vector which contains the slope and intercept of the defined line. If the angle is defined as 90, the slope is set to \code{Inf} and the intercept is replaced by the x-value for the line (which is a vertical line in this situation)
#' @examples 
#' P <- c(0,0)
#' angle <- 45
#' Line <- CreateLineAngle(P, angle)
#' @export
CreateLineAngle<-function(P, angle){
  if (angle%%90==0){
    Line=c("Inf",P[1])
    names(Line)=c("slope","x-value")
  }
  else{
    m=tan(2*pi*angle/360)
    n=P[2]-m*P[1]
    Line=c(m,n)
    names(Line)=c("slope","intercept")
  }
  class(Line) <- append(class(Line),"Line")
  return(Line)
}
