#' Creates a vector that represents the line that connects two points 
#' 
#' \code{CreateLinePoints} creates a vector that represents the line that connects two points 
#' @param P1 Vector containing the xy-coordinates of point 1
#' @param P2 Vector containing the xy-coordinates of point 2
#' @return Returns a vector which contains the slope and intercept of the defined line. If the points have the same x-coordinate, the slope is set to \code{Inf} and the intercept is replaced by the x-value for the line (which is a vertical line in this situation)
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' Line <- CreateLinePoints(P1, P2)
#' @export
CreateLinePoints<-function(P1, P2){
  if (P1[1]==P2[1]){
    Line=c("Inf",P1[1])
    names(Line)=c("slope","x-value")
  }
  else{
    m=(P2[2]-P1[2])/(P2[1]-P1[1])
    n=P1[2]-m*P1[1]
    Line=c(m,n)
    names(Line)=c("slope","intercept")
  }
  class(Line) <- append(class(Line),"Line")
  return(Line)
}