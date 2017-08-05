#' Computes the middle point of the segment that connects two points
#' 
#' \code{MidPoint} computes the middle point of the segment that connects two points
#' @param P1 Vector containing the xy-coordinates of point 1
#' @param P2 Vector containing the xy-coordinates of point 2
#' @return Returns a vector containing the xy-coordinates of the middle point of the segment that connects \code{P1} and \code{P2}
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' mid <- MidPoint(P1, P2)
#' @export
MidPoint<-function(P1, P2){
  x_mid=(P1[1]+P2[1])/2
  y_mid=(P1[2]+P2[2])/2
  return(c(x_mid,y_mid))
}