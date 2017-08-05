#' Computes the distance between two points 
#' 
#' \code{DistancePoints} computes the distance between two points 
#' @param P1 Vector containing the xy-coordinates of point 1
#' @param P2 Vector containing the xy-coordinates of point 2
#' @return Returns the euclidean distance between two points
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' d <- DistancePoints(P1, P2)
#' @export
DistancePoints<-function(P1, P2){
  d=sqrt((P1[1]-P2[1])^2+(P1[2]-P2[2])^2)
  names(d)="distance"
  return(d)
}