#' Computes the distance between two lines
#' 
#' \code{DistanceLines} computes the distance between two lines
#' @param Line1 Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}
#' @param Line2 Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}
#' @return Returns the distance between two points
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' Line1 <- CreateLinePoints(P1, P2)
#' P3 <- c(1,-1)
#' P4 <- c(2,0)
#' Line2 <- CreateLinePoints(P3, P4)
#' d <- DistanceLines(Line1, Line2)
#' @export
DistanceLines<-function(Line1, Line2){
  m1=Line1[1]
  m2=Line2[1]
  n1=Line1[2]
  if (m1==m2){
    P=c(0,n1)
    d=DistancePointLine(P,Line2)
  }
  else{
    d=0
  }
  names(d)="distance"
  return(d)
}