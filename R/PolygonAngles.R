#' Computes each of the existing angles in a given polygon
#' 
#' \code{PolygonAngles} computes each of the existing angles in a given polygon
#' @param Poly Polygon object, previously created with function \code{CreatePolygon}
#' @return Returns a vector containing the angles for each of the points of a polygon. The resulting vector follows the order of the points in the defined polygon
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' P3 <- c(2,0) 
#' Poly <- CreatePolygon(P1, P2, P3)
#' angles <- PolygonAngles(Poly)
#' @export
PolygonAngles<-function(Poly){
  angles=c()
  aux=rep(c(1:nrow(Poly)),3)
  for (i in c(1:nrow(Poly))){
    A=Poly[aux[(nrow(Poly)+i-1)],]
    B=Poly[aux[(nrow(Poly)+i)],]
    C=Poly[aux[(nrow(Poly)+i+1)],]
    angle=Angle(A,B,C)
    angles=c(angles,angle)
  }
  names(angles)=c(1:nrow(Poly))
  return(angles)
}