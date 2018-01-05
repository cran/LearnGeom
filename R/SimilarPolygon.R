#' Creates a similar polygon to a given one
#' 
#' \code{SimilarPolygon} creates a sheared polygon from a given one
#' @param Poly Polygon object, previously created with function \code{CreatePolygon} or \code{CreateRegularPolygon}
#' @param k Positive number that represents the expansion (k > 1) or contraction (k < 1) factor which is applied to the original polygon
#' @return Returns a similar polygon, exapended or contracted, to the original polygon
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' P3 <- c(2,0) 
#' Poly <- CreatePolygon(P1, P2, P3)
#' Draw(Poly, "blue")
#' k <- 2
#' Poly_similar <- SimilarPolygon(Poly, k)
#' Draw(Translate(Poly_similar, c(-1,2)), "orange")
#' @export
SimilarPolygon<-function(Poly, k){
  xx=Poly[,1]
  yy=Poly[,2]
  xx_new=k*xx
  yy_new=k*yy
  Poly_new=cbind(xx_new,yy_new)
  colnames(Poly_new)=c("X","Y")
  class(Poly_new) <- append(class(Poly_new),"Polygon")
  return(Poly_new)
}