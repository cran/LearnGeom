#' Translates a geometric object
#' 
#' \code{Translate} translates a geometric object of any of the following types: line, polygon or segment
#' @param object geometric object, previously created with function \code{CreatePolygon}
#' @param v Vector containing the xy-coordinates of the translation vector 
#' @return Returns a polygon whose coordinates are translated according to vector \code{v}
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
#' v <- c(1,2)
#' Poly_translated <- Translate(Poly, v)
#' Draw(Poly_translated, "orange")
#' @export
Translate<-function(object, v){
  xx_new=object[,1]+v[1]
  yy_new=object[,2]+v[2]
  object_new=cbind(xx_new,yy_new)
  colnames(object_new)=c("X","Y")
  class(object_new) <- append(class(object_new),class(object)[2])
  return(object_new)
}
