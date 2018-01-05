#' Creates a sheared polygon from a given one
#' 
#' \code{ShearedPolygon} creates a sheared polygon from a given one
#' @param Poly Polygon object, previously created with function \code{CreatePolygon} or \code{CreateRegularPolygon}
#' @param k Number that represents the shear factor which is applied to the original polygon
#' @param direction String with value "horizontal" or "vertical" which indicates the direction in which shearing is applied. Horizontal means the shearing is parallel to the X axis, while vertical means parallel to the Y axis
#' @return Returns a sheared polygon, in any of the two axis, to the original one
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' Square <- CreateRegularPolygon(4, c(-2, 0), 1)
#' Draw(Square, "blue")
#' k <- 1
#' Square_shearX <- Translate(ShearedPolygon(Square, k, "horizontal"), c(3,0))
#' Draw(Square_shearX, "orange")
#' Square_shearY <- Translate(ShearedPolygon(Square, k, "vertical"), c(3,0))
#' Draw(Square_shearY, "orange")
#' @export
ShearedPolygon<-function(Poly, k, direction){
  xx=Poly[,1]
  yy=Poly[,2]
  if (direction=="horizontal"){
    xx_new=xx+k*yy
    yy_new=yy
    Poly_new=cbind(xx_new,yy_new)
    colnames(Poly_new)=c("X","Y")
    class(Poly_new) <- append(class(Poly_new),"Polygon")
  } else if (direction=="vertical"){
    xx_new=xx
    yy_new=k*xx+yy
    Poly_new=cbind(xx_new,yy_new)
    colnames(Poly_new)=c("X","Y")
    class(Poly_new) <- append(class(Poly_new),"Polygon")
  }
  return(Poly_new)
}