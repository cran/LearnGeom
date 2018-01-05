#' Adds a point to a previously defined polygon
#' 
#' \code{AddPointPoly} creates a matrix to represent the polygon that connects several points
#' @param Poly Polygon object, previously created with function \code{CreatePolygon} or \code{CreateRegularPolygon}
#' @param point Vector containing the xy-coordinates of the point to be added to the polygon
#' @param position Integer indicating the position of the point in the original polygon, after which the new point is being added (considering that every polygon is an ordered list of points). It is convenient to visualize the polygon with \code{label = T} in order to avoid mistakes
#' @return Returns a matrix which contains the points of the polygon. Each row represents one of the points
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' n <- 5
#' C <- c(0,0)
#' l <- 2
#' Penta <- CreateRegularPolygon(n, C, l)
#' Penta <- AddPointPoly(Penta, CenterPolygon(Penta), 1)
#' Draw(Penta, "blue", label = TRUE)
#' @export
AddPointPoly <- function(Poly, point, position){
  Poly=rbind(Poly[1:position,],point,Poly[(position+1):nrow(Poly),])
  colnames(Poly)=c("X","Y")
  class(Poly) <- append(class(Poly),"Polygon")
  return(Poly)
} 
