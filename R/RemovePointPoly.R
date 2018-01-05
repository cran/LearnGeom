#' Removes a point from a previously defined polygon
#' 
#' \code{RemovePointPoly} creates a matrix to represent the polygon that connects several points
#' @param Poly Polygon object, previously created with function \code{CreatePolygon} or \code{CreateRegularPolygon}
#' @param position Integer indicating the position of the point in the original polygon that is being removed. It is convenient to visualize the polygon with \code{label = T} in order to avoid mistakes
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
#' Penta <- RemovePointPoly(Penta, 4)
#' Draw(Penta, "blue", label = TRUE)
#' @export
RemovePointPoly <- function(Poly, position){
  Poly=Poly[-position,]
  colnames(Poly)=c("X","Y")
  class(Poly) <- append(class(Poly),"Polygon")
  return(Poly)
} 
