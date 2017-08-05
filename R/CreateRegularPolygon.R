#' Creates a matrix to represent a regular polygon 
#' 
#' \code{CreateRegularPolygon} creates a matrix to represent the polygon that connects several points
#' @param n Number of sides for the polygon
#' @param C Vector containing the xy-coordinates for the center of the regular polygon
#' @param l Length of the sides for the polygon
#' @return Returns a matrix which contains the points of a regular polygon given its number of points and the length of its sides. Each row represents one of the points
#' @examples
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' n <- 5
#' C <- c(0,0) 
#' l <- 1
#' Penta <- CreateRegularPolygon(n, C, l)
#' Draw(Penta, "blue", label = TRUE)
#' @export
CreateRegularPolygon <- function(n, C, l){
  angle=(2*pi)/n
  Poly=c()
  for (i in c(1:n)){
    Poly=rbind(Poly,c(C[1]+l*sin(i*angle),C[2]+l*cos(i*angle)))
  }
  colnames(Poly)=c("X","Y")
  class(Poly) <- c("matrix","Polygon")
  if (n==4){
    Poly=Rotate(Poly,CenterPolygon(Poly),45)
  }
  return(Poly)
}