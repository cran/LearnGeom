#' Creates a tessellation from a starting set of geometric objects 
#' 
#' \code{Tessellation} creates a geometric pattern by the repetitive translation of an initial geometric object 
#' @param objects_list A list composed by several geometric objects (mainly polygons created with \code{CreatePolygon} or \code{CreateRegularPolygon})
#' @param colors Vector containing the colors for each of the objects of the initial geometric object
#' @param direction Vector containing the xy-coordinates of the direction in which tessellation is being generated  
#' @param separation Number indicating the distance that separates any of the geometric objects in the repetitive pattern. This distance must be understood in the sense of a translation of the initial object. Indeed, this distance is only preserved in the direction of the chosen vector \code{direction} when generating the pattern. Moreover, the choice of \code{separation = 0} implies no pattern is generated 
#' @param it Number of iterations to be performed for the construction of the tessellation
#' @return None. It produces the plot of a repetitive pattern, usually known as a tessellation
#' @examples 
#' x_min <- -6
#' x_max <- 6
#' y_min <- -2
#' y_max <- 10
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' Hexa <- CreateRegularPolygon(6, c(-3,0), 1)
#' Draw(Hexa, "purple")
#' Tri <- CreatePolygon(c(-3,-1), c(Hexa[4,1],-2), c(Hexa[1,1],-2))
#' Draw(Tri,"pink")
#' objects_list <- list(Tri, Hexa)
#' cols <- c("pink", "purple")
#' direction <- c(1,0)
#' separation <- 1.732051 
#' it <- 3
#' Tessellation(objects_list, cols, direction, separation, it)
#' direction <- c(0,1)
#' separation <- 3
#' it <- 4
#' Tessellation(objects_list, cols, direction, separation, it)
#' @references  http://mathworld.wolfram.com/Tessellation.html
#' @export
Tessellation <- function(objects_list, colors, direction, separation, it){
  if (direction[1]==0){
    ang=pi/2
  }
  else{
    m=direction[2]/direction[1]
    ang=atan(m)
    ang=(180*ang)/pi
  }
  for (i in c(1:it)){
    for (j in c(1:length(objects_list))){
      Poly=objects_list[[j]]
      DrawPolygon(Translate(Poly, i*separation*c(cos(ang),sin(ang))),colors[j])
    }
  }
}


