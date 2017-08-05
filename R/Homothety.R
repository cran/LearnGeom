#' Creates an homothety from a given polygon
#' 
#' \code{Homothety} creates an homothety from a given polygon
#' @param Poly Polygon object, previously created with function \code{CreatePolygon}
#' @param C Vector containing the xy-coordinates of the center of the homothety
#' @param k Number which represents the expansion or contraction factor for the homothety
#' @param lines Boolean. When \code{lines} = \code{TRUE}, the plot displays the lines that connect the center of the homothety with the points of the polygons (the original and the transformed one). If missing, it works as with \code{lines} = \code{FALSE}, so the lines are not displayed 
#' @return Returns the coordinates of a polygon that has been transformed according to the homothethy with center at \code{C} and factor \code{k} 
#' @examples 
#' x_min <- -2
#' x_max <- 6
#' y_min <- -3
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' P3 <- c(2,0) 
#' Poly <- CreatePolygon(P1, P2, P3)
#' Draw(Poly, "blue")
#' C <- c(-1,-2)
#' k1 <- 0.5
#' Poly_homothety1 <- Homothety(Poly, C, k1, lines = TRUE)
#' Draw(Poly_homothety1, "orange")
#' k2 <- 2
#' Poly_homothety2 <- Homothety(Poly, C, k2, lines = TRUE)
#' Draw(Poly_homothety2, "orange")
#' @references  https://www.encyclopediaofmath.org/index.php/Homothety
#' @export
Homothety<-function(Poly, C, k, lines = F){
  xx=Poly[,1]
  yy=Poly[,2]
  xx_new=k*xx+(1-k)*C[1]
  yy_new=k*yy+(1-k)*C[2]
  Poly_new=cbind(xx_new,yy_new)
  colnames(Poly_new)=c("X","Y")
  class(Poly_new) <- append(class(Poly_new),"Polygon")
  if (lines == TRUE){
    for (i in c(1:nrow(Poly))){
      DrawLine(CreateLinePoints(C, Poly[i,]), "black")
    }
  }
  return(Poly_new)
}