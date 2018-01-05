#' Creates the reflection about a line of a given polygon
#' 
#' \code{ReflectedPolygon} creates the reflection about a line of a given polygon
#' @param Poly Polygon object, previously created with function \code{CreatePolygon} or \code{CreateRegularPolygon}
#' @param Line Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}, to be used as the axis of symmetry
#' @return Returns the reflection of a polygon about a line 
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
#' P1 <- c(-3,2)
#' P2 <- c(1,-4)
#' Line <- CreateLinePoints(P1, P2)
#' Draw(Line, "black")
#' Poly_reflected <- ReflectedPolygon(Poly, Line)
#' Draw(Poly_reflected, "orange")
#' @export
ReflectedPolygon<-function(Poly, Line){
  xx=Poly[,1]
  yy=Poly[,2]
  xx_new=c()
  yy_new=c()
  for (i in c(1:nrow(Poly))){
    P=c(xx[i],yy[i])
    reflected=ReflectedPoint(P,Line)
    xx_new=c(xx_new,reflected[1])
    yy_new=c(yy_new,reflected[2])
  }
  Poly_new=cbind(xx_new,yy_new)
  colnames(Poly_new)=c("X","Y")
  class(Poly_new) <- append(class(Poly_new),"Polygon")
  return(Poly_new)
}
