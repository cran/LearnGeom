#' Computes the circumcenter of a given triangle, that is, the intersection of its three medians
#' 
#' \code{Circumcenter} computes the center of a triangle
#' @param Tri Triangle object, previously created with function \code{CreatePolygon}
#' @param lines Boolean. When \code{lines} = \code{TRUE}, the plot displays the lines that represent the medians of each of the sides of the triangle. If missing, it works as with \code{lines} = \code{FALSE}, so the lines are not displayed 
#' @return Vector which contains the xy-coordinates of the circumcenter of the triangle
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' P3 <- c(2,0) 
#' Tri <- CreatePolygon(P1, P2, P3)
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' Draw(Tri, "transparent")
#' I <- Circumcenter(Tri, lines = TRUE)
#' Draw(I, "red")
#' @references http://mathworld.wolfram.com/Circumcenter.html
#' @export
Circumcenter<-function(Tri, lines = F){
  A=Tri[1,]
  B=Tri[2,]
  C=Tri[3,]
  AB=B-A
  AB_orto=c(-AB[2],AB[1])
  AC=C-A
  AC_orto=c(-AC[2],AC[1])
  BC=C-B
  BC_orto=c(-BC[2],BC[1])
  AB_mid=MidPoint(A,B)
  AC_mid=MidPoint(A,C)
  BC_mid=MidPoint(B,C)
  AB1=AB_mid+AB_orto
  AC1=AC_mid+AC_orto
  BC1=BC_mid+BC_orto
  L1=CreateLinePoints(AB_mid,AB1)
  L2=CreateLinePoints(AC_mid,AC1)
  L3=CreateLinePoints(BC_mid,BC1)
  I=IntersectLines(L1,L2)
  if (lines == TRUE){
    DrawLine(L1,"red")
    DrawLine(L2,"red")
    DrawLine(L3,"red")
  }
  names(I)=c("X","Y")
  return(I)
}
