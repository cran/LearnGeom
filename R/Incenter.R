#' Computes the incenter of a given triangle
#' 
#' \code{Incenter} computes the center of a triangle
#' @param Tri Triangle object, previously created with function \code{CreatePolygon}
#' @param lines Boolean. When \code{lines} = \code{TRUE}, the plot displays the lines that bisect each of the angles of the triangle. If missing, it works as with \code{lines} = \code{FALSE}, so the lines are not displayed 
#' @return Vector which contains the xy-coordinates of the incenter of the triangle
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
#' I <- Incenter(Tri, lines = TRUE)
#' Draw(I, "red")
#' @references http://mathworld.wolfram.com/Incenter.html
#' @export
Incenter<-function(Tri, lines = F){
  A=Tri[1,]
  B=Tri[2,]
  C=Tri[3,]
  AB=B-A
  AC=C-A
  BC=C-B
  bis1=0.5*(B-A)+0.5*(C-A)
  bis2=0.5*(A-B)+0.5*(C-B)
  bis3=0.5*(A-C)+0.5*(B-C)
  A1=A+bis1
  L1=CreateLinePoints(A,A1)
  B1=B+bis2
  L2=CreateLinePoints(B,B1)
  C1=C+bis3
  L3=CreateLinePoints(C,C1)
  I=IntersectLines(L1,L2)
  if (lines == TRUE){
    DrawLine(L1,"red")
    DrawLine(L2,"red")
    DrawLine(L3,"red")
  }
  names(I)=c("X","Y")
  return(I)
}