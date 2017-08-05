#' Computes the angle between three points
#' 
#' \code{Angle} computes the angle between three points
#' @param A Vector containing the xy-cooydinates of point A
#' @param B Vector containing the xy-cooydinates of point B. This point acts as the vertex of angle ABC
#' @param C Vector containing the xy-cooydinates of point C
#' @param label Boolean. When \code{label} = \code{TRUE}, the plot displays the angle in the point that acts as the vertex. If missing, it works as with \code{label} = \code{FALSE}, so the angle is not displayed 
#' @return Angle between the three points in degrees
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' A <- c(-1,0)
#' B <- c(0,0)
#' C <- c(0,1)
#' Draw(CreatePolygon(A, B, C), "transparent")
#' angle <- Angle(A, B, C, label = TRUE)
#' angle <- Angle(A, C, B, label = TRUE)
#' angle <- Angle(B, A, C, label = TRUE)
#' @export
Angle<-function(A, B, C, label=FALSE){
  vector1=c(A[1]-B[1],A[2]-B[2])
  vector2=c(C[1]-B[1],C[2]-B[2])
  num=(vector1[1]*vector2[1]+vector1[2]*vector2[2])
  den=sqrt(vector1[1]^2+vector1[2]^2)*sqrt(vector2[1]^2+vector2[2]^2)
  angle=acos(num/den)
  angle=(360*angle)/(2*pi)
  names(angle)="angle"
  if (label==TRUE){
    text(B[1],B[2],toString(round(angle,digits=1)),cex = 0.75)
  }
  return(angle)
}