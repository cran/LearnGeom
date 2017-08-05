#' Creates a matrix to represent the polygon that connects several points
#' 
#' \code{CreatePolygon} creates a matrix to represent the polygon that connects several points
#' @param ... An undetermined number of points introduced by the user in the form of vectors
#' @return Returns a matrix which contains the points of the polygon. Each row represents one of the points
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' P3 <- c(2,0) 
#' Poly <- CreatePolygon(P1, P2, P3)
#' @export
CreatePolygon <- function(...){
  points_list <- list(...)
  Poly=c()
  for (i in c(1:length(points_list))){
    Poly=rbind(Poly,points_list[[i]])
  }
  colnames(Poly)=c("X","Y")
  class(Poly) <- append(class(Poly),"Polygon")
  return(Poly)
}