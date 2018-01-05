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
  combinaciones=c()
  n=nrow(Poly)
  for (i in c(1:(n-1))){
    for (j in c((i+1):n)){
      if (i!=j){
        combinaciones=cbind(combinaciones,c(i,j))
      }
    }
  }  
  ### Search of collinear points
  vectores=c()
  for (i in c(1:ncol(combinaciones))){
    vectores=rbind(vectores,Poly[combinaciones[1,i],]-Poly[combinaciones[2,i],],
                           -Poly[combinaciones[1,i],]+Poly[combinaciones[2,i],])
  }
  if (length(duplicated(vectores))>0){
    print("Some of the inserted points are collinear. This could lead to a defective polygon.")
  }
  colnames(Poly)=c("X","Y")
  class(Poly) <- append(class(Poly),"Polygon")
  return(Poly)
}