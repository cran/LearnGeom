#' Plots the Sierpinski triangle
#' 
#' \code{Sierpinski} plots the first iterations of Sierpinski triangle, a well-known fractal
#' @param Tri Regular triangle, previously created with function \code{CreateRegularPolygon}
#' @param it Number of iterations to be performed for the construction of Sierpinski triangle. It is not recommended to choose a number higher than 10 in order to avoid an excess of computation
#' @return None. It produces the plot of the first \code{n} iterations of Sierpinski triangle in the current coordinate plane
#' @examples 
#' x_min <- -6
#' x_max <- 6
#' y_min <- -6
#' y_max <- 6
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' n <- 3
#' C <- c(0,0)
#' l <- 5
#' Tri <- CreateRegularPolygon(n, C, l)
#' it <- 6
#' Sierpinski(Tri, it)
#' @references  http://mathworld.wolfram.com/SierpinskiSieve.html
#' @export
Sierpinski <- function(Tri, it){
  if (it==0){
  DrawPolygon(Tri,"black")
  }
  if (it>0){
    T1=Sierpinski(rbind(Tri[1,],MidPoint(Tri[1,],Tri[2,]),MidPoint(Tri[1,],Tri[3,])), it-1)
    T2=Sierpinski(rbind(Tri[2,],MidPoint(Tri[1,],Tri[2,]),MidPoint(Tri[2,],Tri[3,])), it-1)
    T3=Sierpinski(rbind(Tri[3,],MidPoint(Tri[1,],Tri[3,]),MidPoint(Tri[2,],Tri[3,])), it-1)
  }
}