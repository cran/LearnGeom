#' Plots the Koch curve
#' 
#' \code{Koch} plots the first iterations of Koch curve, a well-known fractal
#' @param P1 Vector containing the xy-coordinates of point 1. This point is the left extreme of the segment that corresponds to the first iteration (\code{it} = 1)
#' @param P2 Vector containing the xy-coordinates of point 2. This point is the right extreme of the segment that corresponds to the first iteration (\code{it} = 1) 
#' @param it Number of iterations to be performed for the construction of Koch curve. It is not recommended to choose a number higher than 7 in order to avoid an excess of computation
#' @return None. It produces the plot of the first \code{n} iterations of Koch curve in the current coordinate plane
#' @examples 
#' x_min <- -6
#' x_max <- 6
#' y_min <- -4
#' y_max <- 8
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P1 <- c(-5,0)
#' P2 <- c(5,0)
#' it <- 4 
#' Koch(P1, P2, it)
#' @references  http://mathworld.wolfram.com/KochSnowflake.html
#' @export
Koch <- function(P1, P2, it){
  if (it==0){
    DrawSegmentPoints(P1, P2, "black")
  }
  if (it>0){
    P11=P1+1/3*(P2-P1)
    P12=P1+2/3*(P2-P1)
    xx=P11[1]
    yy=P11[2]
    x_fixed=P12[1]
    y_fixed=P12[2]
    xx_new=(xx-x_fixed)*cos(-pi/3)-(yy-y_fixed)*sin(-pi/3)+x_fixed
    yy_new=(xx-x_fixed)*sin(-pi/3)+(yy-y_fixed)*cos(-pi/3)+y_fixed
    P3=c(xx_new, yy_new)
    K1=Koch(P1, P11, it-1)
    K2=Koch(P11, P3, it-1)
    K3=Koch(P3, P12, it-1)
    K4=Koch(P12, P2, it-1)
  }
}
