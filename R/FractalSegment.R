#' Plots a fractal curve starting from a segment 
#' 
#' \code{FractalSegment} plots the first iterations of a fractal curve, starting from a segment in the plane
#' @param P1 Vector containing the xy-coordinates of point 1. This point is the left extreme of the segment that corresponds to the first iteration (\code{it} = 1)
#' @param P2 Vector containing the xy-coordinates of point 2. This point is the right extreme of the segment that corresponds to the first iteration (\code{it} = 1)
#' @param angle Angle (0-360) that determines the angle with which the new segments are drawn at the cut points
#' @param cut1 Number bigger than 0 and smaller than 1 that indicates the proportional part of the segment at which the first cut occurs. This parameter determines the position of the first cut point
#' @param cut2 Number bigger than 0 and smaller than 1 that indicates the proportional part of the segment at which the second cut occurs. This parameter determines the position of the second cut point
#' @param f Positive number that produces an enlargement or a reduction for the new drawn segment in each iteration
#' @param it Number of iterations to be performed for the construction of the fractal curve. It is not recommended to choose a number higher than 7 in order to avoid an excess of computation
#' @return None. It produces the plot of the first \code{n} iterations of a fractal curve in the current coordinate plane. The choice of parameters \code{cut1 = 1/3}, \code{cut2 = 2/3}, \code{angle = 60} and \code{f = 1} produces the Koch curve
#' @examples 
#' x_min <- -6
#' x_max <- 6
#' y_min <- -4
#' y_max <- 8
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P1 <- c(-5,0)
#' P2 <- c(5,0)
#' angle <- 90
#' cut1 <- 1/3
#' cut2 <- 2/3
#' f <- 1
#' it <- 4
#' FractalSegment(P1, P2, angle, cut1, cut2, f, it)
#' @references  http://mathworld.wolfram.com/Fractal.html
#' @export
FractalSegment <- function(P1, P2, angle, cut1, cut2, f, it){
  if (it==0){
    DrawSegmentPoints(P1, P2, "black")
  }
  if (it>0){
    P11=P1+cut1*(P2-P1)
    P12=P1+cut2*(P2-P1)
    ### angle in radians
    angle=(pi*angle)/180
    xx=P11[1]
    yy=P11[2]
    x_fixed=P12[1]
    y_fixed=P12[2]
    xx_new=f*(xx-x_fixed)*cos(-angle)-f*(yy-y_fixed)*sin(-angle)+x_fixed
    yy_new=f*(xx-x_fixed)*sin(-angle)+f*(yy-y_fixed)*cos(-angle)+y_fixed
    if ((180*angle)/pi<=60 | ((180*angle)/pi>300 & (180*angle)/pi<=360)){P31=c(xx_new, yy_new)}
    if ((180*angle)/pi>60 & (180*angle)/pi<=300){P32=c(xx_new, yy_new)}
    #if ((180*angle)/pi>300 & (180*angle)/pi<=360){P31=P32}
    xx=P12[1]
    yy=P12[2]
    x_fixed=P11[1]
    y_fixed=P11[2]
    xx_new=f*(xx-x_fixed)*cos(angle)-f*(yy-y_fixed)*sin(angle)+x_fixed
    yy_new=f*(xx-x_fixed)*sin(angle)+f*(yy-y_fixed)*cos(angle)+y_fixed
    if ((180*angle)/pi<=60 | ((180*angle)/pi>300 & (180*angle)/pi<=360)){P32=c(xx_new, yy_new)}
    if ((180*angle)/pi>60 & (180*angle)/pi<=300){P31=c(xx_new, yy_new)}
    #if ((180*angle)/pi>300 & (180*angle)/pi<=360){P32=P31}
    ### angle in degrees to call the function again
    angle=(180*angle)/pi
    K1=FractalSegment(P1, P11, angle, cut1, cut2, f, it-1)
    K2=FractalSegment(P11, P31, angle, cut1, cut2, f, it-1)
    K3=FractalSegment(P31, P32, angle, cut1, cut2, f, it-1)
    K4=FractalSegment(P32, P12, angle, cut1, cut2, f, it-1)
    K5=FractalSegment(P12, P2, angle, cut1, cut2, f, it-1)
  }
}

