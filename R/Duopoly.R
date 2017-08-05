#' Plots a fractal curve from the trochoids family. Any curve from this family can be defined by some parametrical equations, but they can also be produced (approximated) through a simple iterative process based on segment drawing for certain angles and lengths 
#' 
#' \code{Duopoly} plots a closed curve from the trochoids family
#' @param P Vector containing the xy-coordinates of the starting point for the curve
#' @param l1 Number that indicates the length side of the segment drawn the first in each of the steps of the process
#' @param angle1 Angle (0-360) that indicates the direction of the segment which is drawn the first in each of the steps of the process
#' @param l2 Number that indicates the length side of the segment drawn the second in each of the steps of the process
#' @param angle2 Angle (0-360) that indicates the direction of the segment which is drawn the second in each of the steps of the process
#' @param time Number of seconds to wait for the program before drawing each of the segments that make the trochoid curve. If no \code{time} is specified, default value is 0 (no waiting time). If the chosen time is very small (\code{time} < 0.05) it is possible that the program shows the plot directly. In this case, it should be increased the \code{time} parameter.
#' @param color Color to indicate the points that are obtained during the process to approximate the trochoid. If missing, the points are not indicated and only the segments are drawn in the plot
#' @return None. It produces the plot of a curve from the trochoids family
#' @examples 
#' x_min <- -100
#' x_max <- 100
#' y_min <- -50
#' y_max <- 150
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P <- c(0,0)
#' l1 <- 2
#' angle1 <- 3
#' l2 <- 2
#' angle2 <- 10 
#' Duopoly(P, l1, angle1, l2, angle2)
#' @references Abelson, H., & DiSessa, A. A. (1986). Turtle geometry: The computer as a medium for exploring mathematics. MIT press
#' @references Armon, U. (1996). Representing trochoid curves by DUOPOLY procedure. International Journal of Mathematical Education in Science and Technology, 27(2), 177-187
#' @export
Duopoly <-function(P,l1,angle1,l2,angle2,time=0,color="transparent"){
  GCD <- function(x,y){
    if (y==0){GCD=abs(x)}
    else{
      GCD=GCD(y,x%%y)
    }
    return(GCD)
  }
  last_n=360/(GCD(360,GCD(angle1,angle2)))
  P1=P
  Draw(P1,color)
  for (i in c(1:last_n)){
    Sys.sleep(time)
    DrawSegmentAngle(P1,i*angle1,l1,"black")
    angle1_rad=(pi*i*angle1)/180
    P2=P1+l1*c(cos(angle1_rad),sin(angle1_rad))
    Draw(P2,color)
    Sys.sleep(time)
    DrawSegmentAngle(P2,i*angle2,l2,"black")
    angle2_rad=(pi*i*angle2)/180
    P1=P2+l2*c(cos(angle2_rad),sin(angle2_rad))
    Draw(P1,color)
  }
}