#' Plots an empty coordinate (cartesian) plane with customizable limits for the X and Y axis
#' 
#' \code{CoordinatePlane} plots an empty coordinate (cartesian) plane with customizable limits for the X and Y axis.
#' @param x_min Lowest value for the X axis
#' @param x_max Highest value for the X axis
#' @param y_min Lowest value for the Y axis
#' @param y_max Highest value for the Y axis
#' @return None. It produces a plot of a coordinate plane with axes and grid
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' @export
CoordinatePlane <-function(x_min, x_max, y_min, y_max){
  plot(NULL, xlim=c(x_min,x_max), ylim=c(y_min,y_max), xlab="X", ylab="Y", xaxs="i", yaxs="i")
  grid(x_max-x_min, y_max-y_min)
  return()
}

