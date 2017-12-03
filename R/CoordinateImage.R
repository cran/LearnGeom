#' Plots a coordinate (cartesian) plane with customizable limits for the X and Y axis over a background image
#' 
#' \code{CoordinateImage} plots a coordinate (cartesian) plane with customizable limits for the X and Y axis over a background image
#' @param x_min Lowest value for the X axis
#' @param x_max Highest value for the X axis
#' @param y_min Lowest value for the Y axis
#' @param y_max Highest value for the Y axis
#' @param image Raster object (bitmap image). This image can be loaded in R with function \code{readPNG} (png package) or \code{readJPEG} (jpeg package). It is also possible to obtain aerial images from Google Maps with the aid of RgoogleMaps package (as in the following example) 
#' @return None. It produces a plot of a coordinate plane over a background image. It allows the user to use the functions of the package over a real and physical object
#' @examples 
#' library(RgoogleMaps)
#' ima=GetMap("Piazza San Pietro, 00120 Citta del Vaticano", zoom = 17, maptype = "satellite")
#' ima=ima$myTile
#' CoordinateImage(-6, 6, -6, 6, ima) 
#' Draw(CreateArcAngles(c(0.8411822, 0), 1.5, 0, 360), "white")
#' Draw(CreateLinePoints(c(0.8411822, 0), c(1.880481, 1.087281)), "white")
#' Draw(CreateLinePoints(c(0.8411822, 0), c(1.89672, -0.9488258)), "white")
#' @export
CoordinateImage <-function(x_min, x_max, y_min, y_max, image){
  plot(NULL, xlim=c(x_min,x_max), ylim=c(y_min,y_max), xlab="X", ylab="Y", xaxs="i", yaxs="i")
  lim <- par()
  rasterImage(image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  grid(x_max-x_min, y_max-y_min)
  return()
}

