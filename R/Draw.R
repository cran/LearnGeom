#' Plots a geometric object
#' 
#' \code{Draw} plots geometric objects
#' @param object geometric object of any of these five types: point, segment, arc, line or polygon. A point is simply a vector of length 2, which contains the xy-coordinates for the point. For the other four types, there can be created with any of the following functions: \cr 
#' - \code{CreateArcAngles} \cr 
#' - \code{CreateArcPointsDist} \cr 
#' - \code{CreateLineAngle} \cr 
#' - \code{CreateLinePoints} \cr 
#' - \code{CreatePolygon} \cr 
#' - \code{CreateRegularPolygon} \cr 
#' - \code{CreateSegmentAngle} \cr 
#' - \code{CreateSegmentPoints} \cr 
#' @param colors Vector containing information about the color for the object to be plotted. In the case of polygons, the vector should have length 2 to define the background color and the border color (in this order). Moreover, it can be used \code{"transparent"} in the case no background color is needed for the polygon. For the other four types of objects, \code{color} should be a vector of length 1 (or a simple string) to indicate the color for the object. If this parameter is not specified the default color is black (for polygons, it is black for the background and the border)
#' @param label Boolean, only used for polygons. When \code{label} = \code{TRUE} and the object is a polygon, the plot displays the numbers that correspond to the order of the points of the polygon. If missing, it works as with \code{label} = \code{FALSE}, so the numbers are not displayed 
#' @return None. It produces the plot of a geometric object (point, segment, arc, line or polygon) in the current coordinate plane
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' P3 <- c(2,0) 
#' Poly <- CreatePolygon(P1, P2, P3)
#' Draw(Poly, c("blue"))
#' @export
Draw<-function(object, colors=c("black","black"), label=FALSE){
  if (class(object)[1]=="numeric" & length(class(object))==1){
    DrawPoint(object,colors[1])
  } else{
      if (class(object)[2]=="Polygon"){
        if (length(colors)==1 & label==TRUE){
          DrawPolygon(object,colors[1],label=TRUE)
        } else if (length(colors)==1 & label==FALSE){
          DrawPolygon(object,colors[1])
        } else if (length(colors)==2 & label==TRUE){
          DrawPolygon(object,colors[1],colors[2],label=TRUE)
        } else if (length(colors)==2 & label==FALSE){
          DrawPolygon(object,colors[1],colors[2])
        }
      }
      if (class(object)[2]=="Line"){
        DrawLine(object,colors[1])
      }
      if (class(object)[2]=="Segment"){
        DrawSegmentPoints(object[1,],object[2,],colors[1])
      }
      if (class(object)[2]=="Arc"){
        if (object[6]==1){
          DrawArc(c(object[1],object[2]),object[3],object[4],object[5],colors[1])
        }
        if (object[6]==2){
          DrawArc(c(object[1],object[2]),object[3],max(c(object[4],object[5])),min(c(object[4],object[5]))+360,colors[1])
        }
      }
  }
  return()
}