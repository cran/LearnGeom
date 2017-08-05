#' Creates an arc of a circumference
#' 
#' \code{CreateArcAngles} creates an arc of a circumference 
#' @param C Vector containing the xy-coordinates of the center of the circumference
#' @param r Radius for the circumference (or arc)
#' @param angle1 - Angle in degrees (0-360) at which the arc starts
#' @param angle2 - Angle in degrees (0-360) at which the arc finishes
#' @return Returns a vector which contains the center, radius and angles (0-360) that define the created arc
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' C <- c(0,0)
#' r <- 3
#' angle1 <- 90
#' angle2 <- 180
#' Arc <- CreateArcAngles(C, r, angle1, angle2)
#' Draw(Arc, "black")
#' @export
CreateArcAngles<-function(C, r, angle1, angle2){
  Arc=c(C,r,angle1,angle2)
  names(Arc)=c("X","Y","r","ang1","ang2")
  class(Arc) <- append(class(Arc),"Arc")
  return(Arc)
}

