#' Creates an arc of a circumference
#' 
#' \code{CreateArcAngles} creates an arc of a circumference 
#' @param C Vector containing the xy-coordinates of the center of the circumference
#' @param r Radius for the circumference (or arc)
#' @param angle1 - Angle in degrees (0-360) at which the arc starts
#' @param angle2 - Angle in degrees (0-360) at which the arc finishes
#' @param direction - String indicating the direction which is considered to create the arc, from the smaller to the higher angle. It has two possible values: "clock" (clockwise direction) and "anticlock" (anti-clockwise direction) 
#' @return Returns a vector which contains the center, radius, angles (0-360) and direction (1 - "clock", 2 - "anticlock") that define the created arc
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
#' direction <- "anticlock"
#' Arc1 <- CreateArcAngles(C, r, angle1, angle2, direction)
#' Draw(Arc1, "black")
#' direction <- "clock"
#' Arc2 <- CreateArcAngles(C, r, angle1, angle2, direction)
#' Draw(Arc2, "red")
#' @export
CreateArcAngles<-function(C, r, angle1, angle2, direction="anticlock"){
  Arc=c(C,r,angle1,angle2,ifelse(direction=="anticlock",1,2))
  names(Arc)=c("X","Y","r","ang1","ang2","dir")
  class(Arc) <- append(class(Arc),"Arc")
  return(Arc)
}

