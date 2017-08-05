#' Rotates a geometric object
#' 
#' \code{Rotate} rotates a geometric object of any of the following types: line, polygon or segment
#' @param object geometric object of type line, polygon or segment, previously created with any of the functions in the package
#' @param fixed Vector containing the xy-coordinates of the only point of the plane which remains fixed during rotation 
#' @param angle Angle of rotation in degrees (0-360), considering the clockwise direction
#' @return Returns a geometric object which is the rotation of the original one, following the clockwise direction
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
#' Draw(Poly, "blue")
#' fixed <- c(-1,-1)
#' angle <- 30
#' Poly_rotated <- Rotate(Poly, fixed, angle)
#' Draw(Poly_rotated, "orange")
#' fixed <- c(2,0)
#' Poly_rotated <- Rotate(Poly, fixed, angle)
#' Draw(Poly_rotated, "transparent")
#' @export
Rotate<-function(object, fixed, angle){
  if (length(class(object))==2){
    if (class(object)[2]=="Line"){
      P=c(0,object[2])
      Q=c(1,object[1]+object[2])
      object=rbind(P,Q)
      xx=object[,1]
      yy=object[,2]
      x_fixed=fixed[1]
      y_fixed=fixed[2]
      angle_rad=-2*pi*angle/360
      xx_new=(xx-x_fixed)*cos(angle_rad)-(yy-y_fixed)*sin(angle_rad)+x_fixed
      yy_new=(xx-x_fixed)*sin(angle_rad)+(yy-y_fixed)*cos(angle_rad)+y_fixed
      object_new=CreateLinePoints(c(xx_new[1],yy_new[1]),c(xx_new[2],yy_new[2]))
      return(object_new)
    }
  }
  if (length(class(object))==1){
      object=t(as.matrix(object))
  }
  xx=object[,1]
  yy=object[,2]
  x_fixed=fixed[1]
  y_fixed=fixed[2]
  angle_rad=-2*pi*angle/360
  xx_new=(xx-x_fixed)*cos(angle_rad)-(yy-y_fixed)*sin(angle_rad)+x_fixed
  yy_new=(xx-x_fixed)*sin(angle_rad)+(yy-y_fixed)*cos(angle_rad)+y_fixed
  object_new=cbind(xx_new,yy_new)
  colnames(object_new)=c("X","Y")
  class(object_new) <- append(class(object_new),class(object)[2])
  return(object_new)
}

