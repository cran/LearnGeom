#' Creates an arc of a circumference to connect two points
#' 
#' \code{CreateArcPointsDist} creates an arc of a circumference to connect two points
#' @param P1 Vector containing the xy-coordinates of point 1
#' @param P2 Vector containing the xy-coordinates of point 2
#' @param r Radius for the circumference which is used to generate the arc. This parameter is necessary because there are infinite possible arcs that connect two points. In the case the radius is smaller than half the distance between \code{P1} and \code{P2}, there is no possible arc, so the function tells the user
#' @param choice - Integer indicating which of the two possible centers is chosen to create the arcs. A value of 1 means the center of the circle that contains the arc is chosen in the direction of M + v, being M the middle point between P1 and P2 and v the orthogonal vector of P2 - P1 normalized to the appropriate length for creating the desired arc. A value of 2 means the center of the resulting circle is chosen in the direction of M - V. Remark: There are as well two options for vector v. If P1 = (a,b) and P2 = (c,d), v is written in the internal function as (b-d,c-a)
#' @param direction - String indicating the direction which is considered to create the arc, from the smaller to the higher angle. It has two possible values: "clock" (clockwise direction) and "anticlock" (anti-clockwise direction)
#' @return Returns a vector which contains the center, radius and angles (0-360) that define the created arc
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P1 <- c(-3,2)
#' P2 <- c(0,0)
#' r <- sqrt(18)/2
#' choice=1
#' direction="anticlock"
#' Arc <- CreateArcPointsDist(P1, P2, r, choice, direction)
#' Draw(Arc, "red")
#' choice=2
#' direction="anticlock"
#' Arc <- CreateArcPointsDist(P1, P2, r, choice, direction)
#' Draw(Arc, "blue")
#' choice=1
#' direction="clock"
#' Arc <- CreateArcPointsDist(P1, P2, r, choice, direction)
#' Draw(Arc, "pink")
#' choice=2
#' direction="clock"
#' Arc <- CreateArcPointsDist(P1, P2, r, choice, direction)
#' Draw(Arc, "green")
#' @export
CreateArcPointsDist<-function(P1, P2, r, choice, direction){
  if (r<DistancePoints(P1,P2)/2){
    print("There is no possible arc for this radius. The radius needs to be, at least, half the distance between P1 and P2")
    return()
  }
  x1=P2[1]
  y1=P2[2]
  x2=P1[1]
  y2=P1[2]
  ### Center obtention
  l=sqrt((x1-x2)^2+(y1-y2)^2)
  z=sqrt(r^2-(l^2/4))
  v=c(-(y1-y2),x1-x2)
  if (choice==1){
    x=((x1+x2)/2)+z*(v[1]/sqrt(v[1]^2+v[2]^2))
    y=((y1+y2)/2)+z*(v[2]/sqrt(v[1]^2+v[2]^2))
    C=c(x,y)
  }
  if (choice==2){
    x=((x1+x2)/2)-z*(v[1]/sqrt(v[1]^2+v[2]^2))
    y=((y1+y2)/2)-z*(v[2]/sqrt(v[1]^2+v[2]^2))
    C=c(x,y)
  }
  coseno1=(P1[1]-C[1])/r
  seno1=(P1[2]-C[2])/r
  coseno2=(P2[1]-C[1])/r
  seno2=(P2[2]-C[2])/r
  ang1=acos(coseno1)
  ang1=180*ang1/pi
  if (coseno1>0 & seno1<0){ang1=360-ang1}
  if (coseno1<0 & seno1<0){ang1=270-(ang1-90)}
  ang2=acos(coseno2)
  ang2=180*ang2/pi
  if (coseno2>0 & seno2<0){ang2=360-ang2}
  if (coseno2<0 & seno2<0){ang2=270-(ang2-90)}
  ### Anti-clock direction is assumed
  if (direction=="anticlock"){Arc=c(C,r,min(c(ang1,ang2)),max(c(ang1,ang2)),1)}
  if (direction=="clock"){Arc=c(C,r,min(c(ang1,ang2)),max(c(ang1,ang2)),2)}
  names(Arc)=c("X","Y","r","ang1","ang2","dir")
  class(Arc) <- append(class(Arc),"Arc")
  return(Arc)
}

