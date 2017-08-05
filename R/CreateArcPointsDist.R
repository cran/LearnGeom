#' Creates an arc of a circumference to connect two points
#' 
#' \code{CreateArcPointsDist} creates an arc of a circumference to connect two points
#' @param P1 Vector containing the xy-coordinates of point 1
#' @param P2 Vector containing the xy-coordinates of point 2
#' @param r Radius for the circumference which is used to generate the arc. This parameter is necessary because there are infinite possible arcs that connect two points. In the case the radius is smaller than half the distance between \code{P1} and \code{P2}, there is no possible arc, so the function tells the user
#' @return Returns a vector which contains the center, radius and angles (0-360) that define the created arc
#' @examples 
#' x_min <- -5
#' x_max <- 5
#' y_min <- -5
#' y_max <- 5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' P1 <- c(0,0)
#' P2 <- c(3,3)
#' r <- sqrt(18)/2
#' Arc <- CreateArcPointsDist(P1, P2, r)
#' Draw(Arc, "black")
#' @export
CreateArcPointsDist<-function(P1, P2, r){
  if (r<DistancePoints(P1,P2)/2){
    print("There is no possible arc for this radius. The radius need to be greater")
    return()
  }
  x1=P1[1]
  y1=P1[2]
  x2=P2[1]
  y2=P2[2]
  ### Newton's method to solve the system of non-linear equations
  ### x, y, z to start the method are chosen randomly but depending on the centers and radiuses of the circles
  x=MidPoint(P1,P2)[1]+runif(1,-1,1)
  y=MidPoint(P1,P2)[2]+runif(1,-1,1)
  ### Definition of the functions that represent the non-linear system
  f1=function(x,y) (x-x1)^2+(y-y1)^2-r^2
  f2=function(x,y) (x-x2)^2+(y-y2)^2-r^2
  ### Jacobian matrix from functions f1, f2
  J11=function(x,y) 2*(x-x1)
  J12=function(x,y) 2*(y-y1)
  J21=function(x,y) 2*(x-x2)
  J22=function(x,y) 2*(y-y2)
  it=1000
  for (i in c(1:it)){
    Fx=c(f1(x,y),f2(x,y))
    ### Jacobian matrix evaluation for (x,y,r)
    J=matrix(data = c(J11(x,y),J12(x,y),
                      J21(x,y),J22(x,y)),
             nrow = 2,ncol = 2)
    J=t(J)
    ### Solve system J(X)Y=-F(X)
    Y=solve(J)%*%(-Fx)
    x=x+Y[1]
    y=y+Y[2]
  }
  C=c(x,y)
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
  Arc=c(C,r,min(c(ang1,ang2)),max(c(ang1,ang2)))
  names(Arc)=c("X","Y","r","ang1","ang2")
  class(Arc) <- append(class(Arc),"Arc")
  return(Arc)
}
