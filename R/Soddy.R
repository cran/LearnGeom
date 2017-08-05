#' Finds the inner and outer Soddy circles of three given mutually tangent circles
#' 
#' \code{Soddy} finds inner and outer Soddy circles of three given mutually tangent circles
#' @param A Vector containing the xy-coordinates of the center of circumference 1
#' @param r1 Radius for circumference 1
#' @param B Vector containing the xy-coordinates of the center of circumference 2
#' @param r2 Radius for circumference 2
#' @param C Vector containing the xy-coordinates of the center of circumference 3
#' @param r3 Radius for circumference 3
#' @return A list which contains the Soddy center and the radiuses of Soddy inner and outer circle of three mutually tangent circles
#' @examples 
#' x_min <- -3
#' x_max <- 3
#' y_min <- -2.5
#' y_max <- 3.5
#' CoordinatePlane(x_min, x_max, y_min, y_max)
#' A <- c(-1,0)
#' B <- c(1,0)
#' C <- c(0,sqrt(3))
#' r1 <- 1
#' r2 <- 1
#' r3 <- 1
#' Draw(CreateArcAngles(A, r1, 0, 360), "black")
#' Draw(CreateArcAngles(B, r2, 0, 360), "black")
#' Draw(CreateArcAngles(C, r3, 0, 360), "black")
#' result <- Soddy(A, r1, B, r2, C, r3)
#' soddy_point <- result[[1]]
#' inner_radius <- result[[2]]
#' outer_radius <- result[[3]]
#' Draw(soddy_point,"red")
#' Draw(CreateArcAngles(soddy_point,inner_radius,0,360),"red")
#' Draw(CreateArcAngles(soddy_point,outer_radius,0,360),"red")
#' @references http://mathworld.wolfram.com/SoddyCircles.html
#' @export
Soddy <- function(A, r1, B, r2, C, r3){
  x1=A[1]
  y1=A[2]
  x2=B[1]
  y2=B[2]
  x3=C[1]
  y3=C[2]
  s1=-1
  s2=-1
  s3=-1
  xmax=max(c(A[1]+r1,B[1]+r2,C[1]+r3))
  xmin=min(c(A[1]-r1,B[1]-r2,C[1]-r3))
  ymax=max(c(A[2]+r1,B[2]+r2,C[2]+r3))
  ymin=min(c(A[2]-r1,B[2]-r2,C[2]-r3))
  ### Newton's method to solve the system of non-linear equations
  ### x, y, z to start the method are chosen randomly but depending on the centers and radiuses of the circles
  x=runif(1,xmin,xmax)
  y=runif(1,ymin,ymax)
  r=runif(1,mean(r1,r2,r3),mean(r1,r2,r3)+1)
  ### Definition of the functions that represent the non-linear system
  f1=function(x,y,r) (x-x1)^2+(y-y1)^2-(r-s1*r1)^2
  f2=function(x,y,r) (x-x2)^2+(y-y2)^2-(r-s2*r2)^2
  f3=function(x,y,r) (x-x3)^2+(y-y3)^2-(r-s3*r3)^2
  ### Jacobian matrix from functions f1, f2, f3
  J11=function(x,y,r) 2*(x-x1)
  J12=function(x,y,r) 2*(y-y1)
  J13=function(x,y,r) -2*(r-s1*r1)
  J21=function(x,y,r) 2*(x-x2)
  J22=function(x,y,r) 2*(y-y2)
  J23=function(x,y,r) -2*(r-s2*r2)
  J31=function(x,y,r) 2*(x-x3)
  J32=function(x,y,r) 2*(y-y3)
  J33=function(x,y,r) -2*(r-s3*r3)
  it=1000
  for (i in c(1:it)){
    Fx=c(f1(x,y,r),f2(x,y,r),f3(x,y,r))
    ### Jacobian matrix evaluation for (x,y,r)
    J=matrix(data = c(J11(x,y,r),J12(x,y,r),J13(x,y,r),
                      J21(x,y,r),J22(x,y,r),J23(x,y,r),
                      J31(x,y,r),J32(x,y,r),J33(x,y,r)),
             nrow = 3,ncol = 3)
    J=t(J)
    ### Solve system J(X)Y=-F(X)
    Y=solve(J)%*%(-Fx)
    x=x+Y[1]
    y=y+Y[2]
    r=r+Y[3]
  }
  inner_ratius=r1*r2*r3/(r1*r2+r1*r3+r2*r3+2*sqrt(r1*r2*r3*(r1+r2+r3)))
  outer_ratius=-r1*r2*r3/(r1*r2+r1*r3+r2*r3-2*sqrt(r1*r2*r3*(r1+r2+r3)))
  result=list(c(x,y),inner_ratius,outer_ratius)
  return(result)
}
