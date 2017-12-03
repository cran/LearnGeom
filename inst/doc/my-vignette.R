## ----fig.width=6, fig.height=6, fig.align="center"-----------------------
library(LearnGeom)
x_min <- -5 
x_max <- 5 
y_min <- -5 
y_max <- 5 
CoordinatePlane(x_min, x_max, y_min, y_max) 
P1 <- c(0,0) 
P2 <- c(1,1) 
P3 <- c(2,0)  
Poly <- CreatePolygon(P1, P2, P3) 
Draw(Poly, "blue") 
Hepta <- CreateRegularPolygon(7, c(-3,0), 1)
Draw(Hepta, "orange")
L <- CreateLinePoints(c(-1,0), c(0,3))
Draw(L, "red")
S <- CreateSegmentAngle(c(0,-2), 60, 1)
Draw(S, "black")
A <- CreateArcAngles(c(3,-2), 1, 0, 180, "anticlock")
Draw(A, "black")

## ----fig.width=6, fig.height=6, fig.align="center"-----------------------
x_min <- -5 
x_max <- 5 
y_min <- -5 
y_max <- 5 
CoordinatePlane(x_min, x_max, y_min, y_max) 
P1 <- c(0,0) 
P2 <- c(1,1) 
P3 <- c(2,0)  
Poly <- CreatePolygon(P1, P2, P3) 
Draw(Poly, "blue") 
fixed <- c(-1,-1) 
angle <- 30 
Poly_rotated <- Rotate(Poly, fixed, angle) 
Draw(Poly_rotated, "orange") 
fixed <- c(2,0) 
Poly_rotated <- Rotate(Poly, fixed, angle) 
Draw(Poly_rotated, "transparent") 

## ------------------------------------------------------------------------
P1 <- c(0,0)
P2 <- c(1,1)
P3 <- c(2,0) 
Tri <- CreatePolygon(P1, P2, P3)

## ------------------------------------------------------------------------
A <- Tri[1,]
B <- Tri[2,]
C <- Tri[3,]
AB <- B-A
AB_orto=c(-AB[2],AB[1])
AC <- C-A
AC_orto=c(-AC[2],AC[1])
BC <- C-B
BC_orto=c(-BC[2],BC[1])

## ------------------------------------------------------------------------
AB_mid <- MidPoint(A,B)
AC_mid <- MidPoint(A,C)
BC_mid <- MidPoint(B,C)

## ------------------------------------------------------------------------
AB1 <- AB_mid+AB_orto
AC1 <- AC_mid+AC_orto
BC1 <- BC_mid+BC_orto

## ------------------------------------------------------------------------
L1 <- CreateLinePoints(AB_mid,AB1)
L2 <- CreateLinePoints(AC_mid,AC1)
L3 <- CreateLinePoints(BC_mid,BC1)
I <- IntersectLines(L1,L2) 

## ----fig.width=6, fig.height=6, fig.align="center"-----------------------
x_min <- -6
x_max <- 6
y_min <- -6
y_max <- 6
CoordinatePlane(x_min, x_max, y_min, y_max) 
P1 <- c(0,0)
P2 <- c(1,1)
P3 <- c(2,0) 
Tri <- CreatePolygon(P1, P2, P3)
Draw(Tri, "blue")
Draw(L1, "red")
Draw(L2, "red")
Draw(L3, "red")
Draw(I, "red")

## ----fig.width=6, fig.height=6, fig.align="center"-----------------------
## Creation of the starting hexagon, Hexa0
x_min <- -5; x_max <- 5; y_min <- -5; y_max <- 5
CoordinatePlane(x_min, x_max, y_min, y_max)
Hexa0 <- CreateRegularPolygon(6, c(-3,0), 1)
C=CenterPolygon(Hexa0)
Draw(Hexa0, "gold", label = T)
## Computation of the distance from the right extreme of the hexagon 
## to its central axis
d <- DistancePoints(Hexa0[1,], c(-3,Hexa0[1,2]))
separation <- 2*d
Tessellation(list(Hexa0), "gold", c(1,0), separation, 4)
mid1 <- MidPoint(Hexa0[1,],Hexa0[6,])
mid2 <- MidPoint(Hexa0[2,],Hexa0[3,])
v1 <- mid1-CenterPolygon(Hexa0)
v2 <- mid2-CenterPolygon(Hexa0)
Hexa1 <- Translate(Hexa0,2*v1)
Hexa2 <- Translate(Hexa0,2*v2)
Draw(Hexa1,"gold")
Tessellation(list(Hexa1), "gold", c(1,0), separation, 3)
Draw(Hexa2,"gold")
Tessellation(list(Hexa2), "gold", c(1,0), separation, 3)
## Drawing again the starting polygon to visualize the labels at its 
## vertex
Draw(Hexa0, "gold", label = T)

## ------------------------------------------------------------------------
L1 <- CreateLinePoints(Hexa1[1,],Hexa1[6,])
L2 <- CreateLinePoints(Hexa1[2,],Hexa1[3,])
Hexa1 <- ReflectedPolygon(Hexa1,L1)
Hexa2 <- ReflectedPolygon(Hexa1,L2)

## ----fig.width=6, fig.height=6, fig.align="center"-----------------------
x_min <- -6
x_max <- 6
y_min <- -6
y_max <- 6
CoordinatePlane(x_min, x_max, y_min, y_max)
n <- 3; C <- c(0,0); l <- 5
Tri <- CreateRegularPolygon(n, C, l)
it <- 1
Sierpinski(Tri, it)
CoordinatePlane(x_min, x_max, y_min, y_max)
it <- 3
Sierpinski(Tri, it)

## ----fig.width=6, fig.height=6, fig.align="center"-----------------------
x_min <- -6
x_max <- 6
y_min <- -4
y_max <- 8
CoordinatePlane(x_min, x_max, y_min, y_max)
P1 <- c(-5,0)
P2 <- c(5,0)
angle <- 90
cut1 <- 1/3
cut2 <- 2/3
f <- 1
it <- 4
FractalSegment(P1, P2, angle, cut1, cut2, f, it)

## ----fig.width=6, fig.height=6, fig.align="center"-----------------------
x_min <- -6
x_max <- 6
y_min <- -4
y_max <- 8
CoordinatePlane(x_min, x_max, y_min, y_max)
P1 <- c(-5,0)
P2 <- c(5,0)
angle <- 60
cut1 <- 1/3
cut2 <- 2/3
f <- 1
it <- 4
FractalSegment(P1, P2, angle, cut1, cut2, f, it)

## ----fig.width=6, fig.height=6, fig.align="center"-----------------------
P=c(0,0)
CoordinatePlane(-100,100,-50,150)
Duopoly(P,1,1,1,-3)
CoordinatePlane(-100,100,-50,150)
Duopoly(P,0.8,1,0.8,3)
CoordinatePlane(-100,100,-50,150)
Duopoly(P,2,3,2,10)

