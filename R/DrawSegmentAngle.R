DrawSegmentAngle<-function(P, angle, l, color){
  P1=P
  angle=(pi*angle)/180
  P2=P+c(l*cos(angle), l*sin(angle))
  segments(P1[1], P1[2], P2[1], P2[2], col=color)
  return()
}