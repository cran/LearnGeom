DrawArc<-function(C, r, angle1, angle2, color){
  angle1_rad=2*pi*angle1/360
  angle2_rad=2*pi*angle2/360
  partition=seq(angle1_rad,angle2_rad,(angle2_rad-angle1_rad)/100)
  P1=c(C[1]+r*cos(partition[1]),C[2]+r*sin(partition[1]))
  for (t in partition[2:length(partition)]){
    P2=c(C[1]+r*cos(t),C[2]+r*sin(t))
    segments(P1[1],P1[2],P2[1],P2[2],col=color)
    P1=P2
  }
  return()
}

