Scissor.Poly<-function(P,angle,l,A){
  total=0
  salir=0
  angle0=0
  while (salir==0){
    total=total+A
    s=Scissor(P,angle,angle0,l)
    P=s[1:2]
    angle0=s[3]
    angle0=left(A,angle0)
    if ((total %% 360)==0){
      salir=1
    }
  }
}