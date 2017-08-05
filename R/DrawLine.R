DrawLine<-function(Line, color){
  m=Line[1]
  n=Line[2]
  if (toString(m)=="Inf"){
    abline(v=as.numeric(n),col=color)
  }
  else{
    abline(a=n,b=m,col=color)
  }
  return()
}
