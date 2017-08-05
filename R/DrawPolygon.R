DrawPolygon<-function(Poly, color, border="black", label=FALSE){
  xx=Poly[,1]
  yy=Poly[,2]
  if (missing(label) | label==F){
    polygon(xx, yy, col = color, border = border)
  }
  if (label==T){
    polygon(xx, yy, col = color, border = border)
    for (i in c(1:nrow(Poly))){
      text(Poly[i,1],Poly[i,2],toString(i),col="black")
    }
  }
  return()
}
