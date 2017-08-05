left <- function(angle, angle0){
  if (angle<0){
    angle=360-(-angle)
  }
  angle=angle0+angle
  angle=(angle)%%360
  return(angle)
}


