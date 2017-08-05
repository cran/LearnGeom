right <- function(angle, angle0){
  if (angle<0){
    angle=360-(-angle)
  }
  angle=(90-angle0)+angle
  angle=(angle)%%360
  if (angle >=0 & angle <=90){
    angle=90-angle
  }
  if (angle >=90 & angle <=180){
    angle=-(angle-90)
  }
  if (angle >=180 & angle <=270){
    angle=180+90-(angle-180)
  }
  if (angle >=270 & angle <=360){
    angle=90+90-(angle-270)
  }
  return(angle)
}