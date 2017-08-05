#' Computes the angle that form two lines
#' 
#' \code{LinesAngles} computes the angle that form two lines
#' @param Line1 Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}
#' @param Line2 Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}
#' @return Returns the angle that form the two lines
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' Line1 <- CreateLinePoints(P1, P2)
#' P3 <- c(1,-1)
#' P4 <- c(2,3)
#' Line2 <- CreateLinePoints(P3, P4)
#' angle <- LinesAngles(Line1, Line2)
#' @export
LinesAngles<-function(Line1, Line2){
  m1=Line1[1]
  m2=Line2[1]
  if (m1==m2){
    angle=0
  }
  else{
    vector1=c(1,m1)
    vector2=c(1,m2)
    num=(vector1[1]*vector2[1]+vector1[2]*vector2[2])
    den=sqrt(vector1[1]^2+vector1[2]^2)*sqrt(vector2[1]^2+vector2[2]^2)
    angle=acos(num/den)
    angle=(360*angle)/(2*pi)
  }
  names(angle)="angle"
  return(angle)
}