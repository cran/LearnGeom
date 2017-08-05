#' Finds the intersection of two lines
#' 
#' \code{IntersectLines} finds the intesection of two lines
#' @param Line1 Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}
#' @param Line2 Line object previously created with \code{CreateLinePoints} or \code{CreateLineAngle}
#' @return Returns a vector containing the xy-coordinates of the intersection point. In case of no intersection, the function tells the user
#' @examples 
#' P1 <- c(0,0)
#' P2 <- c(1,1)
#' Line1 <- CreateLinePoints(P1, P2)
#' P3 <- c(1,-1)
#' P4 <- c(2,0)
#' Line2 <- CreateLinePoints(P3, P4)
#' intersection <- IntersectLines(Line1, Line2)
#' @export
IntersectLines<-function(Line1, Line2){
  m1=Line1[1]
  m2=Line2[1]
  n1=Line1[2]
  n2=Line2[2]
  if (m1==m2 & m1!="Inf" & m2!="Inf"){
    int="The lines do not intersect, they are parallel"
  }
  if (m1=="Inf" | m2!="Inf"){
    int=c(as.numeric(n1),m2*as.numeric(n1)+n2)
  }
  if (m2=="Inf" | m1!="Inf"){
    int=c(as.numeric(n2),m1*as.numeric(n2)+n1)
  }
  if (m1=="Inf" & m2=="Inf"){
    if (as.numeric(n1)==as.numeric(n2)){
      int="The lines are coincident"
    }
    else{
      int="The lines do not intersect, they are parallel"
    }
  }
  if (m1!="Inf" & m2!="Inf"){
    x=(n2-n1)/(m1-m2)
    y=m1*x+n1
    int=c(x,y)
    names(int)=c("X","Y")
  }
  return(int)
}