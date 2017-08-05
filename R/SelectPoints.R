#' Selection of points from the coordinate plane
#' 
#' \code{SelectPoints} allows the selection of points from the coordinate plane
#' @param n Number of points to select from the current coordinate plane
#' @return Returns a vector or matrix which contains the xy-coordinates of the selected points. Each row represents one of the points. If \code{n = 1} the output is a numeric vector, if \code{n = 2} then it is a \code{Segment}, and for \code{n > 2} the object is a polygon.
#' @examples
#' n <- 3
#' points <- SelectPoints(n)
#' @export
SelectPoints<-function(n){
  points=locator(n)
  table=cbind(as.numeric(points$x),as.numeric(points$y))
  colnames(table)=c("X","Y")
  if (n==1){
    class(table)="Numeric"
  } else if (n==2){
    class(table)=append(class(table),"Segment")
  } else if (n>2){
    class(table)=append(class(table),"Polygon")
  }
  return(table)
}