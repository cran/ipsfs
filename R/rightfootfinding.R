#'@title Right foot values
#'@description Right foot value for triangular or trapezoidal membership function
#'@param x A data set in the form of document-term matrix
#'@param n A constant value to fix the right foot value
#'@return Right foot values for the input data set x.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'rightfootfinding(x,5)
#'#[1] 25 20 20 25
#'@export
rightfootfinding<-function(x,n)
{
  m<-c()
  right<-c()
  rightfoot<-c()
  for (i in 1:nrow(x)){
    m[i]<-max(x[i,])
    right[i]<-m[i]%%n
    rightfoot[i]<-m[i]+(n-right[i])
  }
  return(rightfoot)
}
