#'@title Left foot values
#'@description Left foot value for triangular or trapezoidal membership function
#'@param x A data set in the form of document-term matrix
#'@param n A constant value to fix the left foot value
#'@return Left foot values for the input data set x.
#'@examples x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'leftfootfinding(x,5)
#'#[1] 10 5 10 10
#'@export
leftfootfinding<-function(x,n)
{
  m<-c()
  left<-c()
  leftfoot<-c()
  for (i in 1:nrow(x)){
    m[i]<-min(x[i,])
    left[i]<-m[i]%%n
    leftfoot[i]<-m[i]-left[i]
  }
  return(leftfoot)
}
