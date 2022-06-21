#'@title Left shoulder values
#'@description Left shoulder value for trapezoidal membership function
#'@param a A constant value for fixing the left shoulder
#'@param b Middle values for the data set x
#'@return Left shoulder values for the input data set x.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'mid<-midvalue(x)
#'leftshoulderfinding(2.5,mid)
#'#[1] 14.0 10.5 12.0 15.0
#'@export
leftshoulderfinding<-function(a,b)
{
  ls<-c()
  for(i in 1:length(b))
  {
    ls[i]=b[i]-a
  }
  return(ls)
}
