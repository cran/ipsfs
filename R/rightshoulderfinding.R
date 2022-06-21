#'@title Right shoulder values
#'@description right shoulder value for trapezoidal membership function
#'@param c A constant value for fixing the right shoulder
#'@param b Middle values for the data set x
#'@return Right shoulder values for the input data set x.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'mid<-midvalue(x)
#'rightshoulderfinding(mid,2.5)
#'#[1] 19.0 15.5 17.0 20.0
#'@export
rightshoulderfinding<-function(b,c)
{
  rs<-c()
  for(i in 1:length(b))
  {
    rs[i]=b[i]+c
  }
  return(rs)
}
