#'@title Trapezoidal membership function
#'@description Trapezoidal membership function with leftfooting, leftshoulder, rightshoulder, rightfooting, and data set
#'@param x A data set in the form of document-term matrix
#'@param a Leftfooting value of the data set x
#'@param b Leftshoulder value of the data set x
#'@param c Rightshoulder value of the data set x
#'@param d Rightfooting value of the data set x
#'@return Trapezoidal membership values for the input data set x.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'a<-leftfootfinding(x,5)
#'mid<-midvalue(x)
#'b<-leftshoulderfinding(2.5,mid)
#'c<-rightshoulderfinding(mid,2.5)
#'d<-rightfootfinding(x,5)
#'memTP(a,b,c,d,x)
#'#       [,1]      [,2]      [,3]
#'#[1,] 0.5000000 0.6666667 0.8333333
#'#[2,] 0.7272727 0.8888889 0.6666667
#'#[3,] 1.0000000 1.0000000 1.0000000
#'#[4,] 0.2000000 0.2000000 0.2000000
#'@export
memTP<-function(a,b,c,d,x){
  mat2<-matrix(0,nrow=nrow(x),ncol=ncol(x))
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      mat2[i,j]<-ifelse(x[i,j]<=a[i],0,ifelse(x[i,j]>=a[i]&&x[i,j]<=b[i],((x[i,j]-a[i])/(b[i]-a[i])),ifelse(x[i,j]>=b[i]&&x[i,j]<=c[i],1,ifelse(x[i,j]>=c[i]&&x[i,j]<=d[i],((d[i]-x[i,j])/(d[i]-c[i])),0))))
    }
  }
  return(mat2)
}
