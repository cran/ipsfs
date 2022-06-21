#'@title Triangular membership function
#'@description Triangular membership function with leftfooting, midvalue, rightfooting, and data set
#'@param x A data set in the form of document-term matrix
#'@param a Leftfooting value of the data set x
#'@param b Middle value of the data set x
#'@param c Rightfooting value of the data set x
#'@return Triangular membership values for the input data set x.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'a<-leftfootfinding(x,5)
#'b<-midvalue(x)
#'c<-rightfootfinding(x,5)
#'memT(a,b,c,x)
#'#       [,1]      [,2]      [,3]
#'#[1,] 0.3076923 0.4705882 0.5882353
#'#[2,] 0.5000000 0.5714286 0.4285714
#'#[3,] 0.8888889 0.9090909 0.8888889
#'#[4,] 0.1333333 0.1333333 0.1333333
#'@export
memT<-function(a,b,c,x){
  mat1<-matrix(0,nrow=nrow(x),ncol=ncol(x))
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      mat1[i,j]<-ifelse(x[i,j]<=a[i],0,ifelse(x[i,j]>=a[i]&&x[i,j]<=b[i],((x[i,j]-a[i])/(b[i]-a[i])),ifelse(x[i,j]>=b[i]&&x[i,j]<=c[i],((c[i]-x[i,j])/(c[i]-b[i])),0)))
    }
  }
  return(mat1)
}
