#'@title Spherical indeterminacy membership function
#'@description Spherical indeterminacy membership values with membership and non-membership values as input
#'@param m SFS membership values computed using either triangular or trapezoidal or guassian membership function
#'@param nm SFS non-membership values computed using either Sugeno and Terano's  or Yager's non-membership function
#'@return SFS indeterminacy membership values
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'a<-mn(x)
#'b<-std(x)
#'m<-memG(a,b,x)
#'lam<-0.5
#'nm<-nonmemS(m,lam)
#'imemSFS(m,nm)
#'#        [,1]       [,2]       [,3]
#'#[1,] 0.09921264 0.05810582 0.03270001
#'#[2,] 0.09915966 0.03100937 0.05966479
#'#[3,] 0.04565299 0.09939456 0.04565299
#'#[4,] 0.04565299 0.09939456 0.04565299
#'@export
imemSFS<-function(m,nm){
  im<-1.00-(m+nm)
  return(im)
}
