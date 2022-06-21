#'@title Spherical refusal membership function
#'@description Spherical refusal membership values with membership,non-membership and indeterminacy values as input
#'@param m SFS membership values computed using either triangular or trapezoidal or guassian membership function
#'@param nm SFS non-membership values computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param im SFS indetermincay values
#'@return SFS refusal membership values
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'a<-mn(x)
#'b<-std(x)
#'m<-memG(a,b,x)
#'lam<-0.5
#'nm<-nonmemS(m,lam)
#'im<-imemSFS(m,nm)
#'rmemSFS(m,nm,im)
#'#       [,1]      [,2]      [,3]
#'#[1,] 0.7586762 0.5847071 0.4405241
#'#[2,] 0.7584805 0.4291073 0.5923419
#'#[3,] 0.5193742 0.7593476 0.5193742
#'#[4,] 0.5193742 0.7593476 0.5193742
#'@export
rmemSFS<-function(m,nm,im){
  rm<-sqrt(1.00-((m^2+nm^2+im^2)))
  return(rm)
}
