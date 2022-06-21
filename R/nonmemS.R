#'@title Sugeno and Terano's non membership function
#'@description Sugeno and Terano's non membership function with membership values and lambda value
#'@param m Membership values for the data set x
#'@param lam Control parameter to establish relationship between membership and non-membership values, values range from 0.1 to 1.0
#'@return Sugeno and Terano's non membership for the data set x.
#'@references M. Sugeno and T. Terano. A model of learning based on fuzzy information. Kybernetes, 1977.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'a<-mn(x)
#'b<-std(x)
#'m<-memG(a,b,x)
#'lam<-0.5
#'nonmemS(m,lam)
#'#        [,1]      [,2]       [,3]
#'#[1,] 0.3838416 0.1460171 0.07314142
#'#[2,] 0.3828998 0.0689030 0.15121934
#'#[3,] 0.1078653 0.3871883 0.10786528
#'#[4,] 0.1078653 0.3871883 0.10786528
#'@export
nonmemS<-function(m,lam){
  c<-matrix(0,nrow=nrow(m),ncol=ncol(m))
  for (i in 1:nrow(m)) {
    for(j in 1:ncol(m))
      c[i,j]<-(1-m[i,j])/(1+(lam*m[i,j]))
  }
  return(c);
}
