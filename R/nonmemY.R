#'@title Yager's non membership function
#'@description Yager's non membership function with membership values and lambda value
#'@param m Membership values for the data set x
#'@param lam Control parameter to establish relationship between membership and non-membership values, values range from 0.1 to 1.0
#'@return Yager's non membership for the data set x.
#'@references R. R. Yager. On the measure of fuzziness and negation part i: membership in the unit interval. 1979.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'a<-mn(x)
#'b<-std(x)
#'m<-memG(a,b,x)
#'lam<-0.5
#'nonmemY(m,lam)
#'#         [,1]        [,2]        [,3]
#'#[1,] 0.078966962 0.011638215 0.002959405
#'#[2,] 0.078578801 0.002628666 0.012471988
#'#[3,] 0.006392896 0.080354498 0.006392896
#'#[4,] 0.006392896 0.080354498 0.006392896
#'@export
nonmemY<-function(m,lam)
{
  c<-matrix(0,nrow=nrow(m),ncol=ncol(m))
  for (i in 1:nrow(m)) {
    for(j in 1:ncol(m))
      c[i,j]<-(1-(m[i,j])^lam)^(1/lam)
  }
  return(c);
}
