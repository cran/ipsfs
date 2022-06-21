#'@title PFS similarity measure simZHFLL1
#'@description PFS similarity measure values using simZHFLL1 computation technique with membership, and non-membership values of two objects or set of objects.
#'@param ma PFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na PFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param mb PFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb PFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param k A constant value, considered as 1
#'@return The PFS similarity values of data set y with data set x
#'@references Q. Zhang, J. Hu, J. Feng, A. Liu, and Y. Li. New similarity measures of pythagorean fuzzy sets and their applications. IEEE Access, 7:138192 - 138202, 2019.
#'@examples
#'x<-matrix(c(12,9,14,11,21,16,15,24,20,17,14,11),nrow=4)
#'y<-matrix(c(11,21,6),nrow=1)
#'a<-mn(x)
#'b<-std(x)
#'a1<-mn(y)
#'b1<-std(y)
#'lam<-0.5
#'ma<-memG(a,b,x)
#'na<-nonmemS(ma,lam)
#'mb<-memG(a1,b1,y)
#'nb<-nonmemS(mb,lam)
#'k<-1
#'simZHFLL1(ma,na,mb,nb,k)
#'#[1] 0.4742565 0.4823949 0.7745995 0.7745995
#'@export
simZHFLL1<-function(ma,na,mb,nb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-(2^(1-max(abs(ma[i,j]^2-mb[k,j]^2),abs(na[i,j]^2-nb[k,j]^2))))-1
  }
  for(j in 1:ncol(c)){
    sum<-(1/ncol(c))*rowSums(c)
  }
  sum
}
