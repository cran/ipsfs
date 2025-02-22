#'@title IFS similarity measure simHY3
#'@description IFS similarity measure values using simHY3 computation technique with membership, and non-membership values of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param k A constant value, considered as 1
#'@return The IFS similarity values of data set y with data set x
#'@references W.-L. Hung and M.-S. Yang. On similarity measures between intuitionistic fuzzy sets. International journal of intelligent systems, 23(3):364 - 383, 2008.
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
#'simHY3(ma,na,mb,nb,k)
#'#[1] 0.5460424 0.5468474 0.8109329 0.8109329
#'@export
simHY3<-function(ma,na,mb,nb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  d<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma)) {
      c[i,j]<-(min(ma[i,j],mb[k,j])+min(na[i,j],nb[k,j]))
      d[i,j]<-(max(ma[i,j],mb[k,j])+max(na[i,j],nb[k,j]))
    }
  }
  for(j in 1:ncol(c)){
    sum<-rowSums(c)/rowSums(d)
  }
  sum
}
