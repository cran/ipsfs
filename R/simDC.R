#'@title IFS similarity measure simDC
#'@description IFS similarity measure values using simDC computation technique with membership,non-membership, and hesitancy values of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param ha IFS hesitancy values for the data set x
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param hb IFS hesitancy values for the data set y
#'@param p Lp norm values for measuring the p-norm distance between x and y, values range from 1 to 5
#'@param k A constant value, considered as 1
#'@return The IFS similarity values of data set y with data set x
#'@references L. Dengfeng and C. Chuntian. New similarity measures of intuitionistic fuzzy sets and application to pattern recognitions. Pattern recognition letters, 23(1-3):221 - 225, 2002.
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
#'ha<-hmemIFS(ma,na)
#'mb<-memG(a1,b1,y)
#'nb<-nonmemS(mb,lam)
#'hb<-hmemIFS(mb,nb)
#'p<-2
#'k<-1
#'simDC(ma,na,mb,nb,ha,hb,p,k)
#'#[1] 0.3553975 0.3558802 0.5378438 0.5378438
#'@export
simDC<-function(ma,na,mb,nb,ha,hb,p,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)){
    for(j in 1:ncol(ma))
      c[i,j]<-abs(((ma[i,j]+1-na[i,j])/2)-((mb[k,j]+1-nb[k,j])/2)^p)
  }
  for(j in 1:ncol(c)){
    sum<-1-((rowSums(c)/ncol(c))^(1/p))
  }
  sum
}
