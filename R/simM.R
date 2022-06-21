#'@title IFS similarity measure simM
#'@description IFS similarity measure values using simM computation technique with membership,non-membership, and hesitancy values of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param p Lp norm values for measuring the p-norm distance between x and y, values range from 1 to 5
#'@param k A constant value, considered as 1
#'@return The IFS similarity values of data set y with data set x
#'@references H. B. Mitchell. On the dengfeng–chuntian similarity measure and its application to pattern recognition. Pattern Recognition Letters, 24(16):3101 - 3104, 2003.
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
#'simM(ma,na,mb,nb,p,k)
#'#[1] 0.3840287 0.3837673 0.3849959 0.3849959
#'@export
simM<-function(ma,na,mb,nb,p,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  d<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)){
    for(j in 1:ncol(ma)){
      c[i,j]<-(abs(ma[i,j]-na[i,j]))^p
      d[i,j]<-(abs(mb[k,j]-nb[k,j]))^p
    }
  }
  for(j in 1:ncol(c)){
    pm<-1-((rowSums(c)/ncol(c)))^(1/p)
    pn<-1-((rowSums(d)/ncol(d)))^(1/p)
    sum<-(pm+pn)/2
  }
  sum
}
