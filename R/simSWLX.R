#'@title IFS similarity measure simSWLX
#'@description IFS similarity measure values using simSWLX computation technique with membership,non-membership, and hesitancy values of two objects or set of objects.
#'@param ma IFS membership values for the data set x computed using either triangular or trapezoidal or guassian membership function
#'@param na IFS non-membership values for the data set x computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param ha IFS hesitancy values for the data set x
#'@param mb IFS membership values for the data set y computed using either triangular or trapezoidal or guassian membership function
#'@param nb IFS non-membership values for the data set y computed using either Sugeno and Terano's  or Yager's non-membership function
#'@param hb IFS hesitancy values for the data set y
#'@param k A constant value, considered as 1
#'@return The IFS similarity values of data set y with data set x
#'@references Y. Song, X. Wang, L. Lei, and A. Xue. A new similarity measure between intuitionistic fuzzy sets and its application to pattern recognition. In Abstract and Applied Analysis, volume 2014. Hindawi, 2014.
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
#'k<-1
#'simSWLX(ma,na,mb,nb,ha,hb,k)
#'#[1] 0.9241207 0.9180258 0.9853267 0.9853267
#'@export
simSWLX<-function(ma,na,mb,nb,ha,hb,k){
  c<-matrix(0,nrow=nrow(ma),ncol=ncol(ma))
  for (i in 1:nrow(ma)) {
    for(j in 1:ncol(ma))
      c[i,j]<-sqrt(ma[i,j]*mb[k,j])+2*(sqrt(na[i,j]*nb[k,j]))+sqrt(ha[i,j]*hb[k,j])+sqrt((1-na[i,j])*(1-nb[k,j]))
  }
  for(j in 1:ncol(c)){
    sum<-(1/(2*ncol(c)))*rowSums(c)
  }
  sum
}
